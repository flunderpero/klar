/**
 * This is not the cleanest, most elegant, or most efficient
 * implementation of a type checker.
 *
 * We basically explore how to write a type checker so we can do a
 * better job in the next iteration.
 */
import * as ast from "./parser"
import {lexer} from "./lexer"
import {TokenStream, parse} from "./parser"
import assert from "assert"
import {Span, quote} from "./common"

export function type_check_ast(ast: ast.AST, env: TypeEnvironment): Type {
    let type: Type = Type.unit
    type_check_declarations_and_definitions(ast, env)
    const ctx = {used_in_expression: false}
    for (let i = 0; i < ast.body.length; i++) {
        type = type_check(ast.body[i], env, {...ctx, used_in_expression: i === ast.body.length - 1})
    }
    return type
}

type Context = {
    used_in_expression: boolean
    return_type?: Type
    parent_is_function_call?: boolean
    inside_loop?: boolean
}

function type_check(node: ast.ASTNode, env: TypeEnvironment, ctx: Context): Type {
    let type: Type
    if (node instanceof ast.Number_) {
        type = env.i32
    } else if (node instanceof ast.Bool) {
        type = env.bool
    } else if (node instanceof ast.Str) {
        type = env.str
    } else if (node instanceof ast.InterpolatedStr) {
        for (const e of node.expressions) {
            const expression_type = type_check(e, env, {...ctx, used_in_expression: true})
            expect_to_implement_trait(expression_type, "ToStr", env, e.span)
        }
        type = env.str
    } else if (node instanceof ast.ArrayLiteral) {
        type = type_check_array_literal(node, env, ctx)
    } else if (node instanceof ast.UnitLiteral) {
        type = env.unit
    } else if (node instanceof ast.Not) {
        expect_equal_types(
            env.bool,
            type_check(node.expression, env, {...ctx, used_in_expression: true}),
            node.span,
        )
        type = env.bool
    } else if (node instanceof ast.UnitOperator) {
        type_check(node.expression, env, {...ctx, used_in_expression: false})
        type = env.unit
    } else if (node instanceof ast.If) {
        const condition_type = type_check(node.condition, env, {...ctx, used_in_expression: true})
        expect_equal_types(env.bool, condition_type, node.span)
        const then_type = type_check(node.then_block, env, {
            ...ctx,
            used_in_expression: !!node.else_block,
        })
        if (node.else_block) {
            const else_type = type_check(node.else_block, env, {...ctx, used_in_expression: false})
            if (ctx.used_in_expression) {
                expect_equal_types(then_type, else_type, node.span)
            }
        }
        type = ctx.used_in_expression ? then_type : Type.unit
    } else if (node instanceof ast.Block) {
        const block_env = new TypeEnvironment(env)
        type_check_declarations_and_definitions(node, block_env)
        let block_type: Type = Type.unit
        for (let i = 0; i < node.body.length; i++) {
            block_type = type_check(node.body[i], block_env, {
                ...ctx,
                // The last expression in a block is used as the block's type.
                used_in_expression: ctx.used_in_expression && i === node.body.length - 1,
            })
        }
        type = block_type
    } else if (node instanceof ast.VariableDeclaration) {
        type_check_variable_declaration(node, env, ctx)
        type = Type.unit
    } else if (node instanceof ast.FieldAccess) {
        const target_type = expect_type_with_fields(
            type_check(node.target, env, {...ctx, used_in_expression: false}),
            node.span,
        )
        type = target_type.field(node.field, node.span)
        if (type instanceof EnumType && type.is_variant) {
            if (type.variant_tuple_type!.fields.size > 0 && !ctx.parent_is_function_call) {
                throw new TypeCheckError(
                    `Enum variant ${quote(type.signature)} must be instantiated`,
                    node.span,
                )
            }
        }
        // Remember the target for later use.
        node.attributes.target_type = target_type
    } else if (node instanceof ast.IndexedAccess) {
        const index_type = type_check(node.index, env, {...ctx, used_in_expression: true})
        const target_type = expect_type_with_fields(
            type_check(node.target, env, {...ctx, used_in_expression: true}),
            node.span,
        )
        if (node.is_write) {
            expect_to_implement_trait(target_type, "IndexedSet", env, node.span)
            const set_function_type = expect_function_type(
                target_type.field("set", node.span),
                node.span,
            )
            expect_assignable_to(set_function_type.parameters[1], index_type, node.span)
            type = set_function_type.parameters[2]
        } else {
            expect_to_implement_trait(target_type, "IndexedGet", env, node.span)
            const get_function_type = expect_function_type(
                target_type.field("get", node.span),
                node.span,
            )
            expect_assignable_to(get_function_type.parameters[1], index_type, node.span)
            type = get_function_type.return_type
        }
    } else if (node instanceof ast.Return) {
        if (node.value) {
            if (!ctx.return_type) {
                throw new SemanticError("Unexpected return", node.span)
            }
            const return_type = type_check(node.value, env, {...ctx, used_in_expression: true})
            type_check_return_type(ctx.return_type, return_type, node.span)
            // `return` is a statement, but we give it the type because it makes it easier
            // to type-check nested blocks like:
            //     fn foo(): i32
            //         if bar => return 1 else => return 2
            //     end
            type = ctx.return_type
        } else {
            type = Type.unit
        }
    } else if (node instanceof ast.BinaryExpression) {
        type = type_check_binary_expression(node, env, ctx)
    } else if (node instanceof ast.FunctionDefinition) {
        const function_type = FunctionType.from_declaration(node.declaration, env)
        type_check_function_body(node, function_type, env)
        type = Type.unit
    } else if (node instanceof ast.ClosureDefinition) {
        const function_type = FunctionType.from_declaration(node, env)
        type_check_function_body(node, function_type, env)
        type = function_type
    } else if (node instanceof ast.Assignment) {
        const target_type = type_check(node.target, env, {...ctx, used_in_expression: true})
        const value_type = type_check(node.value, env, {...ctx, used_in_expression: true})
        if (
            node.value instanceof ast.FieldAccess ||
            node.value instanceof ast.IdentifierReference
        ) {
            // Prevent use cases like: `a = Foo` where `Foo` is an enum or struct type.
            expect_type_can_be_used_in_assignments(value_type, env, node.span)
        }
        expect_assignable_to(target_type, value_type, node.span)
        // Assignments are statements.
        type = Type.unit
    } else if (node instanceof ast.FunctionCall) {
        type = type_check_function_call_or_enum_instantiation(node, env, ctx)
    } else if (node instanceof ast.StructInstantiation) {
        type = struct_instantiation(node, env)
    } else if (node instanceof ast.TupleInstantiation) {
        const types = node.elements.map((v) =>
            type_check(v, env, {...ctx, used_in_expression: true}),
        )
        type = TupleType.from_types(types)
    } else if (node instanceof ast.ParenthesizedExpression) {
        type = type_check(node.expression, env, ctx)
    } else if (node instanceof ast.IdentifierReference) {
        type = env.get(node.name, node.span)
        if (type instanceof ComplexType && node.type_parameters.length > 0) {
            const type_parameters = parse_type_arguments(type, node.type_parameters, env, node.span)
            type = type.with_type_arguments(type_parameters, node.span).with_scope_type_variables(
                TypeVariable.create_type_variable_map(
                    type.type_variables,
                    TypeParameters.from_declaration(node.type_parameters, env).type_parameters.map(
                        (x) => x.type,
                    ),
                ),
            )
        }
    } else if (node instanceof ast.Loop) {
        type_check(node.block, env, {...ctx, used_in_expression: false, inside_loop: true})
        type = Type.unit
    } else if (node instanceof ast.Break || node instanceof ast.Continue) {
        if (!ctx.inside_loop) {
            throw new SemanticError(
                `${quote(
                    node instanceof ast.Break ? "break" : "continue",
                )} can only be used inside a loop`,
                node.span,
            )
        }
        type = Type.unit
    } else if (node instanceof ast.DeclarationOrDefinition) {
        type = Type.unit // We already forward parsed them.
    } else if (node instanceof ast.Match) {
        type = type_check_match(node, env, ctx)
    } else {
        throw new TypeCheckError(
            `Not implemented for node type ${quote(node.constructor.name)}`,
            node.span,
        )
    }
    node.attributes.type = type
    return type
}

function type_check_return_type(expected_return_type: Type, return_type: Type, span: Span) {
    if (expected_return_type.name === "Result" && return_type.name !== "Result") {
        const result_type = expect_type_with_fields(expected_return_type, span)
        if (return_type.name === "Error") {
            return
        } else {
            expect_assignable_to(
                result_type.type_arguments.get(result_type.type_variables[0])!,
                return_type,
                span,
            )
        }
    } else {
        expect_assignable_to(expected_return_type, return_type, span)
    }
}

function expect_equal_types(expected: Type, got: Type, span: Span) {
    if (got === Type.internal_any || expected === Type.internal_any) {
        // This is a special handling for the `panic` function.
        return
    }
    if (!expected.equals(got)) {
        throw new TypeMismatchError(expected, got, span)
    }
}

function expect_function_type(type: Type, span: Span): FunctionType {
    if (!(type instanceof FunctionType)) {
        throw new TypeCheckError(`Expected function type but got ${quote(type.signature)}`, span)
    }
    return type
}

function expect_type_with_fields(type: Type, span: Span): ComplexType<any> {
    if (!(type instanceof ComplexType) || type instanceof FunctionType) {
        throw new TypeCheckError(
            `Expected a type with fields but got ${quote(type.signature)}`,
            span,
        )
    }
    return type
}

function expect_to_implement_trait(
    type: Type,
    trait_name: string,
    env: TypeEnvironment,
    span: Span,
) {
    if (!(type instanceof ComplexType)) {
        throw new TypeCheckError(`Expected complex type but got ${quote(type.signature)}`, span)
    }
    const trait_type = TraitType.from_env(trait_name, env, span)
    if (!type.traits.includes(trait_name)) {
        throw new TypeCheckError(
            `Expected ${quote(type.signature)} to implement ${quote(trait_type.signature)}`,
            span,
        )
    }
}

function expect_assignable_to(expected: Type, got: Type, span: Span) {
    if (got === Type.internal_any || expected === Type.internal_any) {
        // This is a special handling for the `panic` function.
        return
    }
    if (got.assignable_to(expected)) {
        return
    }
    throw new TypeMismatchError(expected, got, span)
}

function expect_type_can_be_used_in_assignments(type: Type, env: TypeEnvironment, span: Span) {
    // Make sure all type variables are resolved.
    if (type instanceof ComplexType) {
        const unresolved_type_variables = type
            .unresolved_type_variables()
            .filter((v) => env.get_or_null(v.name) === null)
        if (unresolved_type_variables.length > 0) {
            throw new TypeCheckError(
                `Cannot assign a type with unresolved type variables ${quote(
                    unresolved_type_variables.map((v) => v.signature).join(","),
                )} to a variable`,
                span,
            )
        }
        if (type instanceof EnumType) {
            if (!type.is_variant) {
                throw new TypeCheckError(`Cannot assign enum type ${quote(type.signature)}`, span)
            }
        }
    }
}

function type_check_array_literal(node: ast.ArrayLiteral, env: TypeEnvironment, ctx: Context) {
    if (node.elements.length === 0) {
        throw new TypeCheckError("Array literal must have at least one element", node.span)
    }
    const element_type = type_check(node.elements[0], env, {...ctx, used_in_expression: true})
    for (let i = 1; i < node.elements.length; i++) {
        const element = node.elements[i]
        const element_type2 = type_check(element, env, {...ctx, used_in_expression: true})
        expect_assignable_to(element_type, element_type2, element.span)
    }
    const array_type = env.get("Array", node.span) as ComplexType<any>
    return array_type.with_type_arguments([element_type], node.span)
}

function type_check_match(node: ast.Match, env: TypeEnvironment, ctx: Context): Type {
    if (node.arms.length === 0) {
        throw new TypeCheckError("Match expression has no arms", node.span)
    }
    const target_type = type_check(node.value, env, {...ctx, used_in_expression: true})
    const arm_types: Type[] = []
    for (const arm of node.arms) {
        arm_types.push(type_check_match_arm(arm, target_type, env, ctx))
    }
    if (ctx.used_in_expression) {
        let type = arm_types[0]!
        for (let i = 1; i < arm_types.length; i++) {
            expect_equal_types(type, arm_types[i], node.span)
            if (type === Type.internal_any) {
                type = arm_types[i]
            }
        }
        return type
    }
    return Type.unit
}

function type_check_match_arm(
    node: ast.MatchArm,
    target_type: Type,
    env: TypeEnvironment,
    ctx: Context,
): Type {
    const pattern_env = new TypeEnvironment(env)
    type_check_match_pattern(node.pattern, target_type, pattern_env, ctx)
    return type_check(node.block, pattern_env, ctx)
}

function type_check_match_pattern(
    pattern: ast.MatchPattern,
    target_type: Type,
    env: TypeEnvironment,
    ctx: Context,
): Type {
    if (pattern instanceof ast.LiteralMatchPattern) {
        if (typeof pattern.value === "number") {
            if (!env.i32.assignable_to(target_type)) {
                // Note how the order in the exception is reversed.
                // We did not expect to find a number but `target_type`.
                // This logic differs from `expect_assignable_to`.
                throw new TypeMismatchError(target_type, env.i32, pattern.span)
            }
            expect_assignable_to(env.i32, target_type, pattern.span)
            return env.i32
        } else if (typeof pattern.value === "boolean") {
            if (!env.bool.assignable_to(target_type)) {
                // Note how the order in the exception is reversed.
                // We did not expect to find a number but `target_type`.
                // This logic differs from `expect_assignable_to`.
                throw new TypeMismatchError(target_type, env.bool, pattern.span)
            }
            return env.bool
        } else {
            throw new TypeCheckError(
                `Not implemented for literal type ${quote(pattern.value)}`,
                pattern.span,
            )
        }
    } else if (pattern instanceof ast.CaptureMatchPatternOrType) {
        // This might be a capture or a type.
        const type = env.get_or_null(pattern.name)
        if (type && type.name === pattern.name) {
            // The name can only match the type if it is a type.
            if (type.has_type_parameters()) {
                throw new TypeCheckError(
                    `Cannot match on generic type ${quote(type.signature)}`,
                    pattern.span,
                )
            }
        }
        env.add(pattern.name, target_type, pattern.span)
        return target_type
    } else if (pattern instanceof ast.StructuredMatchPattern) {
        const pattern_type = type_check(pattern.type_expression, env, ctx)
        if (pattern_type instanceof EnumType) {
            if (!pattern_type.is_variant) {
                throw new TypeCheckError(
                    `Cannot match on enum type ${quote(pattern_type.signature)}, only on variants`,
                    pattern.span,
                )
            }
            if (pattern_type.variant_tuple_type!.fields.size === 0) {
                return target_type
            }
        }
        if (target_type instanceof TypeVariable) {
            if (Object.keys(pattern.fields).length > 0) {
                throw new TypeCheckError(
                    `Cannot match on type variable ${quote(target_type.signature)}`,
                    pattern.span,
                )
            }
            return target_type
        }
        const struct_type = expect_type_with_fields(target_type, pattern.span)
        if (Object.keys(pattern.fields).length !== struct_type.fields.size) {
            throw new TypeCheckError(
                `Expected ${struct_type.fields.size} fields but got ${
                    Object.keys(pattern.fields).length
                }`,
                pattern.span,
            )
        }
        const pattern_type_with_fields = expect_type_with_fields(pattern_type, pattern.span)
        for (const [name, sub_pattern] of Object.entries(pattern.fields)) {
            const field_type = pattern_type_with_fields.field(name, sub_pattern.span)
            const sub_pattern_type = type_check_match_pattern(sub_pattern, field_type, env, ctx)
            expect_assignable_to(field_type, sub_pattern_type, sub_pattern.span)
        }
        return struct_type
    } else if (pattern instanceof ast.TupleMatchPattern) {
        let pattern_type = type_check(pattern.type_expression, env, {
            ...ctx,
            parent_is_function_call: true,
        })
        expect_assignable_to(target_type, pattern_type, pattern.span)
        let tuple_type = pattern_type
        if (target_type instanceof EnumType) {
            assert(pattern_type instanceof EnumType)
            if (!pattern_type.is_variant) {
                throw new TypeCheckError(
                    `Cannot match on enum type ${quote(target_type.signature)}, only on variants`,
                    pattern.span,
                )
            }
            if (pattern_type.variant_tuple_type!.fields.size === 0) {
                return pattern_type
            }
            tuple_type = pattern_type.variant_tuple_type!
        }
        if (!(tuple_type instanceof TupleType)) {
            throw new TypeCheckError(
                `Expected tuple type but got ${quote(target_type.signature)}`,
                pattern.span,
            )
        }
        if (tuple_type.fields.size !== pattern.values.length) {
            throw new TypeCheckError(
                `Expected ${tuple_type.fields.size} elements but got ${pattern.values.length}`,
                pattern.span,
            )
        }
        for (let i = 0; i < pattern.values.length; i++) {
            const sub_pattern = pattern.values[i]
            const field_type = tuple_type.field(`${i}`, sub_pattern.span)
            const sub_pattern_type = type_check_match_pattern(sub_pattern, field_type, env, ctx)
            expect_assignable_to(sub_pattern_type, field_type, sub_pattern.span)
        }
        return target_type
    } else if (pattern instanceof ast.WildcardMatchPattern) {
        return target_type
    } else {
        throw new TypeCheckError(
            `Not implemented for pattern type ${quote(pattern.constructor.name)}`,
            pattern.span,
        )
    }
}

function type_check_variable_declaration(
    node: ast.VariableDeclaration,
    env: TypeEnvironment,
    ctx: Context,
) {
    const value_type = type_check(node.value, env, {...ctx, used_in_expression: true})
    if (value_type.equals(Type.unit)) {
        throw new TypeCheckError("Cannot assign the unit value to a variable", node.span)
    }
    if (node.type) {
        let declared_type = env.get(node.type.name, node.span)
        if (declared_type instanceof ComplexType) {
            declared_type = declared_type.with_type_arguments(
                parse_type_arguments(declared_type, node.type.type_parameters, env, node.span),
                node.span,
            )
        }
        expect_assignable_to(declared_type, value_type, node.span)
    }
    if (node.value instanceof ast.FieldAccess || node.value instanceof ast.IdentifierReference) {
        // Prevent use cases like: `let a = Foo` where `Foo` is an enum or struct type.
        expect_type_can_be_used_in_assignments(value_type, env, node.span)
    }
    env.add(node.name, value_type, node.span)
}

function type_check_binary_expression(
    node: ast.BinaryExpression,
    env: TypeEnvironment,
    ctx: Context,
): Type {
    const trait_based: Record<string, [string, string]> = {
        "+": ["Add", "add"],
        "-": ["Sub", "sub"],
        "*": ["Mul", "mul"],
        "/": ["Div", "div"],
        "==": ["PartialEq", "eq"],
        "!=": ["PartialEq", "ne"],
        "<": ["PartialOrd", "lt"],
        "<=": ["PartialOrd", "le"],
        ">": ["PartialOrd", "gt"],
        ">=": ["PartialOrd", "ge"],
    }
    const lhs = type_check(node.lhs, env, {...ctx, used_in_expression: true})
    const rhs = type_check(node.rhs, env, {...ctx, used_in_expression: true})
    if (["and", "or"].includes(node.operator)) {
        expect_equal_types(env.bool, lhs, node.span)
        expect_equal_types(env.bool, rhs, node.span)
        return env.bool
    }
    if (!(node.operator in trait_based)) {
        throw new TypeCheckError(
            `Not implemented for binary operator ${quote(node.operator)}`,
            node.span,
        )
    }
    const [trait_name, function_name] = trait_based[node.operator]
    if (lhs instanceof TypeVariable) {
        // TODO: We need trait bounds.
        expect_equal_types(lhs, rhs, node.span)
        const trait_type = TraitType.from_env(trait_name, env, node.span)
        const function_type = expect_function_type(
            trait_type.field(function_name, node.span),
            node.span,
        )
        return function_type.return_type
    }
    if (!(lhs instanceof ComplexType) && !(lhs instanceof FunctionType)) {
        throw new TypeCheckError(
            `Expected complex type that is not a function but got ${quote(lhs.signature)}`,
            node.span,
        )
    }
    expect_to_implement_trait(lhs, trait_name, env, node.span)
    const function_type = expect_function_type(lhs.field(function_name, node.span), node.span)
    const parameter = function_type.parameters_without_self[0]
    expect_assignable_to(parameter, rhs, node.span)
    return function_type.return_type
}

function type_check_function_call_or_enum_instantiation(
    node: ast.FunctionCall,
    env: TypeEnvironment,
    ctx: Context,
): Type {
    const target = type_check(node.target, env, {
        ...ctx,
        used_in_expression: true,
        parent_is_function_call: true,
    })
    if (target instanceof EnumType) {
        if (!target.is_variant) {
            throw new TypeCheckError(
                `Cannot call the enum type ${quote(target.signature)}`,
                node.span,
            )
        }
        const variant_type = target.variant_tuple_type!
        if (variant_type.fields.size === 0) {
            throw new TypeCheckError(`Enum variant ${quote(target.name)} has no fields`, node.span)
        }
        if (variant_type.fields.size !== node.args.length) {
            throw new TypeCheckError(
                `Expected ${target.fields.size} arguments but got ${node.args.length}`,
                node.span,
            )
        }
        for (let i = 0; i < node.args.length; i++) {
            const arg_type = type_check(node.args[i], env, {used_in_expression: true})
            expect_assignable_to(variant_type.fields.get(`${i}`)!, arg_type, node.span)
        }
        return target
    }
    let function_type = expect_function_type(target, node.span)
    let parameters = function_type.parameters_without_self
    if (parameters.length !== node.args.length) {
        throw new TypeCheckError(
            `Expected ${function_type.parameters.length} arguments but got ${node.args.length}`,
            node.span,
        )
    }
    const type_arguments = parse_type_arguments(function_type, node.type_arguments, env, node.span)
    function_type = function_type.with_type_arguments(type_arguments, node.span)
    parameters = function_type.parameters_without_self
    for (let i = 0; i < node.args.length; i++) {
        const arg_type = type_check(node.args[i], env, {used_in_expression: true})
        expect_assignable_to(parameters[i], arg_type, node.span)
    }
    if (function_type.name === "panic") {
        // `panic` is a special function that never returns.
        return Type.internal_any
    }
    const return_type = function_type.return_type
    if (node.propagate_error) {
        if (return_type.name !== "Result") {
            throw new TypeCheckError(
                `Cannot propagate error from function that does not return a Result type`,
                node.span,
            )
        }
        assert(return_type instanceof ComplexType)
        return return_type.type_arguments.get(return_type.type_variables[0])!
    }
    return return_type
}

function struct_instantiation(node: ast.StructInstantiation, env: TypeEnvironment): StructType {
    let struct_type = StructType.from_env(node.target_struct_name, env, node.span)
    const type_arguments = parse_type_arguments(struct_type, node.type_arguments, env, node.span)
    struct_type = struct_type.with_type_arguments(type_arguments, node.span)
    for (const [name, value] of Object.entries(node.fields)) {
        const field_type = struct_type.field(name, node.span)
        const value_type = type_check(value, env, {used_in_expression: true})
        expect_assignable_to(field_type, value_type, node.span)
    }
    return struct_type
}

function parse_type_arguments(
    type: ComplexType<any>,
    type_arguments: ast.TypeDeclaration[],
    env: TypeEnvironment,
    span: Span,
    opts?: {ignore_missing: boolean},
) {
    const result: (Type | null)[] = []
    for (const p of type_arguments) {
        const type_argument = TypeParameter.from_declaration(p, env)
        let type_argument_type = type_argument.type
        if (!opts?.ignore_missing && !(type_argument.type instanceof TupleType)) {
            env.get(type_argument.type.name, span)
        }
        if (type_argument_type instanceof TypeVariable) {
            result.push(null)
        } else {
            if (type_argument_type instanceof ComplexType) {
                type_argument_type = type_argument_type.with_type_arguments(
                    parse_type_arguments(type_argument_type, p.type_parameters, env, span),
                    span,
                )
            }
            result.push(type_argument_type)
        }
    }
    // Add trailing default types.
    for (let i = result.length; i < type.type_variables.length; i++) {
        const type_variable = type.type_variables[i]
        if (type_variable.default_type) {
            result.push(type_variable.default_type)
        }
    }
    return result
}

function type_check_declarations_and_definitions(block: ast.Block, env: TypeEnvironment) {
    // First we add all types to the environment without parsing their bodies.
    // This is sufficient for type checking and for resolving recursive and forward type definitions.
    const nodes = block.body.filter((x) => x instanceof ast.DeclarationOrDefinition)
    const struct_declarations: ast.StructDeclaration[] = []
    const enum_declarations: ast.EnumDeclaration[] = []
    const function_declarations: ast.FunctionDeclaration[] = []
    const trait_declarations: ast.TraitDeclaration[] = []
    const impl_definitions: {node: ast.ImplDefinition; extern: boolean}[] = []
    function forward_declare(node: ast.DeclarationOrDefinition, extern: boolean) {
        if (node instanceof ast.StructDeclaration) {
            const struct_type = StructType.from_declaration(node, env)
            env.add(struct_type.name, struct_type, node.span)
            struct_declarations.push(node)
        } else if (node instanceof ast.EnumDeclaration) {
            const enum_type = EnumType.from_declaration(node, env)
            env.add(enum_type.name, enum_type, node.span)
            enum_declarations.push(node)
        } else if (node instanceof ast.FunctionDefinition) {
            const function_type = FunctionType.from_declaration(node.declaration, env)
            env.add(function_type.name, function_type, node.span)
            function_declarations.push(node.declaration)
        } else if (node instanceof ast.FunctionDeclaration) {
            const function_type = FunctionType.from_declaration(node, env)
            env.add(function_type.name, function_type, node.span)
            function_declarations.push(node)
        } else if (node instanceof ast.TraitDeclaration) {
            const trait_type = TraitType.from_declaration(node, env)
            env.add(trait_type.name, trait_type, node.span)
            trait_declarations.push(node)
        } else if (node instanceof ast.VariableDeclaration) {
            // Variable declarations are not forward declared.
        } else if (node instanceof ast.ImplDefinition) {
            // Will be evaluated next.
            impl_definitions.push({node, extern})
        } else if (node instanceof ast.ExternBlock) {
            // Forward declare functions last because they might depend on other types.
            for (const extern_node of node
                .contained_nodes()
                .filter(
                    (x) =>
                        !(
                            x instanceof ast.FunctionDeclaration ||
                            x instanceof ast.FunctionDefinition
                        ),
                )) {
                forward_declare(extern_node, true)
            }
            for (const extern_node of node
                .contained_nodes()
                .filter(
                    (x) =>
                        x instanceof ast.FunctionDeclaration || x instanceof ast.FunctionDefinition,
                )) {
                forward_declare(extern_node, true)
            }
        } else {
            throw new TypeCheckError(
                `Not implemented for node type ${quote(node.constructor.name)}`,
                node.span,
            )
        }
    }

    // Forward declare functions last because they might depend on other types.
    for (const node of nodes.filter(
        (x) => !(x instanceof ast.FunctionDeclaration || x instanceof ast.FunctionDefinition),
    )) {
        forward_declare(node, false)
    }
    for (const node of nodes.filter(
        (x) => x instanceof ast.FunctionDeclaration || x instanceof ast.FunctionDefinition,
    )) {
        forward_declare(node, false)
    }

    for (const node of struct_declarations) {
        type_check_struct_fields(node, env)
    }

    for (const node of enum_declarations) {
        type_check_enum_variants(node, env)
    }

    for (const node of trait_declarations) {
        type_check_trait_functions(node, env, "signatures")
    }

    for (const impl of impl_definitions) {
        type_check_impl(impl.node, env, {extern: impl.extern}, "signatures")
    }

    for (const node of trait_declarations) {
        type_check_trait_functions(node, env, "default_impls")
    }

    for (const impl of impl_definitions) {
        type_check_impl(impl.node, env, {extern: impl.extern}, "impls")
    }
}

function type_check_struct_fields(node: ast.StructDeclaration, env: TypeEnvironment) {
    const struct_type = StructType.from_env(node.name, env, node.span)
    const struct_env = struct_type.create_type_environment(env, node.span)
    for (const [name, type] of Object.entries(node.fields)) {
        let field_type = Type.from_env_or_declaration(type, struct_env)
        const type_parameters = TypeParameters.from_declaration(type.type_parameters, struct_env)
        struct_type.add_field(name, field_type, type_parameters, type.span)
    }
}

function type_check_enum_variants(node: ast.EnumDeclaration, env: TypeEnvironment) {
    const enum_type = EnumType.from_env(node.name, env, node.span)
    const enum_env = enum_type.create_type_environment(env, node.span)
    for (const variant_declaration of node.variants) {
        const variant = EnumType.variant_from_declaration(enum_type, variant_declaration, enum_env)
        enum_type.add_field_immediate(variant_declaration.name, variant)
    }
}

function type_check_function_body(
    node: ast.FunctionDefinition | ast.ClosureDefinition,
    function_type: FunctionType,
    env: TypeEnvironment,
) {
    const function_env = new TypeEnvironment(env)
    for (const name of function_type.parameter_names) {
        const type = function_type.parameter(name, node.span)
        function_env.add(name, type, node.span)
    }
    const return_type = function_type.return_type
    const block_type = type_check(node.block, function_env, {
        return_type,
        used_in_expression: true,
    })
    if (node.block.body.at(-1) instanceof ast.Return) {
        // The last expression is a return statement, so we don't need to check the return type.
        // We already checked that the return type is correct in the return statement.
    } else {
        type_check_return_type(return_type, block_type, node.span)
    }
}

function type_check_trait_functions(
    node: ast.TraitDeclaration,
    env: TypeEnvironment,
    mode: "signatures" | "default_impls",
) {
    let trait_type = TraitType.from_env(node.name, env, node.span)
    let trait_env: TypeEnvironment
    if (mode === "default_impls") {
        // We need to use the actual `Self` type to check the default implementations.
        trait_type = trait_type.copy_without_fields()
        trait_env = trait_type.create_type_environment(env, node.span)
        trait_env.add("Self", trait_type, node.span)
    } else {
        trait_env = trait_type.create_type_environment(env, node.span)
        trait_env.add("Self", TypeVariable.Self, node.span)
    }
    for (const func of node.functions) {
        const declaration = func instanceof ast.FunctionDeclaration ? func : func.declaration
        const function_type = FunctionType.from_declaration(declaration, trait_env)
        if (trait_type.fields.has(function_type.name)) {
            throw new TypeCheckError(
                `Field ${quote(function_type.name)} already defined in trait ${quote(
                    trait_type.signature,
                )}`,
                func.span,
            )
        }
        trait_type.add_field_immediate(function_type.name, function_type)
    }
    if (mode === "signatures") {
        return
    }
    const functions_with_default_impl = node.functions.filter(
        (f) => !(f instanceof ast.FunctionDeclaration),
    ) as ast.FunctionDefinition[]
    if (functions_with_default_impl.length === 0) {
        return
    }
    for (const func of functions_with_default_impl) {
        const function_type = trait_type.fields.get(func.name)!
        assert(function_type instanceof FunctionType)
        type_check_function_body(func, function_type, trait_env)
    }
}

function type_check_impl(
    node: ast.ImplDefinition,
    env: TypeEnvironment,
    opts: {extern: boolean},
    mode: "signatures" | "impls",
) {
    const complex_type = ComplexType.from_env(node.target_name, env, node.span)
    if (complex_type instanceof TraitType || complex_type instanceof FunctionType) {
        throw new TypeCheckError(
            `Cannot implement a function or trait type ${quote(complex_type.signature)}`,
            node.span,
        )
    }
    // Add impl to the target type's attributes for later use.
    if (complex_type instanceof StructType || complex_type instanceof EnumType) {
        complex_type.data.declaration?.attributes.impls.push(node)
    }
    const impl_env = new TypeEnvironment(env)
    impl_env.add("Self", complex_type, node.span)
    const type_variables = node.type_parameters.map((p) =>
        TypeVariable.from_declaration(p, impl_env),
    )
    if (type_variables.length !== complex_type.type_variables.length) {
        throw new TypeCheckError(
            `Expected ${complex_type.type_variables.length} type parameters but got ${type_variables.length}`,
            node.span,
        )
    }
    for (let i = 0; i < type_variables.length; i++) {
        impl_env.add(type_variables[i].name, complex_type.type_variables[i], node.span)
    }
    if (mode === "signatures") {
        for (const func of node.functions) {
            const declaration = func instanceof ast.FunctionDeclaration ? func : func.declaration
            const function_type = FunctionType.from_declaration(declaration, impl_env)
            if (complex_type.fields.has(function_type.name)) {
                throw new TypeCheckError(
                    `Field ${quote(function_type.name)} already defined in struct ${quote(
                        complex_type.signature,
                    )}`,
                    func.span,
                )
            }
            const type_parameters = TypeParameters.from_declaration(
                declaration.type_parameters,
                impl_env,
            )
            complex_type.add_field(function_type.name, function_type, type_parameters, func.span)
        }
        if (node.trait_name) {
            complex_type.data.traits.push(node.trait_name)
            let trait_type = TraitType.from_env(node.trait_name, impl_env, node.span)
            const trait_type_arguments = parse_type_arguments(
                trait_type,
                node.trait_type_parameters,
                impl_env,
                node.span,
                {ignore_missing: true},
            )
            trait_type = trait_type.with_type_arguments(trait_type_arguments, node.span)
            const type_variable_map = new Map<TypeVariable, TypeVariable>()
            for (let i = 0; i < node.trait_type_parameters.length; i++) {
                const arg = TypeParameter.from_declaration(node.trait_type_parameters[i], impl_env)
                if (arg.type instanceof TypeVariable) {
                    const src = trait_type.type_variables[i]
                    type_variable_map.set(src, arg.type)
                }
            }
            type_variable_map.set(TypeVariable.Self, complex_type as any)
            // Add declaration for later user.
            node.attributes.trait_declaration = trait_type.data.declaration
            for (let [name, type] of trait_type.fields) {
                assert(type instanceof FunctionType)
                if (!complex_type.fields.has(name)) {
                    // See, if we have a default implementation.
                    const default_impl = trait_type.data.functions_with_default_impl.includes(name)
                    if (!default_impl && !opts.extern) {
                        throw new TypeCheckError(
                            `Trait function ${quote(name)} is not implemented in struct ${quote(
                                complex_type.signature,
                            )}`,
                            node.span,
                        )
                    }
                    // Add the default impl to the struct.
                    type = type.with_scope_type_variables(type_variable_map)
                    assert(type instanceof FunctionType)
                    type = complex_type.add_field_immediate(name, type)
                } else {
                    type = type.with_scope_type_variables(type_variable_map)
                }
                const struct_field_type = complex_type.field(name, node.span)
                expect_assignable_to(type, struct_field_type, node.span)
            }
        }
        return
    }
    const function_definitions = node.functions.filter(
        (f) => f instanceof ast.FunctionDefinition,
    ) as ast.FunctionDefinition[]
    const function_env = new TypeEnvironment(impl_env)
    for (const type of complex_type.type_variables) {
        function_env.add(type.name, type, node.span)
    }
    for (const func of function_definitions) {
        const function_type = complex_type.field(func.name, func.span)
        assert(function_type instanceof FunctionType)
        type_check_function_body(func, function_type, function_env)
    }
}

class SemanticError extends Error {
    constructor(
        message: string,
        public span: Span,
    ) {
        super(`${message} at ${span.toString()}`)
    }
}

class TypeCheckError extends SemanticError {}

class TypeMismatchError extends TypeCheckError {
    constructor(
        public expected: Type,
        public got: Type,
        public span: Span,
    ) {
        super(
            `Expected ${quote(expected.signature_and_kind)} but got ${quote(
                got.signature_and_kind,
            )}`,
            span,
        )
    }
}

export class Type {
    static unit = new Type("()")
    static internal_any = new Type("__internal_any__")

    /**
     * Some types like tuples are `inline defined` and don't have a name.
     * All other types are looked up in the environment.
     */
    static from_env_or_declaration(declaration: ast.TypeDeclaration, env: TypeEnvironment): Type {
        const type = Type.from_env_or_declaration_or_null(declaration, env)
        if (!type) {
            throw new TypeCheckError(
                `Unknown ${quote(declaration.name)} in type environment`,
                declaration.span,
            )
        }
        return type
    }

    static from_env_or_declaration_or_null(
        declaration: ast.TypeDeclaration,
        env: TypeEnvironment,
    ): Type | null {
        if (declaration instanceof ast.TupleTypeDeclaration) {
            const type_variables = TypeParameters.from_declaration(
                declaration.type_parameters,
                env,
            ).all_type_variables()
            const type = TupleType.from_declaration(
                declaration,
                env,
                type_variables,
                declaration.span,
            )
            return type
        } else if (declaration instanceof ast.UnitTypeDeclaration) {
            return env.unit
        }
        return env.get_or_null(declaration.name)
    }

    protected static known_types = new Map<string, Type>()
    protected static add_or_get_known_type<T extends Type>(name: string, type: T, span: Span): T {
        const known_type = Type.known_types.get(name)
        if (known_type) {
            if (known_type.constructor !== type.constructor) {
                throw new TypeCheckError(
                    `Type ${quote(name)} already defined as ${quote(
                        known_type.signature,
                    )} of type ${quote(type.constructor.name)}`,
                    span,
                )
            }
            return known_type as T
        }
        Type.known_types.set(name, type)
        return type
    }
    static clear_known_types() {
        Type.known_types.clear()
    }

    protected constructor(public name: string) {}

    get signature(): string {
        return this.name
    }

    get signature_and_kind(): string {
        return `${this.signature} (${this.constructor.name})`
    }

    get debug_str(): string {
        return this.signature_and_kind
    }

    resolve_type_variables(_map: Map<TypeVariable, Type>): Type {
        return this
    }

    equals(other: Type): boolean {
        return this === other
    }

    assignable_to(other: Type): boolean {
        return this.equals(other)
    }

    has_type_parameters(): boolean {
        return false
    }
}

class TypeVariable extends Type {
    static Self = new TypeVariable("Self", null)
    static from_declaration(declaration: ast.TypeDeclaration, env: TypeEnvironment): TypeVariable {
        let default_type: Type | null = null
        if (declaration.type_parameter_default) {
            default_type = Type.from_env_or_declaration(declaration.type_parameter_default, env)
        }
        const result = new TypeVariable(declaration.name, default_type)
        let type = env.get_or_null(result.signature)
        if (type) {
            if (!(type instanceof TypeVariable)) {
                throw new TypeCheckError(
                    `Expected type variable but got ${quote(type.signature)}`,
                    declaration.span,
                )
            }
            return type
        }
        return Type.add_or_get_known_type(
            result.signature,
            result,
            declaration.span,
        ) as TypeVariable
    }

    static create_type_variable_map(
        type_variables: TypeVariable[],
        type_arguments: (Type | null)[],
    ) {
        const map = new Map<TypeVariable, TypeVariable>()
        for (let i = 0; i < type_variables.length; i++) {
            const type_variable = type_variables[i]
            const type_argument = type_arguments[i]
            if (type_argument instanceof TypeVariable) {
                map.set(type_variable, type_argument)
            }
        }
        return map
    }

    private constructor(
        name: string,
        public default_type: Type | null,
    ) {
        super(name)
    }

    resolve_type_variables(map: Map<TypeVariable, Type>): Type {
        return map.get(this) ?? this
    }

    get signature(): string {
        if (this.default_type) {
            return `${this.name} default ${this.default_type.signature}`
        }
        return this.name
    }
}

export class TypeParameters {
    static from_declaration(
        type_parameters: ast.TypeDeclaration[],
        env: TypeEnvironment,
    ): TypeParameters {
        return new TypeParameters(
            type_parameters.map((p) => TypeParameter.from_declaration(p, env)),
        )
    }

    static from_type_variables(type_variables: TypeVariable[]): TypeParameters {
        return new TypeParameters(type_variables.map((v) => TypeParameter.from_type_variable(v)))
    }

    private constructor(public type_parameters: TypeParameter[]) {}

    all_type_variables(): TypeVariable[] {
        return this.type_parameters.flatMap((p) => p.type_variables())
    }
}

/**
 * Convert something like Num<V> into a type parameter.
 */
export class TypeParameter {
    static from_declaration(declaration: ast.TypeDeclaration, env: TypeEnvironment): TypeParameter {
        let type = Type.from_env_or_declaration_or_null(declaration, env)
        if (!type) {
            // The type is unknown, we treat this as a type variable.
            type = TypeVariable.from_declaration(declaration, env)
            return new TypeParameter(type, [])
        }
        const type_parameters = declaration.type_parameters.map((p) =>
            TypeParameter.from_declaration(p, env),
        )
        return new TypeParameter(type, type_parameters)
    }

    static from_type_variable(type_variable: TypeVariable): TypeParameter {
        return new TypeParameter(type_variable, [])
    }

    static to_str(
        type_parameters: (TypeParameter | TypeVariable)[],
        type_arguments: Map<TypeVariable, Type>,
    ): string {
        const args = []
        for (let i = 0; i < type_parameters.length; i++) {
            const type_parameter = type_parameters[i]
            if (type_parameter instanceof TypeVariable) {
                const type_argument = type_arguments.get(type_parameter)
                if (type_argument) {
                    args.push(`${type_parameter.signature}=${type_argument.signature}`)
                    continue
                }
            }
            args.push(type_parameter.signature)
        }
        return `<${args.join(",")}>`
    }

    private constructor(
        public type: Type,
        public type_parameters: TypeParameter[],
    ) {}

    get signature(): string {
        if (this.type_parameters.length === 0) {
            return this.type.signature
        }
        return `${this.type.signature}${TypeParameter.to_str(this.type_parameters, new Map())}`
    }

    type_variables(): TypeVariable[] {
        const res = this.type_parameters.flatMap((p) => p.type_variables())
        if (this.type instanceof TypeVariable) {
            res.push(this.type)
        }
        return res
    }

    contains_type_variable(): boolean {
        return this.type_variables().length > 0
    }
}

abstract class ComplexType<
    T extends {name: string; fields: Map<string, Type>; traits: string[]},
> extends Type {
    static from_env(name: string, env: TypeEnvironment, span: Span): ComplexType<any> {
        const type = env.get(name, span)
        if (!(type instanceof ComplexType)) {
            throw new TypeCheckError(`Expected complex type but got ${quote(type.signature)}`, span)
        }
        return type
    }

    protected _fields?: Map<string, Type>

    protected constructor(
        public readonly data: T,
        public readonly type_variables: TypeVariable[],
        public readonly type_arguments = new Map<TypeVariable, Type>(),
        public readonly type_variable_map = new Map<TypeVariable, TypeVariable>(),
    ) {
        super(data.name)
    }

    protected abstract new_instance(
        data: T,
        type_variables: TypeVariable[],
        type_arguments: Map<TypeVariable, Type>,
        type_variable_map: Map<TypeVariable, TypeVariable>,
    ): this

    private create_new_instance_if_differs(
        data: T,
        type_variables: TypeVariable[],
        type_arguments: Map<TypeVariable, Type>,
        type_variable_map: Map<TypeVariable, TypeVariable>,
    ): this {
        const new_instance = this.new_instance(
            data,
            type_variables,
            type_arguments,
            type_variable_map,
        )
        if (this.signature !== new_instance.signature) {
            return Type.add_or_get_known_type(
                new_instance.signature,
                new_instance,
                builtin_span,
            ) as this
        }
        return this
    }

    create_type_environment(env: TypeEnvironment, span: Span): TypeEnvironment {
        const result = new TypeEnvironment(env)
        for (const type_variable of this.type_variables) {
            result.add(type_variable.name, type_variable, span)
        }
        return result
    }

    with_type_arguments(
        type_arguments: (Type | null)[] | Map<TypeVariable, Type>,
        span: Span,
    ): this {
        if (Array.isArray(type_arguments)) {
            if (type_arguments.length !== this.type_variables.length) {
                throw new TypeCheckError(
                    `Expected ${this.type_variables.length} type arguments but got ${type_arguments.length}`,
                    span,
                )
            }
        }
        const new_type_arguments = new Map<TypeVariable, Type>(this.type_arguments)
        for (let i = 0; i < this.type_variables.length; i++) {
            const type_variable = this.type_variables[i]
            const type_argument = Array.isArray(type_arguments)
                ? type_arguments[i]
                : type_arguments.get(type_variable)
            if (!type_argument) {
                continue
            }
            if (type_argument instanceof TypeVariable) {
                throw new TypeCheckError(
                    `Cannot use type variable ${quote(type_argument.signature)} as type argument`,
                    span,
                )
            }
            new_type_arguments.set(type_variable, type_argument)
        }
        return this.create_new_instance_if_differs(
            this.data,
            this.type_variables,
            new_type_arguments,
            this.type_variable_map,
        )
    }

    with_scope_type_variables(map: Map<TypeVariable, TypeVariable>): this {
        const new_map = new Map(map)
        for (const [key, value] of this.type_variable_map.entries()) {
            if (!new_map.has(key)) {
                new_map.set(key, value)
            }
        }
        return this.new_instance(this.data, this.type_variables, this.type_arguments, new_map)
    }

    resolve_type_variables(map: Map<TypeVariable, Type>): Type {
        const new_type_arguments = new Map<TypeVariable, Type>(this.type_arguments)
        for (const key of map.keys()) {
            const type = map.get(this.type_variable_map.get(key) ?? key)
            if (type) {
                new_type_arguments.set(key, type)
            }
        }
        return this.create_new_instance_if_differs(
            this.data,
            this.type_variables,
            new_type_arguments,
            this.type_variable_map,
        )
    }

    unresolved_type_variables(): TypeVariable[] {
        const result = []
        for (const type_variable of this.type_variables) {
            const type_argument = this.type_arguments.get(type_variable)
            if (!type_argument || type_argument instanceof TypeVariable) {
                result.push(type_variable)
            }
        }
        return result
    }

    add_field<T extends Type>(
        name: string,
        field_type: T,
        type_parameters: TypeParameters,
        span: Span,
    ): T {
        if (type_parameters.type_parameters.length > 0) {
            assert(field_type instanceof ComplexType)
            const type_variables = field_type.type_variables
            const type_variable_map = new Map<TypeVariable, TypeVariable>(
                field_type.type_variable_map,
            )
            const type_arguments = new Map<TypeVariable, Type>()
            if (type_variables.length !== type_parameters.type_parameters.length) {
                throw new TypeCheckError(
                    `Expected ${type_variables.length} type parameters but got ${type_parameters.type_parameters.length}`,
                    span,
                )
            }
            for (let i = 0; i < type_parameters.type_parameters.length; i++) {
                const type_parameter = type_parameters.type_parameters[i]
                const type_variable = type_variables[i]
                if (type_parameter.type instanceof TypeVariable) {
                    if (
                        !type_variable_map.has(type_variable) &&
                        type_variable !== type_parameter.type
                    ) {
                        type_variable_map.set(type_variable, type_parameter.type)
                    }
                } else {
                    type_arguments.set(type_variable, type_parameter.type)
                }
            }
            field_type = field_type.with_scope_type_variables(type_variable_map)
            field_type = field_type.resolve_type_variables(type_arguments) as T
        }
        if (field_type instanceof ComplexType) {
            field_type = field_type.with_scope_type_variables(this.type_variable_map)
        }
        this.data.fields.set(name, field_type)
        this._fields = undefined
        return field_type
    }

    add_field_immediate<T extends Type>(name: string, field_type: T): T {
        this.data.fields.set(name, field_type)
        this._fields = undefined
        return field_type
    }

    get traits(): string[] {
        return this.data.traits
    }

    get type_signature(): string {
        const type_variables = this.type_variables.map((v) => this.type_variable_map.get(v) ?? v)
        return TypeParameter.to_str(type_variables, this.type_arguments)
    }

    get fields(): Map<string, Type> {
        // if (this._fields) {
        //     return this._fields
        // }
        this._fields = new Map()
        for (let [name, type] of this.data.fields) {
            if (type instanceof TypeVariable) {
                // Respect `this.type_variable_map` here.
                type = this.type_variable_map.get(type) ?? type
            }
            type = type.resolve_type_variables(this.type_arguments)
            if (type instanceof ComplexType) {
                type = type.with_scope_type_variables(this.type_variable_map)
            }
            this._fields.set(name, type)
        }
        return this._fields
    }

    field(name: string, span: Span): Type {
        const field = this.fields.get(name)
        if (!field) {
            throw new TypeCheckError(
                `Unknown field ${quote(name)} in ${quote(this.signature)}`,
                span,
            )
        }
        return field
    }

    equals(other: Type): boolean {
        if (!(other instanceof ComplexType)) {
            return false
        }
        if (this.data !== other.data) {
            return false
        }
        const this_arguments = new Map(
            [...this.type_arguments.entries()].map(([k, v]) => [k, v || k.default_type]),
        )
        const other_arguments = new Map(
            [...other.type_arguments.entries()].map(([k, v]) => [k, v || k.default_type]),
        )
        for (const type_variable of this.type_variables) {
            if (type_variable.default_type && !this_arguments.has(type_variable)) {
                this_arguments.set(type_variable, type_variable.default_type)
            }
        }
        for (const type_variable of other.type_variables) {
            if (type_variable.default_type && !other_arguments.has(type_variable)) {
                other_arguments.set(type_variable, type_variable.default_type)
            }
        }
        if (this_arguments.size !== other_arguments.size) {
            return false
        }
        for (const [key, value] of this_arguments.entries()) {
            const other_value = other_arguments.get(key)
            if (!other_value || !value.equals(other_value)) {
                return false
            }
        }
        return this.data === other.data
    }

    assignable_to(other: Type): boolean {
        if (this.equals(other)) {
            return true
        }
        if (!(other instanceof ComplexType)) {
            return false
        }
        if (other instanceof TraitType) {
            return this.traits.includes(other.name)
        }
        const this_arguments = new Map(
            [...this.type_arguments.entries()].map(([k, v]) => [k, v || k.default_type]),
        )
        const other_arguments = new Map(
            [...other.type_arguments.entries()].map(([k, v]) => [k, v || k.default_type]),
        )
        for (const type_variable of this.type_variables) {
            if (type_variable.default_type && !this_arguments.has(type_variable)) {
                this_arguments.set(type_variable, type_variable.default_type)
            }
        }
        for (const type_variable of other.type_variables) {
            if (type_variable.default_type && !other_arguments.has(type_variable)) {
                other_arguments.set(type_variable, type_variable.default_type)
            }
        }
        if (this_arguments.size !== other_arguments.size) {
            return false
        }
        for (const [key, value] of this_arguments.entries()) {
            const other_value = other_arguments.get(key)
            if (!other_value || !value.equals(other_value)) {
                return false
            }
        }
        return false
    }

    has_type_parameters(): boolean {
        return this.type_variables.length > 0
    }
}

export class StructType extends ComplexType<StructData> {
    static from_declaration(declaration: ast.StructDeclaration, env: TypeEnvironment): StructType {
        const type_variables = declaration.type_parameters.map((p) =>
            TypeVariable.from_declaration(p, env),
        )
        const struct_type = new StructType(
            new StructData(declaration.name, declaration),
            type_variables,
        )
        return Type.add_or_get_known_type(struct_type.signature, struct_type, declaration.span)
    }

    static from_env(name: string, env: TypeEnvironment, span: Span): StructType {
        const type = env.get(name, span)
        if (!(type instanceof StructType)) {
            throw new TypeCheckError(`Expected struct type but got ${quote(type.signature)}`, span)
        }
        return type
    }

    private constructor(
        public data: StructData,
        type_variables: TypeVariable[],
        type_arguments?: Map<TypeVariable, Type>,
        type_variable_map?: Map<TypeVariable, TypeVariable>,
    ) {
        super(data, type_variables, type_arguments, type_variable_map)
    }

    new_instance(
        data: StructData,
        type_variables: TypeVariable[],
        type_arguments: Map<TypeVariable, Type>,
        type_variable_map: Map<TypeVariable, TypeVariable>,
    ): this {
        return new StructType(data, type_variables, type_arguments, type_variable_map) as this
    }

    get declaration(): ast.StructDeclaration {
        return this.data.declaration!
    }

    get debug_str() {
        const fields = [...this.fields.entries()].map(
            ([name, field]) => `${name}: ${field.signature}`,
        )
        return `${this.signature}{${fields.join(", ")}}`
    }

    get signature() {
        return `${this.data.name}${this.type_signature}`
    }
}

class StructData {
    constructor(
        public readonly name: string,
        public readonly declaration: ast.StructDeclaration,
        public readonly fields: Map<string, Type> = new Map(),
        public readonly traits: string[] = [],
    ) {}
}

class EnumType extends ComplexType<EnumData> {
    static from_declaration(declaration: ast.EnumDeclaration, env: TypeEnvironment): EnumType {
        const type_variables = declaration.type_parameters.map((p) =>
            TypeVariable.from_declaration(p, env),
        )
        const enum_type = new EnumType(new EnumData(declaration.name, declaration), type_variables)
        return Type.add_or_get_known_type(enum_type.signature, enum_type, declaration.span)
    }

    static from_env(name: string, env: TypeEnvironment, span: Span): EnumType {
        const type = env.get(name, span)
        if (!(type instanceof EnumType)) {
            throw new TypeCheckError(`Expected enum type but got ${quote(type.signature)}`, span)
        }
        return type
    }

    static variant_from_declaration(
        enum_type: EnumType,
        declaration: ast.EnumVariant,
        env: TypeEnvironment,
    ): EnumType {
        const data = new EnumData(
            enum_type.data.name,
            undefined,
            enum_type.data.fields,
            enum_type.data.traits,
        )
        const type_variables = enum_type.type_variables
        return new EnumType(
            data,
            enum_type.type_variables,
            enum_type.type_arguments,
            enum_type.type_variable_map,
            declaration.name,
            TupleType.from_declaration(
                declaration.fields,
                enum_type.create_type_environment(env, declaration.span),
                type_variables,
                declaration.span,
            ),
            enum_type,
        )
    }

    private constructor(
        public data: EnumData,
        type_variables: TypeVariable[],
        type_arguments?: Map<TypeVariable, Type>,
        type_variable_map?: Map<TypeVariable, TypeVariable>,
        private variant_name?: string,
        public variant_tuple_type?: TupleType,
        private variant_parent?: EnumType,
    ) {
        super(data, type_variables, type_arguments, type_variable_map)
    }

    new_instance(
        data: EnumData,
        type_variables: TypeVariable[],
        type_arguments: Map<TypeVariable, Type>,
        type_variable_map: Map<TypeVariable, TypeVariable>,
    ): this {
        const type = new EnumType(data, type_variables, type_arguments, type_variable_map) as this
        if (this.is_variant) {
            type.variant_parent = this.variant_parent
            type.variant_name = this.variant_name
            type.variant_tuple_type = this.variant_tuple_type!.resolve_type_variables(
                type_arguments,
            ) as TupleType
            type.variant_tuple_type =
                type.variant_tuple_type!.with_scope_type_variables(type_variable_map)
            return type
        }
        for (const [name, variant] of this.variants) {
            type.add_field_immediate(name, variant)
        }
        return type
    }

    get declaration(): ast.EnumDeclaration | undefined {
        return this.data.declaration
    }

    get fields(): Map<string, Type> {
        if (this.is_variant) {
            const fields = new Map(
                [...this.variant_parent!.fields.entries()].filter(
                    ([_, type]) => type instanceof FunctionType,
                ),
            )
            return fields
        }
        return super.fields
    }

    get is_variant(): boolean {
        return !!this.variant_name
    }

    get variants(): Map<string, EnumType> {
        if (this.is_variant) {
            return new Map()
        }
        return new Map(
            [...this.data.fields.entries()].filter(([, type]) => type instanceof EnumType),
        ) as Map<string, EnumType>
    }

    get debug_str() {
        if (this.is_variant) {
            return `${this.signature}${this.variant_tuple_type?.signature}`
        }
        const variants = [...this.variants.entries()].map(
            ([name, variant]) => `${name}: ${variant.variant_tuple_type?.signature}`,
        )
        const fields = [...this.fields.entries()]
            .filter(([_, type]) => !(type instanceof EnumType))
            .map(([name, type]) => `${name}: ${type.signature}`)
            .join(", ")
        return `${this.signature}{${variants.join(", ")}${fields ? `, ${fields}` : ""}}`
    }

    get signature() {
        if (this.is_variant) {
            return `${this.data.name}${this.type_signature}.${this.variant_name}`
        }
        return `${this.data.name}${this.type_signature}`
    }

    equals(other: Type): boolean {
        if (!(other instanceof EnumType)) {
            return false
        }
        if (this === other) {
            return true
        }
        if (this.data === other.data) {
            return true
        }
        const this_enum_type = this.variant_parent ?? this
        const other_enum_type = other.variant_parent ?? other
        return (
            this_enum_type.data.name === other_enum_type.data.name &&
            this_enum_type.data.fields === other_enum_type.data.fields
        )
    }
}

class EnumData {
    constructor(
        public readonly name: string,
        public readonly declaration?: ast.EnumDeclaration,
        public readonly fields: Map<string, Type> = new Map(),
        public readonly traits: string[] = [],
    ) {}
}

export class TraitType extends ComplexType<TraitData> {
    static from_declaration(declaration: ast.TraitDeclaration, env: TypeEnvironment): TraitType {
        const type_variables = declaration.type_parameters.map((p) =>
            TypeVariable.from_declaration(p, env),
        )
        const functions_with_default_impl = declaration.functions
            .filter((f) => f instanceof ast.FunctionDefinition)
            .map((f) => f.name)
        const struct_data = new TraitData(
            declaration.name,
            declaration,
            new Map(),
            functions_with_default_impl,
        )
        const trait_type = new TraitType(struct_data, type_variables)
        return Type.add_or_get_known_type(trait_type.signature, trait_type, declaration.span)
    }

    static from_env(name: string, env: TypeEnvironment, span: Span): TraitType {
        const type = env.get(name, span)
        if (!(type instanceof TraitType)) {
            throw new TypeCheckError(`Expected trait type but got ${quote(type.signature)}`, span)
        }
        return type
    }

    private constructor(
        public data: TraitData,
        type_variables: TypeVariable[],
        type_arguments?: Map<TypeVariable, Type>,
        type_variable_map?: Map<TypeVariable, TypeVariable>,
    ) {
        super(data, type_variables, type_arguments, type_variable_map)
    }

    new_instance(
        data: TraitData,
        type_variables: TypeVariable[],
        type_arguments: Map<TypeVariable, Type>,
        type_variable_map: Map<TypeVariable, TypeVariable>,
    ): this {
        return new TraitType(data, type_variables, type_arguments, type_variable_map) as this
    }

    copy_without_fields(): this {
        const data = new TraitData(
            this.data.name,
            this.data.declaration,
            new Map(),
            this.data.traits,
        )
        return this.new_instance(
            data,
            this.type_variables,
            this.type_arguments,
            this.type_variable_map,
        )
    }

    get debug_str() {
        const fields = [...this.fields.entries()].map(
            ([name, type]) => `${name}: ${type.signature}`,
        )
        return `${this.signature}{${fields.join(", ")}}`
    }

    get signature() {
        return `${this.data.name}${this.type_signature}`
    }

    assignable_to(other: Type): boolean {
        if (this.equals(other)) {
            return true
        }
        if (!(other instanceof ComplexType)) {
            return false
        }
        if (this.data.name === other.data.name) {
            return true
        }
        return this.data.traits.includes(other.data.name)
    }
}

class TraitData {
    constructor(
        public readonly name: string,
        public readonly declaration: ast.TraitDeclaration,
        public readonly fields: Map<string, Type> = new Map(),
        public readonly functions_with_default_impl: string[],
        public readonly traits: string[] = [],
    ) {}
}

export class FunctionType extends ComplexType<FunctionData> {
    static from_declaration(
        declaration: ast.FunctionDeclaration | ast.FunctionTypeDeclaration | ast.ClosureDefinition,
        env: TypeEnvironment,
    ): FunctionType {
        let type_variables: TypeVariable[] = []
        if (!(declaration instanceof ast.ClosureDefinition)) {
            type_variables = declaration.type_parameters.map((p) =>
                TypeVariable.from_declaration(p, env),
            )
        }
        const function_env = new TypeEnvironment(env)
        for (const type_variable of type_variables) {
            function_env.add(type_variable.name, type_variable, declaration.span)
        }
        function resolve_type(declaration: ast.TypeDeclaration) {
            let type: Type
            if (declaration instanceof ast.FunctionTypeDeclaration) {
                type = FunctionType.from_declaration(declaration, function_env)
            } else {
                type = Type.from_env_or_declaration(declaration, function_env)
            }
            if (!(type instanceof ComplexType)) {
                return type
            }
            const type_variable_map = TypeVariable.create_type_variable_map(
                type.type_variables,
                TypeParameters.from_declaration(
                    declaration.type_parameters,
                    env,
                ).all_type_variables(),
            )
            type = type.with_scope_type_variables(type_variable_map)
            if (declaration.type_parameters.length > 0) {
                type = (type as ComplexType<any>).with_type_arguments(
                    parse_type_arguments(
                        type as ComplexType<any>,
                        declaration.type_parameters,
                        function_env,
                        declaration.span,
                    ),
                    declaration.span,
                )
            }
            return type
        }
        const parameters =
            declaration instanceof ast.FunctionDeclaration
                ? declaration.parameters
                : declaration instanceof ast.ClosureDefinition
                ? declaration.parameters.map((p) => ({name: p.name, type: p.type}))
                : declaration.arg_types.map((type, i) => ({name: `${i}`, type}))
        const fields = new Map<string, Type>()
        for (const parameter of parameters) {
            const type = resolve_type(parameter.type)
            if (!env.get_or_null(type.name)) {
                env.add(type.name, type, parameter.type.span)
            }
            fields.set(parameter.name, type)
        }
        let return_type = resolve_type(declaration.return_type)
        const throws = declaration instanceof ast.FunctionDeclaration ? declaration.throws : false
        if (throws) {
            return_type = expect_type_with_fields(
                env.get("Result", declaration.span),
                declaration.span,
            ).with_type_arguments([return_type, env.unit], declaration.span)
        }
        fields.set("return", return_type)
        const data = new FunctionData(
            declaration instanceof ast.ClosureDefinition ? "<closure>" : declaration.name,
            fields,
            parameters.map((p) => p.name),
        )
        const type = new FunctionType(data, type_variables)
        return Type.add_or_get_known_type(type.signature, type, declaration.span)
    }

    static from_env(name: string, env: TypeEnvironment, span: Span): FunctionType {
        const type = env.get(name, span)
        if (!(type instanceof FunctionType)) {
            throw new TypeCheckError(
                `Expected function type but got ${quote(type.signature)}`,
                span,
            )
        }
        return type
    }

    static from_data(data: FunctionData, span: Span): FunctionType {
        const type_variables = data.fields.get("self") ? [TypeVariable.Self] : []
        const type = new FunctionType(data, type_variables)
        return Type.add_or_get_known_type(type.signature, type, span)
    }

    private constructor(
        data: FunctionData,
        type_variables: TypeVariable[],
        type_arguments?: Map<TypeVariable, Type>,
        type_variable_map?: Map<TypeVariable, TypeVariable>,
    ) {
        super(data, type_variables, type_arguments, type_variable_map)
    }

    new_instance(
        data: FunctionData,
        type_variables: TypeVariable[],
        type_arguments: Map<TypeVariable, Type>,
        type_variable_map: Map<TypeVariable, TypeVariable>,
    ): this {
        return new FunctionType(data, type_variables, type_arguments, type_variable_map) as this
    }

    is_instance_function(): boolean {
        return this.data.parameter_names[0] === "self"
    }

    get parameters_without_self() {
        if (!this.is_instance_function()) {
            return this.parameters
        }
        return this.parameters.slice(1)
    }

    get parameters() {
        return this.data.parameter_names.map((name) => this.field(name, builtin_span))
    }

    get parameter_names() {
        return this.data.parameter_names
    }

    parameter(name: string, span: Span): Type {
        return this.field(name, span)
    }

    get return_type() {
        return this.field("return", builtin_span)
    }

    get debug_str() {
        return this.signature
    }

    get signature() {
        const parameters = this.parameters.map((p) => p.signature).join(",")
        const return_type = this.return_type.signature
        return `${this.name || "fn"}${this.type_signature}(${parameters})->${return_type}`
    }

    assignable_to(other: Type): boolean {
        if (this.equals(other)) {
            return true
        }
        if (!(other instanceof FunctionType)) {
            return false
        }
        if (this.parameters.length !== other.parameters.length) {
            return false
        }
        if (!this.return_type.assignable_to(other.return_type)) {
            return false
        }
        const self_parameters = this.parameters_without_self
        const other_parameters = other.parameters_without_self
        for (let i = 0; i < self_parameters.length; i++) {
            if (!self_parameters[i].assignable_to(other_parameters[i])) {
                return false
            }
        }
        return true
    }
}

class FunctionData extends Type {
    constructor(
        public readonly name: string,
        public readonly fields: Map<string, Type>,
        public readonly parameter_names: string[],
        public readonly traits: string[] = [],
    ) {
        super(name)
    }
}

class TupleType extends ComplexType<TupleData> {
    static from_declaration(
        declaration: ast.TupleTypeDeclaration,
        env: TypeEnvironment,
        type_variables: TypeVariable[],
        span: Span,
    ): TupleType {
        const types = declaration.fields.map((f) => Type.from_env_or_declaration(f, env))
        const fields = new Map<string, Type>()
        for (let i = 0; i < types.length; i++) {
            fields.set(`${i}`, types[i])
        }
        const data = new TupleData(`(${types.map((t) => t.signature).join(",")})`, fields)
        const type = new TupleType(data, type_variables)
        return Type.add_or_get_known_type(type.signature, type, span)
    }

    static from_types(types: Type[]): TupleType {
        const name = `(${types.map((t) => t.signature).join(",")})`
        const fields = new Map<string, Type>(types.map((t, i) => [`${i}`, t]))
        const type = new TupleType(new TupleData(name, fields), [])
        return Type.add_or_get_known_type(type.signature, type, builtin_span)
    }

    private constructor(
        public data: TupleData,
        type_variables: TypeVariable[],
        type_arguments?: Map<TypeVariable, Type>,
        type_variable_map?: Map<TypeVariable, TypeVariable>,
    ) {
        super(data, type_variables, type_arguments, type_variable_map)
    }

    new_instance(
        data: TupleData,
        type_variables: TypeVariable[],
        type_arguments: Map<TypeVariable, Type>,
        type_variable_map: Map<TypeVariable, TypeVariable>,
    ): this {
        return new TupleType(data, type_variables, type_arguments, type_variable_map) as this
    }

    get debug_str() {
        return this.signature
    }

    get signature() {
        let s = "("
        for (let i = 0; i < this.fields.size; i++) {
            const field = this.fields.get(`${i}`)
            if (s.length > 1) {
                s += ","
            }
            s += field!.signature
        }
        s += ")"
        return s
    }

    assignable_to(other: Type): boolean {
        if (this.equals(other)) {
            return true
        }
        if (!(other instanceof TupleType)) {
            return false
        }
        if (this.fields.size !== other.fields.size) {
            return false
        }
        for (let i = 0; i < this.fields.size; i++) {
            const this_field = this.fields.get(`${i}`)!
            const other_field = other.fields.get(`${i}`)!
            if (!this_field.assignable_to(other_field)) {
                return false
            }
        }
        return true
    }
}

class TupleData {
    constructor(
        public readonly name: string,
        public readonly fields: Map<string, Type>,
        public readonly traits: string[] = [],
    ) {}
}

const builtin_span = new Span(0, 0, "<builtin>", "")

export class StrType extends ComplexType<StrData> {
    static default() {
        const type = new StrType("str")
        return Type.add_or_get_known_type(type.signature, type, builtin_span)
    }

    private constructor(name: string) {
        super(new StrData(name, new Map()), [])
    }

    new_instance(): this {
        return this
    }
}

class StrData {
    constructor(
        public readonly name: string,
        public readonly fields: Map<string, Type>,
        public readonly traits: string[] = [],
    ) {}
}

export class BoolType extends ComplexType<BoolData> {
    static default() {
        const type = new BoolType("bool")
        return Type.add_or_get_known_type(type.signature, type, builtin_span)
    }

    private constructor(name: string) {
        super(new BoolData(name, new Map()), [])
    }

    new_instance(): this {
        return this
    }
}

class BoolData {
    constructor(
        public readonly name: string,
        public readonly fields: Map<string, Type>,
        public readonly traits: string[] = [],
    ) {}
}

export class NumericType extends ComplexType<NumericData> {
    static from_name(name: string): NumericType {
        const type = new NumericType(name)
        return Type.add_or_get_known_type(type.signature, type, builtin_span)
    }

    private constructor(name: string) {
        super(new NumericData(name, new Map()), [])
    }

    new_instance(): this {
        return this
    }
}

class NumericData {
    constructor(
        public readonly name: string,
        public readonly fields: Map<string, Type>,
        public readonly traits: string[] = [],
    ) {}
}

export class ArrayType extends ComplexType<ArrayData> {
    static from_type(type: Type, span: Span): ArrayType {
        const data = new ArrayData(`[${type.signature}]`, new Map(), type)
        const array_type = new ArrayType(data, [])
        return Type.add_or_get_known_type(array_type.signature, array_type, span)
    }

    private constructor(
        public data: ArrayData,
        type_variables: TypeVariable[],
        type_arguments?: Map<TypeVariable, Type>,
        type_variable_map?: Map<TypeVariable, TypeVariable>,
    ) {
        super(data, type_variables, type_arguments, type_variable_map)
    }

    new_instance(
        data: ArrayData,
        type_variables: TypeVariable[],
        type_arguments: Map<TypeVariable, Type>,
        type_variable_map: Map<TypeVariable, TypeVariable>,
    ): this {
        return new ArrayType(data, type_variables, type_arguments, type_variable_map) as this
    }

    get debug_str() {
        return this.signature
    }

    get signature() {
        return `[${this.data.element_type.signature}]`
    }
}

class ArrayData {
    constructor(
        public readonly name: string,
        public readonly fields: Map<string, Type>,
        public readonly element_type: Type,
        public readonly traits: string[] = [],
    ) {}
}

type CoreTrait = "Add" | "Sub" | "Mul" | "Div" | "PartialEq" | "PartialOrd" | "ToStr"

function add_core_traits<T extends ComplexType<any>>(
    type: T,
    bool_type: BoolType,
    str_type: StrType,
    ...names: CoreTrait[]
): T {
    const trait_functions: Record<CoreTrait, string[]> = {
        Add: ["add"],
        Sub: ["sub"],
        Mul: ["mul"],
        Div: ["div"],
        PartialEq: ["eq", "ne"],
        PartialOrd: ["lt", "le", "gt", "ge"],
        ToStr: ["to_str"],
    }
    for (const name of names) {
        if (type.data.traits.includes(name)) {
            continue
        }
        type.data.traits.push(name)
        for (const function_name of trait_functions[name]) {
            if (function_name === "to_str") {
                type.data.fields.set(
                    function_name,
                    FunctionType.from_data(
                        new FunctionData(
                            function_name,
                            new Map<string, Type>([
                                ["self", type],
                                ["return", str_type],
                            ]),
                            ["self"],
                        ),
                        builtin_span,
                    ),
                )
                continue
            }
            type.add_field_immediate(
                function_name,
                FunctionType.from_data(
                    new FunctionData(
                        function_name,
                        new Map<string, Type>([
                            ["self", type],
                            ["rhs", type],
                            [
                                "return",
                                ["PartialEq", "PartialOrd"].includes(name) ? bool_type : type,
                            ],
                        ]),
                        ["self", "rhs"],
                    ),
                    builtin_span,
                ),
            )
        }
    }
    return type
}

export class TypeEnvironment {
    static global() {
        Type.clear_known_types()
        const env = new TypeEnvironment()
        const bool = BoolType.default()
        const str = StrType.default()
        env.add(
            "i32",
            add_core_traits(
                NumericType.from_name("i32"),
                bool,
                str,
                "PartialEq",
                "PartialOrd",
                "ToStr",
                "Add",
                "Sub",
                "Mul",
                "Div",
            ),
            builtin_span,
        )
        env.add(
            "bool",
            add_core_traits(bool, bool, str, "PartialEq", "PartialOrd", "ToStr"),
            builtin_span,
        )
        env.add("str", add_core_traits(str, bool, str, "PartialEq", "ToStr"), builtin_span)
        env.add("()", Type.unit, builtin_span)
        return env
    }

    private types_by_name = new Map<string, Type>()

    constructor(private parent?: TypeEnvironment) {}

    add(name: string, type: Type, span: Span) {
        if (this.types_by_name.has(name)) {
            throw new TypeCheckError(
                `${quote(name)} already defined in type environment as ${quote(
                    this.types_by_name.get(type.name)?.signature,
                )}`,
                span,
            )
        }
        if (type instanceof TypeParameter) {
            throw new TypeCheckError(
                `Cannot add type parameter ${quote(name)} to type environment`,
                span,
            )
        }
        this.types_by_name.set(name, type)
    }

    get(name: string, span: Span): Type {
        const type = this.get_or_null(name)
        if (!type) {
            throw new TypeCheckError(`Unknown ${quote(name)} in type environment`, span)
        }
        return type
    }

    get_or_null(name: string): Type | null {
        return this.types_by_name.get(name) ?? this.parent?.get_or_null(name) ?? null
    }

    get i32() {
        return this.get("i32", builtin_span)
    }

    get bool() {
        return this.get("bool", builtin_span)
    }

    get unit() {
        return this.get("()", builtin_span)
    }

    get str() {
        return this.get("str", builtin_span)
    }

    debug_str(indent = 0): string {
        const lines = []
        for (const [name, type] of this.types_by_name.entries()) {
            lines.push(`${" ".repeat(indent)}${name}: ${type.debug_str}`)
        }
        return lines.join("\n") + "\n" + (this.parent?.debug_str(indent + 2) ?? "")
    }
}

// Tests

const test = {
    parse(src: string) {
        return parse(new TokenStream(lexer({src, file: "test"})))
    },

    type_check(
        src: string,
        opts?: {with_core_traits?: boolean},
    ): {type: Type; env: TypeEnvironment} {
        if (opts?.with_core_traits) {
            src =
                `
                trait Add:
                    fn add(self, rhs Self) Self
                end
                trait Sub:
                    fn sub(self, rhs Self) Self
                end
                trait Mul:
                    fn mul(self, rhs Self) Self
                end
                trait Div:
                    fn div(self, rhs Self) Self
                end
                trait PartialEq:
                    fn eq(self, rhs Self) bool
                    fn ne(self, rhs Self) bool
                end
                trait PartialOrd:
                    fn lt(self, rhs Self) bool
                    fn le(self, rhs Self) bool
                    fn gt(self, rhs Self) bool
                    fn ge(self, rhs Self) bool
                end
                trait Ord:
                end
                trait ToStr:
                    fn to_str(self) str
                end
                trait IndexedGet<K, V>:
                    fn get(self, key K) V
                end
                trait IndexedSet<K, V>:
                    fn set(self, key K, value V)
                end
                struct Array<T>:
                end
                struct Error<T=()>:
                    message str
                    data T
                end
                enum Result<T, E=()>:
                    Ok(T)
                    Err(Error<E>)
                end
                fn panic(message str):
                end
            ` + src
        }
        const ast = test.parse(src)
        const env = TypeEnvironment.global()
        return {type: type_check_ast(ast, env), env}
    },

    type_check_with_core_types(src: string) {
        return test.type_check(src, {with_core_traits: true})
    },

    test_let() {
        const {type} = test.type_check("let a = 1")
        assert.strictEqual(type, Type.unit)
    },

    test_let_with_basic_types() {
        const {env} = test.type_check(
            `
            let a = 1
            let b = true
            let c = "foo"
        `,
        )
        assert.equal(env.get("a", builtin_span), env.i32)
        assert.equal(env.get("b", builtin_span), env.bool)
        assert.equal(env.get("c", builtin_span), env.str)
    },

    test_let_with_type_declaration() {
        const {type} = test.type_check("let a i32 = 1")
        assert.strictEqual(type, Type.unit)
    },

    test_let_with_mismatching_type_declaration() {
        assert.throws(
            () => test.type_check("let a bool = 1"),
            /Expected `bool \(BoolType\)` but got `i32 \(NumericType\)`/,
        )
    },

    test_let_with_unit_type_is_not_allowed() {
        assert.throws(
            () =>
                test.type_check(`
                fn foo(): 
                end

                let a = foo()
            `),
            /Cannot assign the unit value to a variable/,
        )
    },

    test_identifier_reference_variable() {
        const {type, env} = test.type_check("let a = 1 a")
        assert.strictEqual(type, env.i32)
    },

    test_identifier_reference_unknown_variable() {
        assert.throws(() => test.type_check("a"), /Unknown `a` in type environment/)
    },

    test_identifier_reference_simple_type() {
        const {type, env} = test.type_check("i32")
        assert.strictEqual(type, env.i32)
    },

    test_identifier_reference_struct_type() {
        const {type} = test.type_check("struct Foo: a i32 end Foo")
        assert.equal(type.signature, "Foo<>")
    },

    test_identifier_reference_function_type() {
        const {type} = test.type_check("fn foo(a i32, b bool): end foo")
        assert.equal(type.signature, "foo<>(i32,bool)->()")
    },

    test_function_definition() {
        const {env} = test.type_check("fn foo(a i32, b bool): end")
        const type = env.get("foo", builtin_span)
        assert.strictEqual(type.signature, "foo<>(i32,bool)->()")
    },

    test_nested_function_definition() {
        const {env} = test.type_check(`
            fn foo(a i32) i32: 
                fn bar() bool: 
                    true
                end
                if bar() => 1 else => 2 
            end
        `)
        const type = env.get("foo", builtin_span)
        assert.strictEqual(type.signature, "foo<>(i32)->i32")
        // `bar` is not visible outside of `foo`.
        assert.equal(env.get_or_null("bar"), null)
    },

    test_nested_function_definition_in_sub_block() {
        assert.throws(
            () =>
                test.type_check(`
            fn foo(a i32) i32: 
                if true:
                    fn bar() bool:
                        true
                    end
                    if bar() => 1 else => 2 
                else:
                    bar()
                end
            end
        `),
            /Unknown `bar` in type environment at test:9:21/,
        )
    },

    test_nested_function_definition_captures_variables() {
        const {env} = test.type_check(`
            fn foo(a i32) i32: 
                let b = true
                fn bar() bool: 
                    b 
                end
                if bar() => 1 else => 2 
            end
        `)
        const type = env.get("foo", builtin_span)
        assert.strictEqual(type.signature, "foo<>(i32)->i32")
    },

    test_function_duplication() {
        assert.throws(
            () => test.type_check("fn foo(): end fn foo(a i32): end"),
            /`foo` already defined/,
        )
    },

    test_generic_function() {
        const {env} = test.type_check("fn foo<T>(a T): end")
        const type = env.get("foo", builtin_span)
        assert.strictEqual(type.signature, "foo<T>(T)->()")
        assert.strictEqual(type.constructor, FunctionType)
    },

    test_closure_captures_variables() {
        const {type} = test.type_check(
            `
            fn foo(a i32) i32: 
                let b = 1
                let bar = fn() i32:
                    b 
                end
                bar()
            end
            foo
        `,
        )
        assert.strictEqual(type.signature, "foo<>(i32)->i32")
    },

    test_struct() {
        const {env} = test.type_check("struct Foo: a i32 end")
        const foo = env.get("Foo", builtin_span)
        assert.equal(foo.signature, "Foo<>")
        assert(foo instanceof StructType)
        assert.deepEqual([...foo.fields.keys()], ["a"])
    },

    test_struct_with_tuple_field() {
        const {env} = test.type_check("struct Foo: a (i32, bool) end")
        const foo = env.get("Foo", builtin_span)
        assert.equal(foo.debug_str, "Foo<>{a: (i32,bool)}")
    },

    test_struct_duplicate_struct() {
        assert.throws(
            () => test.type_check("struct Foo: a i32 end struct Foo: a i32 end"),
            /`Foo` already defined/,
        )
    },

    test_generic_struct() {
        const {env} = test.type_check("struct Foo<T>: a T end")
        const foo = env.get("Foo", builtin_span)
        assert.equal(foo.signature, "Foo<T>")
        assert(foo instanceof StructType)
        const a = foo.fields.get("a")!
        assert(a instanceof TypeVariable)
        assert.equal(a.signature, "T")
    },

    test_generic_struct_with_default_type() {
        const {env} = test.type_check(`
            struct Foo<T=i32>: 
                a T
            end
            
            let foo_default = Foo{a: 1}
            let foo_bool = Foo<bool>{a: true}
        `)
        assert.equal(env.get("foo_default", builtin_span).signature, "Foo<T default i32=i32>")
        assert.equal(env.get("foo_bool", builtin_span).signature, "Foo<T default i32=bool>")
    },

    test_generic_struct_with_trailing_default_type() {
        const {env} = test.type_check(`
            struct Error<T=()>:
                message str
                data T
            end

            enum Result<T, E=()>:
                Ok(T)
                Err(Error<E>)
            end

            let a = Result<i32>.Ok(1)
            let b = Result<i32>.Err(Error{message: "foo", data: ()})
        `)
        assert.equal(env.get("a", builtin_span).signature, "Result<T=i32,E default ()=()>.Ok")
        assert.equal(env.get("b", builtin_span).signature, "Result<T=i32,E default ()=()>.Err")
    },

    test_generic_struct_with_type_parameter_that_is_not_a_type_variable() {
        assert.throws(
            () =>
                test.type_check(`
                struct Foo<T>:
            end

                struct Bar<Foo<T>>:
            end
                `),
            /Expected type variable but got `Foo<T>`/,
        )
    },

    test_impl_for_struct() {
        const {env} = test.type_check(`
            struct Foo:
            a i32 
            end

            impl Foo:
            fn foo():
            end
            end
                `)
        const foo = env.get("Foo", builtin_span)
        assert.equal(foo.signature, "Foo<>")
        assert(foo instanceof StructType)
        assert.equal(foo.debug_str, "Foo<>{a: i32, foo: foo<>()->()}")
    },

    test_impl_for_struct_fails_if_field_exists() {
        assert.throws(
            () =>
                test.type_check(`
            struct Foo:
            a i32 
            end

            impl Foo:
            fn a():
            end
            end
                `),
            /`a` already defined/,
        )
    },

    test_impl_for_generic_struct() {
        const {env} = test.type_check(`
            struct Foo<T>:
            a T
            end

            impl Foo<A>:
            fn foo(v A) A:
            v 
                end
            end
                `)
        const foo = env.get("Foo", builtin_span)
        assert.equal(foo.signature, "Foo<T>")
        assert(foo instanceof StructType)
        assert.equal(foo.fields.get("foo")?.signature, "foo<>(T)->T")
        assert.equal(foo.debug_str, "Foo<T>{a: T, foo: foo<>(T)->T}")
    },

    test_impl_with_self_type() {
        const {env} = test.type_check(`
            struct Foo:
            a i32 
            end

            impl Foo:
            fn foo(self) Self:
            self.a-- Test accessing a field of \`self\`
                    self
                end
            end
        `)
        const foo = env.get("Foo", builtin_span)
        assert.equal(foo.signature, "Foo<>")
        assert(foo instanceof StructType)
        assert.equal(foo.fields.get("foo")?.signature, "foo<>(Foo<>)->Foo<>")
        assert.equal(foo.debug_str, "Foo<>{a: i32, foo: foo<>(Foo<>)->Foo<>}")
    },

    test_impl_with_self_type_for_generic_struct() {
        const {env} = test.type_check(`
            struct Foo<T>: 
                a T
            end

            impl Foo<A>: 
                fn foo(self) Self: 
                    self
                end
            end
        `)
        const foo = env.get("Foo", builtin_span)
        assert.equal(foo.signature, "Foo<T>")
        assert(foo instanceof StructType)
        assert.equal(foo.fields.get("foo")?.signature, "foo<>(Foo<T>)->Foo<T>")
        assert.equal(foo.debug_str, "Foo<T>{a: T, foo: foo<>(Foo<T>)->Foo<T>}")
    },

    test_impl_with_inner_function() {
        const {env} = test.type_check(`
            struct Foo: 
                a i32 
            end

            impl Foo: 
                fn foo(self) Self: 
                    let b = true

                    fn bar() i32: 
                        self.a
                    end

                    self
                end
            end
        `)
        const foo = env.get("Foo", builtin_span)
        assert.equal(foo.signature, "Foo<>")
        assert(foo instanceof StructType)
        assert.equal(foo.fields.get("foo")?.signature, "foo<>(Foo<>)->Foo<>")
        assert.equal(foo.debug_str, "Foo<>{a: i32, foo: foo<>(Foo<>)->Foo<>}")
    },

    test_struct_instantiation() {
        const {type} = test.type_check(`
            struct Foo: 
                a i32 
            end

            let foo = Foo{a: 1}
            foo
        `)
        assert.equal(type.signature, "Foo<>")
    },

    test_struct_instantiation_with_wrong_field_type() {
        assert.throws(
            () =>
                test.type_check(`
            struct Foo: 
                a i32 
            end

            let foo = Foo{a: true}
        `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_struct_instantiation_with_generic_struct() {
        const {type, env} = test.type_check(`
            struct Foo<T>: 
                a T
            end

            impl Foo<A>: 
                fn foo(v A) A: 
                    v
                end
            end

            let foo = Foo<i32>{a: 1}
            foo
        `)
        assert.equal(type.signature, "Foo<T=i32>")
        assert(type instanceof StructType)
        assert.equal(type.debug_str, "Foo<T=i32>{a: i32, foo: foo<>(i32)->i32}")
        assert.equal(env.get("Foo", builtin_span)!.debug_str, "Foo<T>{a: T, foo: foo<>(T)->T}")
    },

    test_struct_instantiation_with_generic_struct_incompatible_type() {
        assert.throws(
            () =>
                test.type_check(`
            struct Foo<T>: 
                a T
            end

            let foo = Foo<i32>{a: 1}
            let bar Foo<bool> = foo
        `),
            /Expected `Foo<T=bool> \(StructType\)` but got `Foo<T=i32> \(StructType\)`/,
        )
    },

    test_struct_with_complex_types() {
        const {type, env} = test.type_check(`
            struct Foo<T>: 
                a T
            end

            struct Bar<T>:
                foo Foo<T>
            end

            impl Foo<A>: 
                fn do_foo(v A) A: 
                    v
                end
            end

            let bar = Bar<i32>{foo: Foo<i32>{a: 1}}
            let bar_foo = bar.foo
            let bar_foo_do_foo = bar_foo.do_foo
            bar
        `)
        assert.equal(type.signature, "Bar<T=i32>")
        assert(type instanceof StructType)
        assert.equal(type.debug_str, "Bar<T=i32>{foo: Foo<T=i32>}")
        assert.equal(env.get("Bar", builtin_span)!.debug_str, "Bar<T>{foo: Foo<T>}")
        assert.equal(
            env.get("Foo", builtin_span)!.debug_str,
            "Foo<T>{a: T, do_foo: do_foo<>(T)->T}",
        )
        assert.equal(env.get("bar_foo_do_foo", builtin_span)!.signature, "do_foo<>(i32)->i32")
    },

    test_static_struct_function() {
        const {type} = test.type_check(`
            struct Foo<T>: 
                a T
            end

            impl Foo<A>:
                fn new(a A) Foo<A>:
                    Foo<A>{a: a}
                end
            end

            let foo = Foo<i32>.new(1)
            foo
        `)
        assert.equal(type.signature, "Foo<T=i32>")
    },

    test_function_call() {
        const {type} = test.type_check(`
            fn foo(a i32, b bool) bool: 
                true
            end
            foo(1, true)
        `)
        assert.equal(type.signature, "bool")
    },

    test_function_call_generic() {
        const {type} = test.type_check(`
            fn foo<T>(a T, b bool) T: 
                a
            end
            foo<i32>(1, true)
        `)
        assert.equal(type.signature, "i32")
    },

    test_struct_function_call() {
        const {type} = test.type_check(`
            struct Foo: 
                a i32
            end

            impl Foo: 
                fn foo(self) i32: 
                    self.a
                end
            end

            let foo = Foo{a: 1}
            foo.foo()
        `)
        assert.equal(type.signature, "i32")
    },

    test_recursive_generic() {
        const {env} = test.type_check(`
            struct Foo<T>: 
                a Foo<T>
                bar Bar<T, i32>
            end

            impl Foo<A>:
                fn foo<B>(v A, b Foo<B>) Foo<B>: 
                    b
                end
            end

            struct Bar<U, V>:
                foo Foo<U>
                bar Bar<U, V>
            end
        `)
        assert.equal(
            env.get("Bar", builtin_span)!.debug_str,
            "Bar<U,V>{foo: Foo<U>, bar: Bar<U,V>}",
        )
        assert.equal(
            env.get("Foo", builtin_span)!.debug_str,
            "Foo<T>{a: Foo<T>, bar: Bar<T,V=i32>, foo: foo<B>(T,Foo<B>)->Foo<B>}",
        )
    },

    test_function_call_generic_on_generic_struct() {
        const {type} = test.type_check(`
            struct Foo<T>: 
                a T
            end

            impl Foo<A>: 
                fn foo<B>(v A, b B) B: 
                    b
                end
            end

            let foo = Foo<i32>{a: 1}
            foo.foo<bool>(1, true)

        `)
        assert.equal(type.signature, "bool")
    },

    test_trait() {
        const {env} = test.type_check(`
            trait Foo: 
                fn foo()
            end
        `)
        assert.equal(env.get("Foo", builtin_span)!.debug_str, "Foo<>{foo: foo<>()->()}")
    },

    test_trait_with_self_type() {
        const {env} = test.type_check(`
            trait Foo: 
                fn foo(self) Self

                fn bar(self) Self:
                    self.foo() -- Test accessing a field of \`self\`.
                end
            end
        `)
        assert.equal(
            env.get("Foo", builtin_span)!.debug_str,
            "Foo<>{foo: foo<>(Self)->Self, bar: bar<>(Self)->Self}",
        )
    },

    test_generic_trait() {
        const {env} = test.type_check(`
            trait Foo<T>: 
                fn foo(v T)
            end
        `)
        assert.equal(env.get("Foo", builtin_span)!.debug_str, "Foo<T>{foo: foo<>(T)->()}")
    },

    test_trait_impl() {
        const {env} = test.type_check(`
            struct Foo<A>:
                a A
            end

            trait FooTrait<B>: 
                fn foo(v B) B

                -- This is a default implementation.
                fn bar(v B):
                end
            end

            impl FooTrait<T> for Foo<T>: 
                fn foo(v T) T: 
                    v
                end
            end
        `)
        assert.equal(
            env.get("Foo", builtin_span)!.debug_str,
            "Foo<A>{a: A, foo: foo<>(A)->A, bar: bar<>(A)->()}",
        )
    },

    test_trait_impl_with_self_type() {
        const {env} = test.type_check(`
            struct Foo<A>:
                a A
            end

            trait FooTrait<B>: 
                fn foo(self, v B) Self

                fn bar(self, v B) Self:
                    self
                end
            end

            impl FooTrait<T> for Foo<T>: 
                fn foo(self, v T) Foo<T>:
                    self.bar(v)  -- Test accessing a default implementation.
                end
            end

            let a = Foo<i32>{a: 1}
            a.foo(1)
        `)
        assert.equal(
            env.get("Foo", builtin_span)!.debug_str,
            "Foo<A>{a: A, foo: foo<>(Foo<A>,A)->Foo<A>, bar: bar<>(Foo<A>,A)->Foo<A>}",
        )
    },

    test_trait_impl_missing_function() {
        assert.throws(
            () =>
                test.type_check(`
            struct Foo<A>:
                a A
            end

            trait FooTrait<B>: 
                fn foo(v B) B
            end

            impl FooTrait<T> for Foo<T>: 
            end
        `),
            /Trait function `foo` is not implemented/,
        )
    },

    test_trait_impl_accessing_function_of_another_struct() {
        test.type_check(`
            struct Foo:
                a i32
            end

            trait Bar: 
                fn bar() Foo:
                   Foo.new()
                end
            end

            impl Foo:
                fn new() Foo:
                    Foo{a: 1}
                end
            end
        `)
    },

    test_trait_impl_with_generic_struct() {
        const {env} = test.type_check(`
            trait IndexedGet<T>:
                fn get(self, i i32) T
            end

            extern:
                struct JSArray<X>:
                end

                impl JSArray<X>:
                    fn get(self, i i32) X
                end
            end

            struct array<A>:
                data JSArray<A>
            end

            impl IndexedGet<T> for array<T>:
                fn get(self, i i32) T:
                    self.data.get(i)
                end
            end
        `)
        assert.equal(
            env.get("array", builtin_span)!.debug_str,
            "array<A>{data: JSArray<A>, get: get<>(array<A>,i32)->A}",
        )
    },

    test_generic_trait_impl_with_concrete_type() {
        const {env} = test.type_check(`
            struct Foo:
                a i32
            end

            trait Trait<B>: 
                fn foo(self) B
            end

            impl Trait<Foo> for Foo: 
                fn foo(self) Foo: 
                    self
                end
            end
        `)
        assert.equal(
            env.get("Foo", builtin_span)!.debug_str,
            "Foo<>{a: i32, foo: foo<>(Foo<>)->Foo<>}",
        )
    },

    test_if() {
        const {type} = test.type_check(`
            if true => 1 else => 2
        `)
        // The expression is "used" because it's the last expression in the block.
        assert.equal(type.signature, "i32")
    },

    test_if_with_condition_type_mismatch() {
        assert.throws(
            () => test.type_check("if 1 => 1 else => 2"),
            /Expected `bool \(BoolType\)` but got `i32 \(NumericType\)`/,
        )
    },

    test_if_with_branch_type_mismatch() {
        assert.throws(
            () => test.type_check("let a = if true => 1 else => true"),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_if_with_branch_type_mismatch_when_used_as_condition_in_if() {
        assert.throws(
            () => test.type_check("if (if true => 1 else => true) => 1 else => 2"),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_if_with_branch_type_mismatch_when_used_in_function_call() {
        assert.throws(
            () => test.type_check("fn foo(a i32): end foo(if true => 1 else => true)"),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_if_with_branch_type_mismatch_when_used_in_struct_instantiation() {
        assert.throws(
            () =>
                test.type_check(`
            struct Foo: a i32 end
            Foo{a: if true => 1 else => true}
        `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_if_with_branch_type_mismatch_when_it_is_the_result_of_a_block() {
        assert.throws(
            () =>
                test.type_check(`
            fn foo() i32:
                if true => 1 else => true
            end
        `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_if_with_nested_branches() {
        const {type} = test.type_check(`
            if true:
                if true => 1 else => 2 
            else => 2
        `)
        assert.equal(type.signature, "i32")
    },

    test_if_branch_types_do_not_have_to_match_if_expression_is_not_used() {
        const {type, env} = test.type_check(`
            if true => 1 else => true
            1 -- We add this expression so the if expression is 
              -- not the last expression in the block.
        `)
        assert.equal(type, env.i32)
    },

    test_return_without_value() {
        const {type} = test.type_check(`
            fn foo(): 
                return
            end
            foo()
        `)
        assert.equal(type, Type.unit)
    },

    test_return_with_value() {
        const {type, env} = test.type_check(`
            fn foo() i32: 
                return 1
            end
            foo()
        `)
        assert.equal(type, env.i32)
    },

    test_nested_return() {
        const {type, env} = test.type_check(`
            fn foo(b bool) i32: 
                if b => return 1 else => return 2
            end
                
            foo(true)
        `)
        assert.equal(type, env.i32)
    },

    test_nested_return_with_only_one_branch_actually_returning() {
        const {type, env} = test.type_check(`
            fn foo(b bool) i32: 
                if b => return 1 else => 2
            end
                
            foo(true)
        `)
        assert.equal(type, env.i32)
    },

    test_nested_return_must_return_same_type() {
        assert.throws(
            () =>
                test.type_check(`
            fn foo(b bool) i32: 
                if b => return 1 else => return true
            end
                
            foo(true)
        `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_return_in_nested_function() {
        const {type, env} = test.type_check(`
            fn foo(b bool) i32: 
                fn bar() bool: 
                    return true
                end
                if bar() => return 1 else => return 2
            end
                
            foo(true)
        `)
        assert.equal(type, env.i32)
    },

    test_if_with_branch_type_mismatch_when_used_in_return() {
        assert.throws(
            () =>
                test.type_check(`
            fn foo() i32:
                return if true => 1 else => true
            end
        `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    expect_binary(expression: string, expected: "bool" | "i32") {
        const {type, env} = test.type_check_with_core_types(expression)
        assert.equal(type, env.get(expected, builtin_span))
    },

    test_binary_and_or() {
        test.expect_binary("true and false", "bool")
        test.expect_binary("true or false", "bool")
        test.expect_binary("true and false or true", "bool")
    },

    test_binary_and_or_with_type_mismatch() {
        assert.throws(
            () => test.type_check_with_core_types("true or 1"),
            /Expected `bool \(BoolType\)` but got `i32 \(NumericType\)`/,
        )
        assert.throws(
            () => test.type_check_with_core_types("1 or true"),
            /Expected `bool \(BoolType\)` but got `i32 \(NumericType\)`/,
        )
    },

    test_binary_comparison_i32() {
        test.expect_binary("1 == 1", "bool")
        test.expect_binary("1 != 1", "bool")
        test.expect_binary("1 < 1", "bool")
        test.expect_binary("1 <= 1", "bool")
        test.expect_binary("1 > 1", "bool")
        test.expect_binary("1 >= 1", "bool")
    },

    test_binary_comparison_i32_with_type_mismatch() {
        assert.throws(
            () => test.type_check_with_core_types("1 == true"),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
        assert.throws(
            () => test.type_check_with_core_types("true == 1"),
            /Expected `bool \(BoolType\)` but got `i32 \(NumericType\)`/,
        )
    },

    test_binary_comparison_bool() {
        test.expect_binary("true == true", "bool")
        test.expect_binary("true != true", "bool")
        test.expect_binary("true < true", "bool")
        test.expect_binary("true <= true", "bool")
        test.expect_binary("true > true", "bool")
        test.expect_binary("true >= true", "bool")
    },

    test_binary_comparison_struct() {
        const {type, env} = test.type_check_with_core_types(`
            struct Foo: 
                a i32
            end

            impl PartialEq for Foo: 
                fn eq(self, rhs Self) bool: 
                    self.a == rhs.a
                end

                fn ne(self, rhs Self) bool:
                    self.a != rhs.a
                end
            end

            let a = Foo{a: 1}
            let b = Foo{a: 1}
            a == b
        `)
        assert.equal(type, env.bool)
    },

    test_binary_math_i32() {
        test.expect_binary("1 + 1", "i32")
        test.expect_binary("1 - 1", "i32")
        test.expect_binary("1 * 1", "i32")
        test.expect_binary("1 / 1", "i32")
    },

    test_assignment() {
        const {type, env} = test.type_check(`
            mut a = 1
            a = 2
        `)
        assert.equal(type, env.unit)
        assert.equal(env.get("a", builtin_span), env.i32)
    },

    test_assignment_with_type_mismatch() {
        assert.throws(
            () =>
                test.type_check(`
            mut a = 1
            a = true
        `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_assignment_cannot_be_used_as_an_expression() {
        assert.throws(
            () =>
                test.type_check(`
            mut a = 1
            let b = a = 2
        `),
            /Cannot assign the unit value to a variable/,
        )
    },

    test_struct_field_assignment() {
        const {type, env} = test.type_check(`
            struct Foo: 
                a i32
            end

            mut foo = Foo{a: 1}
            foo.a = 2
        `)
        assert.equal(type, env.unit)
    },

    test_extern_block() {
        const {env} = test.type_check(`
            trait A:
                fn foo()
            end

            extern:
                fn foo(a i32, b bool) bool

                struct Foo:
                    a i32
                end

                impl A for Foo

                impl Foo:
                    fn bar() i32
                end
            end

            let a = foo(1, true)
            let b = Foo{a: 1}
            let c = b.foo
            let d = b.bar()
        `)
        assert.equal(env.get("a", builtin_span), env.bool)
        assert.equal(env.get("b", builtin_span).signature, "Foo<>")
        assert.equal(env.get("c", builtin_span).signature, "foo<>()->()")
        assert.equal(env.get("d", builtin_span), env.i32)
    },

    test_tuples() {
        const {env} = test.type_check(`
            let a = (1, true)
            let b = a.0
            let c = a.1
        `)
        assert.equal(env.get("a", builtin_span).signature, "(i32,bool)")
        assert.equal(env.get("b", builtin_span), env.i32)
        assert.equal(env.get("c", builtin_span), env.bool)
    },

    test_nested_tuples() {
        const {env} = test.type_check(`
            let a = (1, (true, 1))
            let b = a.0
            let c = a.1.0
            let d = a.1.1
        `)
        assert.equal(env.get("a", builtin_span).signature, "(i32,(bool,i32))")
        assert.equal(env.get("b", builtin_span), env.i32)
        assert.equal(env.get("c", builtin_span), env.bool)
        assert.equal(env.get("d", builtin_span), env.i32)
    },

    test_tuples_as_function_parameters() {
        const {env} = test.type_check(`
            fn foo(a (i32, bool)): 
            end

            foo((1, true))
        `)
        assert.equal(env.get("foo", builtin_span).signature, "foo<>((i32,bool))->()")
    },

    test_nested_tuples_as_function_parameters() {
        const {env} = test.type_check(`
            fn foo(a (i32, (bool, i32))): 
            end

            foo((1, (true, 1)))
        `)
        assert.equal(env.get("foo", builtin_span).signature, "foo<>((i32,(bool,i32)))->()")
    },

    test_tuples_as_function_return_type() {
        const {env} = test.type_check(`
            fn foo() (i32, bool): 
                (1, true)
            end

            let a = foo()
            let b = a.0
            let c = a.1
        `)
        assert.equal(env.get("foo", builtin_span).signature, "foo<>()->(i32,bool)")
        assert.equal(env.get("a", builtin_span).signature, "(i32,bool)")
        assert.equal(env.get("b", builtin_span), env.i32)
        assert.equal(env.get("c", builtin_span), env.bool)
    },

    test_tuples_as_function_return_type_with_nested_tuples() {
        const {env} = test.type_check(`
            fn foo() (i32, (bool, i32)): 
                (1, (true, 1))
            end

            let a = foo()
            let b = a.0
            let c = a.1.0
            let d = a.1.1
        `)
        assert.equal(env.get("foo", builtin_span).signature, "foo<>()->(i32,(bool,i32))")
        assert.equal(env.get("a", builtin_span).signature, "(i32,(bool,i32))")
        assert.equal(env.get("b", builtin_span), env.i32)
        assert.equal(env.get("c", builtin_span), env.bool)
        assert.equal(env.get("d", builtin_span), env.i32)
    },

    test_tuple_as_type_parameter() {
        const {type} = test.type_check(` 
            struct Foo<T>:
                a T
            end

            let foo = Foo<(i32, bool)>{a: (1, true)}
            foo
        `)
        assert.equal(type.signature, "Foo<T=(i32,bool)>")
    },

    test_enum() {
        const {env} = test.type_check(`
            enum Foo:
                Bar
                Baz(i32)
            end

            fn foo(a Foo):
            end

            foo(Foo.Bar)

            let a = Foo.Bar
            let b = Foo.Baz(1)
        `)
        const enum_type = env.get("Foo", builtin_span)
        assert.equal(enum_type.signature, "Foo<>")
        assert.equal(enum_type.debug_str, "Foo<>{Bar: (), Baz: (i32)}")
        assert.equal(env.get("a", builtin_span).signature, "Foo<>.Bar")
        assert.equal(env.get("b", builtin_span).signature, "Foo<>.Baz")
    },

    test_enum_with_generic_type() {
        const {env} = test.type_check(`
            enum Foo<T>:
                Bar
                Baz(T)
            end

            fn foo(a Foo<i32>):
            end

            let a = Foo<i32>.Bar
            let b = Foo<i32>.Baz(1)
            foo(Foo<i32>.Baz(1))
        `)
        const enum_type = env.get("Foo", builtin_span)
        assert.equal(enum_type.signature, "Foo<T>")
        assert.equal(enum_type.debug_str, "Foo<T>{Bar: (), Baz: (T)}")
        assert.equal(env.get("a", builtin_span).signature, "Foo<T=i32>.Bar")
        assert.equal(env.get("b", builtin_span).signature, "Foo<T=i32>.Baz")
    },

    test_enum_with_generic_type_and_type_mismatch() {
        assert.throws(
            () =>
                test.type_check(`
                    enum Foo<T>: 
                        Baz(T)
                    end

                    Foo<i32>.Baz(true)
                `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_enum_type_cannot_be_assigned_in_variable_declaration() {
        assert.throws(
            () =>
                test.type_check(`
                    enum Foo:
                    end
                    let a = Foo
                `),
            /Cannot assign enum type/,
        )
    },

    test_enum_type_cannot_be_assigned() {
        assert.throws(
            () =>
                test.type_check(`
                    enum Foo:
                        Bar
                    end
                    mut a = Foo.Bar
                    a = Foo
                `),
            /Cannot assign enum type/,
        )
    },

    test_enum_variant_with_fields_must_be_instantiated() {
        assert.throws(
            () =>
                test.type_check(`
                    enum Foo:
                        Bar(i32)
                    end
                    let a = Foo.Bar
                `),
            /Enum variant `Foo<>.Bar` must be instantiated/,
        )
    },

    test_enum_impl() {
        const {env} = test.type_check(`
            enum Foo:
                Bar
                Baz(i32)
            end

            impl Foo:
                fn foo(self) i32:
                    match self:
                        Foo.Bar => 1
                        Foo.Baz(a) => a
                    end
                end
            end

            let a = Foo.Bar.foo
            let b = Foo.Baz(1).foo
        `)
        const enum_type = env.get("Foo", builtin_span)
        assert.equal(enum_type.signature, "Foo<>")
        assert.equal(enum_type.debug_str, "Foo<>{Bar: (), Baz: (i32), foo: foo<>(Foo<>)->i32}")
        assert.equal(env.get("a", builtin_span).signature, "foo<>(Foo<>)->i32")
        assert.equal(env.get("b", builtin_span).signature, "foo<>(Foo<>)->i32")
    },

    test_function_type_as_function_parameter() {
        const {env} = test.type_check(`
            fn foo(a (fn(i32, bool) bool)): 
            end

            fn bar(a i32, b bool) bool:
                b
            end

            foo(bar)

        `)
        assert.equal(env.get("foo", builtin_span).signature, "foo<>(fn<>(i32,bool)->bool)->()")
    },

    test_function_type_as_parameter_type_mismatch() {
        assert.throws(
            () =>
                test.type_check(`
                    fn foo(a (fn(i32, bool) bool)): 
                    end

                    fn bar(a bool, b i32) bool:
                        a
                    end

                    foo(bar)
                `),
            /Expected `fn<>\(i32,bool\)->bool \(FunctionType\)` but got `bar<>\(bool,i32\)->bool \(FunctionType\)/,
        )
    },

    test_function_type_as_function_return_type() {
        const {env, type} = test.type_check(`
            fn foo() (fn(i32, bool) bool): 
                fn bar(a i32, b bool) bool:
                    b
                end
                bar
            end

            let a = foo()
            a(1, true)
        `)
        assert.equal(type, env.bool)
        assert.equal(env.get("foo", builtin_span).signature, "foo<>()->fn<>(i32,bool)->bool")
        assert.equal(env.get("a", builtin_span).signature, "fn<>(i32,bool)->bool")
    },

    test_function_type_as_function_return_type_mismatch() {
        assert.throws(
            () =>
                test.type_check(`
                    fn foo() (fn(i32, bool) bool): 
                        fn bar(a bool, b i32) bool:
                            a
                        end
                        bar
                    end

                    let a = foo()
                    a(1, true)
                `),
            /Expected `fn<>\(i32,bool\)->bool \(FunctionType\)` but got `bar<>\(bool,i32\)->bool \(FunctionType\)/,
        )
    },

    test_loop() {
        const {type} = test.type_check(`
            loop:
                break
            end
        `)
        assert.equal(type, Type.unit)
    },

    test_loop_with_continue() {
        const {type} = test.type_check(`
            let a = true
            loop:
                if a => continue
                break
            end
        `)
        assert.equal(type, Type.unit)
    },

    test_break_outside_of_loop() {
        assert.throws(() => test.type_check("break"), /`break` can only be used inside a loop/)
    },

    test_continue_outside_of_loop() {
        assert.throws(
            () => test.type_check("continue"),
            /`continue` can only be used inside a loop/,
        )
    },

    test_match_primitives() {
        const {type, env} = test.type_check(`
            match 1:
                1 => true
                2 => false
            end
        `)
        assert.equal(type, env.bool)
    },

    test_match_struct() {
        const {type, env} = test.type_check(`
            struct Foo: 
                a i32
            end

            match Foo{a: 1}:
                Foo{a: 1} => true
                Foo{a: 2} => false
            end
        `)
        assert.equal(type, env.bool)
    },

    test_match_nested_struct() {
        const {type, env} = test.type_check(`
            struct Foo: 
                a i32
            end

            struct Bar:
                foo Foo
            end

            match Bar{foo: Foo{a: 1}}:
                Bar{foo: Foo{a: 1}} => true
                Bar{foo: Foo{a: 2}} => false
            end
        `)
        assert.equal(type, env.bool)
    },

    test_match_struct_with_generic_type() {
        const {type, env} = test.type_check(`
            struct Foo<T>: 
                a T
            end

            match Foo<i32>{a: 1}:
                Foo<i32>{a: 1} => true
                Foo<i32>{a: 2} => false
            end
        `)
        assert.equal(type, env.bool)
    },

    test_match_struct_with_generic_type_and_type_mismatch() {
        assert.throws(
            () =>
                test.type_check(`
                    struct Foo<T>: 
                        a T
                    end

                    match Foo<i32>{a: 1}:
                        Foo<i32>{a: 1} => true
                        Foo<i32>{a: true} => false
                    end
                `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_match_struct_with_generic_type_and_type_mismatch_in_pattern() {
        assert.throws(
            () =>
                test.type_check(`
                    struct Foo<T>: 
                        a T
                    end

                    match Foo<i32>{a: 1}:
                        Foo<i32>{a: 1} => true
                        Foo<i32>{a: true} => false
                    end
                `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_match_enum_without_fields() {
        const {type, env} = test.type_check(`
            enum Foo:
                Bar
                Baz
            end

            match Foo.Bar:
                Foo.Bar => true
                Foo.Baz => false
            end
        `)
        assert.equal(type, env.bool)
    },

    test_match_enum_with_generic_type() {
        const {type, env} = test.type_check(`
            enum Foo<T>:
                Bar
                Baz(T)
            end

            match Foo<i32>.Bar:
                Foo<i32>.Bar => true
                Foo<i32>.Baz(1) => false
            end
        `)
        assert.equal(type, env.bool)
    },

    test_match_enum_with_generic_type_on_parameterized_variant() {
        const {type, env} = test.type_check(`
            enum Foo<T>:
                Bar
                Baz(T)
            end

            match Foo<i32>.Baz(1): -- This is the difference from the previous test.
                Foo<i32>.Bar => true
                Foo<i32>.Baz(1) => false
            end
        `)
        assert.equal(type, env.bool)
    },

    test_match_enum_with_generic_type_and_type_mismatch() {
        assert.throws(
            () =>
                test.type_check(`
                    enum Foo<T>:
                        Bar
                        Baz(T)
                    end

                    match Foo<i32>.Bar:
                        Foo<i32>.Bar => true
                        Foo<i32>.Baz(true) => false
                    end
                `),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
    },

    test_wildcard_pattern() {
        const {type, env} = test.type_check(`
            match 1:
                _ => true
            end
        `)
        assert.equal(type, env.bool)
    },

    test_wildcard_pattern_with_complex_match() {
        const {type, env} = test.type_check(`
            struct BazStruct<T>:
                baz T
            end

            enum Foo<T>:
                Bar
                Baz(BazStruct<T>)
            end

            struct FooStruct<T>:
                foo Foo<T>
            end

            match FooStruct<i32>{foo: Foo<i32>.Baz(BazStruct<i32>{baz: 1})}:
                FooStruct<i32>{foo: _} => true
                FooStruct<i32>{foo: Foo<i32>.Baz(BazStruct<i32>{baz: _})} => false
                _ => false
            end
        `)
        assert.equal(type, env.bool)
    },

    test_match_with_variable_capture() {
        const {type, env} = test.type_check(`
            match 1:
                a => a
            end
        `)
        assert.equal(type, env.i32)
    },

    test_interpolated_string() {
        const {type, env} = test.type_check_with_core_types(`
            let a = 1
            f"foo: {a}"
        `)
        assert.equal(type, env.str)
    },

    test_interpolated_string_expressions_must_implement_to_str() {
        assert.throws(
            () =>
                test.type_check_with_core_types(`
                    struct Foo: end
                    let foo = Foo{}
                    f"foo: {foo}"
                `),
            /Expected `Foo<>` to implement `ToStr<>`/,
        )
    },

    test_not() {
        const {type, env} = test.type_check(`
            let a = true
            not a
        `)
        assert.equal(type, env.bool)
    },

    test_not_only_works_on_bool() {
        assert.throws(
            () => test.type_check("not 1"),
            /Expected `bool \(BoolType\)` but got `i32 \(NumericType\)`/,
        )
    },

    test_array_literal() {
        const {type} = test.type_check_with_core_types(`
            let a = [1, 2, 3]
            a
        `)
        assert.equal(type.signature, "Array<T=i32>")
    },

    test_indexed_access() {
        const {type, env} = test.type_check_with_core_types(`
            struct Foo:
                a1 i32
                a2 i32
                a3 i32
            end

            impl IndexedGet<i32, i32> for Foo:
                fn get(self, i i32) i32 =>
                    match i:
                        0 => self.a1
                        1 => self.a2
                        2 => self.a3
                    end
            end

            impl IndexedSet<i32, i32> for Foo:
                fn set(mut self, i i32, v i32):
                    match i:
                        0 => self.a1 = v
                        1 => self.a2 = v
                        2 => self.a3 = v
                    end
                end
            end

            let foo = Foo{a1: 1, a2: 2, a3: 3}
            let a = foo[0]
            foo[1] = 4
        `)
        assert.equal(type, env.unit)
    },

    test_indexed_access_on_generic_type() {
        const {type, env} = test.type_check_with_core_types(`
            enum Option<T>:
                Some(T)
                None
            end

            struct Map<K, V>:
                k1 K
                k2 K
                v1 V
                v2 V
            end

            impl IndexedGet<K, Option<V>> for Map<K, V>:
                fn get(self, k K) Option<V>:
                    match k:
                        self.k1 => Option<V>.Some(self.v1)
                        self.k2 => Option<V>.Some(self.v2)
                        _ => Option<V>.None
                    end
                end
            end

            impl IndexedSet<K, V> for Map<K, V>:
                fn set(mut self, k K, v V):
                    match k:
                        self.k1 => self.v1 = v
                        self.k2 => self.v2 = v
                        _ => panic("Key not found")
                    end
                end
            end

            let map = Map<str, i32>{k1: "a", k2: "b", v1: 1, v2: 2}
            map["a"] = 3
            map["a"]
        `)
        const map_type = env.get("map", builtin_span)
        assert(map_type instanceof ComplexType)
        assert.equal(map_type.signature, "Map<K=str,V=i32>")
        assert.equal(
            map_type.field("get", builtin_span)!.signature,
            "get<>(Map<K=str,V=i32>,str)->Option<V=i32>",
        )
        assert.equal(type.signature, "Option<V=i32>")
    },

    test_indexed_access_on_generic_type_with_concrete_type() {
        const {type} = test.type_check_with_core_types(`
            struct Foo<T>:
                v T
            end

            impl IndexedGet<i32, T> for Foo<T>:
                fn get(self, i i32) T => self.v
            end

            Foo
        `)
        assert.equal(type.signature, "Foo<T>")
    },

    test_concise_error_handling_with_return() {
        const {env} = test.type_check_with_core_types(`
            fn divide(dividend i32, divisor i32) i32 throws:
                if divisor == 0 => return Error {message: "Division by zero", data: ()}
                return dividend / divisor
            end

            let ok = divide(10, 2)
            let err = divide(10, 0)
        `)
        assert.equal(env.get("ok", builtin_span).signature, "Result<T=i32,E default ()=()>")
        assert.equal(env.get("err", builtin_span).signature, "Result<T=i32,E default ()=()>")
    },

    test_concise_error_handling_with_nested_expression() {
        const {env} = test.type_check_with_core_types(`
            fn divide(dividend i32, divisor i32) i32 throws:
                if divisor == 0 => return Error {message: "Division by zero", data: ()}
                if divisor > 10 => dividend / divisor else => 0
            end

            let ok = divide(10, 2)
            let err = divide(10, 0)
        `)
        assert.equal(env.get("ok", builtin_span).signature, "Result<T=i32,E default ()=()>")
        assert.equal(env.get("err", builtin_span).signature, "Result<T=i32,E default ()=()>")
    },

    test_concise_error_handling_with_result_type() {
        const {env} = test.type_check_with_core_types(`
            fn divide(dividend i32, divisor i32) Result<i32>:
                if divisor == 0 
                    -- Explicit return is tested here.
                    => return Result<i32>.Err(Error {message: "Division by zero", data: ()})
                -- Implicit return is tested here.
                Result<i32>.Ok(dividend / divisor)
            end

            let ok = divide(10, 2)
            let err = divide(10, 0)
        `)
        assert.equal(env.get("ok", builtin_span).signature, "Result<T=i32,E default ()=()>")
        assert.equal(env.get("err", builtin_span).signature, "Result<T=i32,E default ()=()>")
    },

    test_error_propagation() {
        const {env} = test.type_check_with_core_types(`
            fn divide(dividend i32, divisor i32) i32 throws:
                if divisor == 0 => return Error {message: "Division by zero", data: ()}
                dividend / divisor
            end

            fn divide_10_by(n i32) i32 throws:
                let result = divide(10, n)!
                if result == 1 => return Error {message: "We did not expect 1", data: ()}
                result
            end

            let ok = divide_10_by(2)
            let err = divide_10_by(0)
        `)
        assert.equal(env.get("ok", builtin_span).signature, "Result<T=i32,E default ()=()>")
        assert.equal(env.get("err", builtin_span).signature, "Result<T=i32,E default ()=()>")
    },

    test_panic_call_is_ignored_in_function_return() {
        const {type, env} = test.type_check_with_core_types(`
            fn foo() i32: 
                panic("foo") -- This does not violate the return type.
            end
            foo()
        `)
        assert.equal(type, env.i32)
    },

    test_panic_call_is_ignored_in_match() {
        const {type, env} = test.type_check_with_core_types(`
            match 1:
                1 => panic("foo") -- This does not violate the match type.
                2 => 2
            end
        `)
        assert.equal(type, env.i32)
    },

    test_semicolon_transforms_expression_into_statement() {
        const {type, env} = test.type_check(`
            true;
        `)
        assert.equal(type, env.unit)
    },

    test_semicolon_has_the_highest_precedence() {
        const {type, env} = test.type_check_with_core_types(`
            1 == 2;
        `)
        assert.equal(type, env.unit)
    },
}

if (Bun.argv[1].endsWith("type_check2.ts")) {
    const filter = Bun.argv[2]
    for (const [name, fn] of Object.entries(test).filter(([name]) => name.startsWith("test"))) {
        if (filter && !name.includes(filter)) {
            continue
        }
        try {
            // @ts-ignore
            fn()
            console.log("PASS", name)
        } catch (e) {
            console.error("FAIL", name)
            console.error(e)
            break
        }
    }
}
