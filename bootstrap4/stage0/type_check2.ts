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
            if (type.data.variant_tuple_type!.fields.size > 0 && !ctx.parent_is_function_call) {
                throw new TypeCheckError(
                    `Enum variant ${quote(type.signature)} must be instantiated`,
                    node.span,
                )
            }
        }
    } else if (node instanceof ast.Return) {
        if (node.value) {
            const return_type = type_check(node.value, env, {...ctx, used_in_expression: true})
            if (!ctx.return_type) {
                throw new SemanticError("Unexpected return", node.span)
            }
            expect_equal_types(ctx.return_type, return_type, node.span)
            // `return` is a statement, but we give it the type because it makes it easier
            // to type-check nested blocks like:
            //     fn foo(): i32
            //         if bar => return 1 else => return 2
            //     end
            type = return_type
        } else {
            type = Type.unit
        }
    } else if (node instanceof ast.BinaryExpression) {
        type = binary_expression(node, env, ctx)
    } else if (node instanceof ast.FunctionDefinition) {
        const function_type = FunctionType.from_declaration(node.declaration, env)
        type_check_function_body(node, function_type, env)
        type = Type.unit
    } else if (node instanceof ast.Assignment) {
        const target_type = type_check(node.target, env, {...ctx, used_in_expression: true})
        const value_type = type_check(node.value, env, {...ctx, used_in_expression: true})
        expect_type_can_be_used_in_assignments(value_type, env, node.span)
        expect_assignable_to(target_type, value_type, node.span)
        type = value_type
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
            const type_parameters = parse_type_arguments(node.type_parameters, env, node.span)
            type = type.with_type_arguments(type_parameters, node.span)
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
    } else {
        throw new TypeCheckError(
            `Not implemented for node type ${quote(node.constructor.name)}`,
            node.span,
        )
    }
    node.attributes.type = type
    return type
}

function expect_equal_types(expected: Type, got: Type, span: Span) {
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
    if (expected.assignable_to(got)) {
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
        const declared_type = env.get(node.type.name, node.span)
        expect_equal_types(declared_type, value_type, node.span)
    }
    expect_type_can_be_used_in_assignments(value_type, env, node.span)
    env.add(node.name, value_type, node.span)
}

function binary_expression(node: ast.BinaryExpression, env: TypeEnvironment, ctx: Context): Type {
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
        const variant_type = target.data.variant_tuple_type!
        if (variant_type.fields.size === 0) {
            throw new TypeCheckError(`Enum variant ${quote(target.name)} has no fields`, node.span)
        }
        if (variant_type.fields.size !== node.args.length) {
            throw new TypeCheckError(
                `Expected ${target.fields.size} arguments but got ${node.args.length}`,
                node.span,
            )
        }
        // for (let i = 0; i < node.args.length; i++) {
        //     const arg_type = type_check(node.args[i], env, {used_in_expression: true})
        //     expect_assignable_to(target.fields[i], arg_type, env, node.span)
        // }
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
    const type_arguments = parse_type_arguments(node.type_arguments, env, node.span)
    function_type = function_type.with_type_arguments(type_arguments, node.span)
    parameters = function_type.parameters_without_self
    for (let i = 0; i < node.args.length; i++) {
        const arg_type = type_check(node.args[i], env, {used_in_expression: true})
        expect_assignable_to(parameters[i], arg_type, node.span)
    }
    return function_type.return_type
}

function struct_instantiation(node: ast.StructInstantiation, env: TypeEnvironment): StructType {
    const type_arguments = parse_type_arguments(node.type_arguments, env, node.span)
    let struct_type = StructType.from_env(node.target_struct_name, env, node.span)
    struct_type = struct_type.with_type_arguments(type_arguments, node.span)
    for (const [name, value] of Object.entries(node.fields)) {
        const field_type = struct_type.field(name, node.span)
        const value_type = type_check(value, env, {used_in_expression: true})
        expect_equal_types(field_type, value_type, node.span)
    }
    return struct_type
}

function parse_type_arguments(
    type_arguments: ast.TypeDeclaration[],
    env: TypeEnvironment,
    span: Span,
) {
    const result = []
    for (const p of type_arguments) {
        const type_argument = TypeParameter.from_declaration(p, env)
        result.push(env.get(type_argument.signature, span))
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
            for (const extern_node of node.contained_nodes()) {
                forward_declare(extern_node, true)
            }
        } else {
            throw new TypeCheckError(
                `Not implemented for node type ${quote(node.constructor.name)}`,
                node.span,
            )
        }
    }

    for (const node of nodes) {
        forward_declare(node, false)
    }

    for (const node of struct_declarations) {
        type_check_struct_fields(node, env)
    }

    for (const node of enum_declarations) {
        type_check_enum_variants(node, env)
    }

    for (const node of trait_declarations) {
        type_check_trait_functions(node, env, "default_impls")
        type_check_trait_functions(node, env, "signatures")
    }

    for (const impl of impl_definitions) {
        type_check_impl(impl.node, env, {extern: impl.extern})
    }
}

function type_check_struct_fields(node: ast.StructDeclaration, env: TypeEnvironment) {
    const struct_type = StructType.from_env(node.name, env, node.span)
    const struct_env = struct_type.create_type_environment(env, node.span)
    for (const [name, type] of Object.entries(node.fields)) {
        let field_type = struct_env.get(type.name, type.span)
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
    node: ast.FunctionDefinition,
    function_type: FunctionType,
    env: TypeEnvironment,
) {
    const function_env = new TypeEnvironment(env)
    for (const name of function_type.parameter_names) {
        const type = function_type.parameter(name, node.span)
        function_env.add(name, type, node.span)
    }
    const block_type = type_check(node.block, function_env, {
        return_type: function_type.return_type,
        used_in_expression: true,
    })
    if (node.block.body.at(-1) instanceof ast.Return) {
        // The last expression is a return statement, so we don't need to check the return type.
        // We already checked that the return type is correct in the return statement.
    } else {
        expect_assignable_to(function_type.return_type, block_type, node.span)
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
        if (trait_type.data.fields.has(function_type.name)) {
            throw new TypeCheckError(
                `Field ${quote(function_type.name)} already defined in trait ${quote(
                    trait_type.signature,
                )}`,
                func.span,
            )
        }
        trait_type.data.fields.set(function_type.name, function_type)
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
        const function_type = trait_type.data.fields.get(func.name)!
        assert(function_type instanceof FunctionType)
        type_check_function_body(func, function_type, trait_env)
    }
}

function type_check_impl(node: ast.ImplDefinition, env: TypeEnvironment, opts: {extern: boolean}) {
    const complex_type = ComplexType.from_env(node.target_name, env, node.span)
    if (complex_type instanceof TraitType || complex_type instanceof FunctionType) {
        throw new TypeCheckError(
            `Cannot implement a function or trait type ${quote(complex_type.signature)}`,
            node.span,
        )
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
    const function_definitions: ast.FunctionDefinition[] = []
    for (const func of node.functions) {
        const declaration = func instanceof ast.FunctionDeclaration ? func : func.declaration
        const function_type = FunctionType.from_declaration(declaration, impl_env)
        if (complex_type.data.fields.has(function_type.name)) {
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
        if (func instanceof ast.FunctionDefinition) {
            function_definitions.push(func)
        }
    }
    if (node.trait_name) {
        complex_type.data.traits.push(node.trait_name)
        let trait_type = TraitType.from_env(node.trait_name, impl_env, node.span)
        const type_variable_map = TypeVariable.create_type_variable_map(
            trait_type.type_variables,
            complex_type.type_variables,
        )
        type_variable_map.set(TypeVariable.Self, complex_type)
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

    /**
     * Some types like tuples are `inline defined` and don't have a name.
     * All other types are looked up in the environment.
     */
    static from_env_or_declaration(declaration: ast.TypeDeclaration, env: TypeEnvironment): Type {
        if (declaration instanceof ast.TupleTypeDeclaration) {
            const type = TupleType.from_declaration(declaration, env, declaration.span)
            return type
        }
        return env.get(declaration.name, declaration.span)
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

    with_type_arguments(_type_arguments: Type[], _span: Span): Type {
        throw new Error(`Cannot add type arguments to ${quote(this.signature)}`)
    }

    equals(other: Type): boolean {
        return this === other
    }

    assignable_to(other: Type): boolean {
        return this.equals(other)
    }
}

class TypeVariable extends Type {
    static Self = new TypeVariable("Self")
    static from_declaration(declaration: ast.TypeDeclaration, env: TypeEnvironment): TypeVariable {
        let type = env.get_or_null(declaration.name)
        if (type) {
            if (!(type instanceof TypeVariable)) {
                throw new TypeCheckError(
                    `Expected type variable but got ${quote(type.signature)}`,
                    declaration.span,
                )
            }
            return type
        }
        return TypeVariable.from_string(declaration.name, declaration.span)
    }

    static from_string(name: string, span: Span) {
        const type = new TypeVariable(name)
        return Type.add_or_get_known_type(name, type, span)
    }

    static create_type_variable_map(
        type_variables: TypeVariable[],
        type_arguments: (Type | null)[],
    ) {
        const map = new Map<TypeVariable, Type>()
        for (let i = 0; i < type_variables.length; i++) {
            const type_variable = type_variables[i]
            const type_argument = type_arguments[i]
            if (type_argument) {
                map.set(type_variable, type_argument)
            }
        }
        return map
    }

    private constructor(name: string) {
        super(name)
    }

    resolve_type_variables(map: Map<TypeVariable, Type>): Type {
        return map.get(this) ?? this
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
        let type = env.get_or_null(declaration.name)
        if (!type) {
            // The type is unknown, we treat this as a type variable.
            type = TypeVariable.from_string(declaration.name, declaration.span)
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

    with_type_arguments(type_arguments: (Type | null)[], span: Span): this {
        if (type_arguments.length !== this.type_variables.length) {
            throw new TypeCheckError(
                `Expected ${this.type_variables.length} type arguments but got ${type_arguments.length}`,
                span,
            )
        }
        const new_type_arguments = new Map<TypeVariable, Type>(this.type_arguments)
        for (let i = 0; i < this.type_variables.length; i++) {
            const type_variable = this.type_variables[i]
            const type_argument = type_arguments[i]
            if (!type_argument) {
                throw new TypeCheckError(
                    `Missing type argument for ${quote(type_variable.signature)}`,
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
        return this.new_instance(this.data, this.type_variables, this.type_arguments, map)
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
                    type_variable_map.set(type_variable, type_parameter.type)
                } else {
                    type_arguments.set(type_variable, type_parameter.type)
                }
            }
            field_type = field_type.with_scope_type_variables(type_variable_map)
            field_type = field_type.resolve_type_variables(type_arguments) as T
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
        if (this._fields) {
            return this._fields
        }
        this._fields = new Map()
        for (let [name, type] of this.data.fields) {
            type = type.resolve_type_variables(this.type_arguments)
            if (type instanceof TypeVariable) {
                // Respect `this.type_variable_map` here.
                type = this.type_variable_map.get(type) ?? type
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
        return false
    }
}

export class StructType extends ComplexType<StructData> {
    static from_declaration(declaration: ast.StructDeclaration, env: TypeEnvironment): StructType {
        const type_variables = declaration.type_parameters.map((p) =>
            TypeVariable.from_declaration(p, env),
        )
        const struct_type = new StructType(new StructData(declaration.name), type_variables)
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
        public readonly fields: Map<string, Type> = new Map(),
        public readonly traits: string[] = [],
    ) {}
}

class EnumType extends ComplexType<EnumData> {
    static from_declaration(declaration: ast.EnumDeclaration, env: TypeEnvironment): EnumType {
        const type_variables = declaration.type_parameters.map((p) =>
            TypeVariable.from_declaration(p, env),
        )
        const enum_type = new EnumType(new EnumData(declaration.name), type_variables)
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
            new Map(),
            enum_type.data.traits,
            declaration.name,
            TupleType.from_declaration(
                declaration.fields,
                enum_type.create_type_environment(env, declaration.span),
                declaration.span,
            ),
            enum_type,
        )
        return new EnumType(
            data,
            enum_type.type_variables,
            enum_type.type_arguments,
            enum_type.type_variable_map,
        )
    }

    private constructor(
        public data: EnumData,
        type_variables: TypeVariable[],
        type_arguments?: Map<TypeVariable, Type>,
        type_variable_map?: Map<TypeVariable, TypeVariable>,
    ) {
        super(data, type_variables, type_arguments, type_variable_map)
    }

    new_instance(
        data: EnumData,
        type_variables: TypeVariable[],
        type_arguments: Map<TypeVariable, Type>,
        type_variable_map: Map<TypeVariable, TypeVariable>,
    ): this {
        return new EnumType(data, type_variables, type_arguments, type_variable_map) as this
    }

    get is_variant(): boolean {
        return !!this.data.variant_name
    }

    get variants(): Map<string, EnumType> {
        if (this.is_variant) {
            return new Map()
        }
        return this.data.fields as Map<string, EnumType>
    }

    get debug_str() {
        if (this.is_variant) {
            return `${this.signature}.${this.data.variant_name}${this.data.variant_tuple_type?.signature}`
        }
        const variants = [...this.variants.entries()].map(
            ([name, variant]) => `${name}: ${variant.data.variant_tuple_type?.signature}`,
        )
        return `${this.signature}{${variants.join(", ")}}`
    }

    get signature() {
        if (this.is_variant) {
            return `${this.data.name}${this.type_signature}.${this.data.variant_name}`
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
        const this_enum_type = this.data.variant_parent ?? this
        const other_enum_type = other.data.variant_parent ?? other
        return (
            this_enum_type.data.name === other_enum_type.data.name &&
            this_enum_type.data.fields === other_enum_type.data.fields
        )
    }
}

class EnumData {
    constructor(
        public readonly name: string,
        public readonly fields: Map<string, Type> = new Map(),
        public readonly traits: string[] = [],
        public readonly variant_name?: string,
        public readonly variant_tuple_type?: TupleType,
        public readonly variant_parent?: EnumType,
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
        const struct_data = new TraitData(declaration.name, new Map(), functions_with_default_impl)
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
        const data = new TraitData(this.data.name, new Map(), this.data.traits)
        return this.new_instance(
            data,
            this.type_variables,
            this.type_arguments,
            this.type_variable_map,
        )
    }

    get debug_str() {
        const fields = [...this.data.fields.keys()].map(
            (name) => `${name}: ${this.field(name, builtin_span).signature}`,
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
        public readonly fields: Map<string, Type> = new Map(),
        public readonly functions_with_default_impl: string[],
        public readonly traits: string[] = [],
    ) {}
}

export class FunctionType extends ComplexType<FunctionData> {
    static from_declaration(
        declaration: ast.FunctionDeclaration | ast.FunctionTypeDeclaration,
        env: TypeEnvironment,
    ): FunctionType {
        const type_variables = declaration.type_parameters.map((p) =>
            TypeVariable.from_declaration(p, env),
        )
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
            if (type instanceof ComplexType) {
                // Map type variables to the function type variables.
                const type_variable_map = TypeVariable.create_type_variable_map(
                    type.type_variables,
                    type_variables,
                )
                type = type.with_scope_type_variables(type_variable_map)
            }
            return type
        }
        const parameters =
            declaration instanceof ast.FunctionDeclaration
                ? declaration.parameters
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
        fields.set("return", return_type)
        const data = new FunctionData(
            declaration.name,
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
        for (let i = 0; i < this.parameters.length; i++) {
            if (!this.parameters[i].assignable_to(other.parameters[i])) {
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
        span: Span,
    ): TupleType {
        const type_variables = declaration.type_parameters.map((p) =>
            TypeVariable.from_declaration(p, env),
        )
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
        return this.name
    }

    get signature() {
        return this.name
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
        Type.clear_known_types()
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
            ` + src
        }
        const ast = test.parse(src)
        const env = TypeEnvironment.global()
        return {type: type_check_ast(ast, env), env}
    },

    type_check_with_core_traits(src: string) {
        return test.type_check(src, {with_core_traits: true})
    },

    test_let() {
        const {type} = test.type_check("let a = 1")
        assert.strictEqual(type, Type.unit)
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

                fn bar() i32: 
                    if b => 1 else => a
                end

                bar()
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

    test_struct() {
        const {env} = test.type_check("struct Foo: a i32 end")
        const foo = env.get("Foo", builtin_span)
        assert.equal(foo.signature, "Foo<>")
        assert(foo instanceof StructType)
        assert.deepEqual([...foo.fields.keys()], ["a"])
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
                    self.a  -- Test accessing a field of \`self\`
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

    test_impl_with_closures() {
        const {env} = test.type_check(`
            struct Foo: 
                a i32 
            end

            impl Foo: 
                fn foo(self) Self: 
                    let b = true

                    fn bar() i32: 
                        if b => 1 else => self.a
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
        const {type, env} = test.type_check_with_core_traits(expression)
        assert.equal(type, env.get(expected, builtin_span))
    },

    test_binary_and_or() {
        test.expect_binary("true and false", "bool")
        test.expect_binary("true or false", "bool")
        test.expect_binary("true and false or true", "bool")
    },

    test_binary_and_or_with_type_mismatch() {
        assert.throws(
            () => test.type_check_with_core_traits("true or 1"),
            /Expected `bool \(BoolType\)` but got `i32 \(NumericType\)`/,
        )
        assert.throws(
            () => test.type_check_with_core_traits("1 or true"),
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
            () => test.type_check_with_core_traits("1 == true"),
            /Expected `i32 \(NumericType\)` but got `bool \(BoolType\)`/,
        )
        assert.throws(
            () => test.type_check_with_core_traits("true == 1"),
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
        const {type, env} = test.type_check_with_core_traits(`
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
        assert.equal(type, env.i32)
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

    test_assignment_as_expression() {
        const {type, env} = test.type_check(`
            mut a = 1
            let b = a = 2
            b
        `)
        assert.equal(type, env.i32)
    },

    test_struct_field_assigment() {
        const {type, env} = test.type_check(`
            struct Foo: 
                a i32
            end

            mut foo = Foo{a: 1}
            foo.a = 2
        `)
        assert.equal(type, env.i32)
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
}

if (Bun.argv[1].endsWith("type_check2.ts")) {
    for (const [name, fn] of Object.entries(test).filter(([name]) => name.startsWith("test"))) {
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
