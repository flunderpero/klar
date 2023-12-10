import assert from "assert"
import {Span, quote} from "./common"
import * as ast from "./parser"

export function type_check_ast(ast: ast.AST, env: TypeEnvironment) {
    add_declarations_to_env(ast.body, env)
    for (const node of ast.body) {
        type_check(node, env)
    }
}

/**
 * Assumptions:
 *
 * - All trait implementations are hoisted to the scope of the struct they are implemented for.
 */
export function type_check(node: ast.ASTNode, env: TypeEnvironment): Type {
    function add_attrs(type: Type, more?: {target_type?: Type}) {
        node.attributes.type = type
        if (more) {
            node.attributes.target_type = more.target_type
        }
        return type
    }
    // Statements
    if (node instanceof ast.Return) {
        if (node.value) {
            return type_check(node.value, env)
        }
        return add_attrs(env.builtin_type("()"))
    }
    if (node instanceof ast.Break || node instanceof ast.Continue) {
        return add_attrs(env.builtin_type("()"))
    }
    // Literal expressions
    if (node instanceof ast.Number_) {
        return add_attrs(env.builtin_type("i32"))
    }
    if (node instanceof ast.Bool) {
        return add_attrs(env.builtin_type("bool"))
    }
    if (node instanceof ast.Str) {
        return add_attrs(env.str_type())
    }
    if (node instanceof ast.InterpolatedString) {
        for (const e of node.expressions) {
            const type = expect_structured_type(type_check(e, env), e.span)
            expect_to_impl_trait(type, "ToStr", e.span)
        }
        return add_attrs(env.str_type())
    }
    // Unary expressions
    if (node instanceof ast.ParenthesizedExpression) {
        return add_attrs(type_check(node.expression, env))
    }
    if (node instanceof ast.Not) {
        expect_type(
            type_check(node.expression, env),
            env.builtin_type("bool"),
            node.expression.span,
        )
        return add_attrs(env.builtin_type("bool"))
    }
    // Tuple and struct instantiations
    if (node instanceof ast.TupleInstantiation) {
        const types = node.elements.map((e) => type_check(e, env))
        return add_attrs(new TupleType(types, env, node.span))
    }
    if (node instanceof ast.StructInstantiation) {
        const struct_type = env.get_type({name: node.target_struct_name, span: node.span})
        if (!(struct_type instanceof StructType)) {
            throw new TypeError(
                `Expected struct type but got ${quote(struct_type.signature)}`,
                node.span,
            )
        }
        if (node.type_arguments.length !== struct_type.declaration.type_parameters.length) {
            throw new TypeError(
                `Expected ${struct_type.declaration.type_parameters.length} type arguments but got ${node.type_arguments.length}`,
                node.span,
            )
        }
        const fields = new Map<string, Type>()
        for (const [name, value] of Object.entries(node.fields)) {
            fields.set(name, type_check(value, env))
        }
        const expected_num_fields = Object.keys(struct_type.declaration.fields).length
        if (fields.size !== expected_num_fields) {
            throw new TypeError(
                `Expected ${expected_num_fields} fields but got ${fields.size}`,
                node.span,
            )
        }
        // Resolve all type parameters. 
        const instance_type = resolve_type_parameters(node, struct_type)
        return add_attrs(instance_type)
    }

    if (node instanceof ast.FunctionCall) {
        const function_type_ = expect_function_type(type_check(node.target, env), node.span)
        let function_type = function_type_
        const func_args = function_type.is_method ? function_type.args.slice(1) : function_type.args
        if (node.args.length !== func_args.length) {
            throw new TypeError(
                `Expected ${func_args.length} arguments but got ${
                    node.args.length
                } for function ${quote(function_type.signature)}`,
                node.span,
            )
        }
        const args = node.args.map((a) => type_check(a, env))
        for (let i = 0; i < args.length; i++) {
            expect_type(args[i], func_args[i], node.span)
        }
        let return_type = function_type.return_type
        return add_attrs(return_type)
    }

    if (node instanceof ast.IdentifierReference) {
        let target_type = env.get_type(node)
        if (node.type_parameters.length > 0) {
            // Create a new type if we have type parameters.
            const structured_type = expect_struct_type(target_type, node.span)
            if (
                node.type_parameters.length !== structured_type.declaration.type_parameters.length
            ) {
                throw new TypeError(
                    `Expected ${structured_type.declaration.type_parameters.length} type arguments but got ${node.type_parameters.length}`,
                    node.span,
                )
            }
            // console.log("AAA idref", target_type.signature, target_type.env.to_debug_str())
            target_type = resolve_type_parameters(node, structured_type)
        }
        return add_attrs(target_type, {target_type})
    }

    if (node instanceof ast.Assignment) {
        const target_type = type_check(node.target, env)
        const value_type = type_check(node.value, env)
        expect_type(value_type, target_type, node.span)
        return add_attrs(value_type)
    }

    if (node instanceof ast.FieldAccess) {
        const target_type = expect_structured_type(type_check(node.target, env), node.span)
        let field_type = target_type.env.get_type({name: node.field, span: node.span})
        if (!field_type) {
            throw new TypeError(`Unknown field ${quote(node.field)}`, node.span)
        }
        return add_attrs(field_type, {target_type})
    }

    if (node instanceof ast.IndexedAccess) {
        const index_type = expect_numeric_type(type_check(node.index, env), node.span)
        // TODO: type inference should look at all the usages of a variable
        //       to infer its type. This currently does not work:
        //       ```
        //       let a = [1, 2, 3]
        //       let i = 1  -- i is inferred to be i32
        //       a[i] = 4  -- this is an error because we expect usize
        //       ```
        if (index_type.is_assignable_to(env.builtin_type("usize"))) {
            throw new TypeError(`Expected usize but got ${quote(index_type.signature)}`, node.span)
        }
        const target_type = expect_structured_type(type_check(node.target, env), node.span)
        if (node.is_write) {
            expect_to_impl_trait(target_type, "IndexedSet", node.span)
            const set_function = expect_to_have_function(target_type, "set", node.span)
            return add_attrs(set_function.return_type)
        }
        expect_to_impl_trait(target_type, "IndexedGet", node.span)
        const get_function = expect_to_have_function(target_type, "get", node.span)
        return add_attrs(get_function.return_type)
    }

    if (node instanceof ast.ArrayLiteral) {
        if (node.elements.length === 0) {
            throw new TypeError(`Empty array literal`, node.span)
        }
        const element_type = type_check(node.elements[0], env)
        for (const e of node.elements) {
            expect_type(type_check(e, env), element_type, e.span)
        }
        const array_type = expect_structured_type(
            env.get_type({name: "array", span: node.span}),
            node.span,
        )
        const type = new StructuredType(array_type.name, array_type.env, node.span)
        type.traits = array_type.traits
        type.type_parameters.set_type_argument(0, element_type, node.span) // `T`
        return add_attrs(type)
    }

    if (node instanceof ast.BinaryExpression) {
        if (node.operator === "and" || node.operator === "or") {
            expect_type(type_check(node.lhs, env), env.builtin_type("bool"), node.lhs.span)
            expect_type(type_check(node.rhs, env), env.builtin_type("bool"), node.rhs.span)
            return add_attrs(env.builtin_type("bool"))
        }
        const [trait, func] = {
            "==": ["PartialEq", "eq"],
            "!=": ["Eq", "ne"],
            "<": ["PartialOrd", "lt"],
            "<=": ["PartialOrd", "le"],
            ">": ["PartialOrd", "gt"],
            ">=": ["PartialOrd", "ge"],
            "+": ["Add", "add"],
            "-": ["Sub", "sub"],
            "*": ["Mul", "mul"],
            "/": ["Div", "div"],
        }[node.operator]
        if (!trait) {
            throw new TypeError(`Unknown operator ${quote(node.operator)}`, node.span)
        }
        const lhs_type = expect_to_impl_trait(type_check(node.lhs, env), trait, node.lhs.span)
        const op_function = expect_to_have_function(lhs_type, func, node.lhs.span)
        const rhs_type = type_check(node.rhs, env)
        expect_type(rhs_type, op_function.args[1], node.rhs.span)
        return add_attrs(op_function.return_type)
    }

    if (node instanceof ast.If) {
        expect_type(type_check(node.condition, env), env.builtin_type("bool"), node.condition.span)
        const then_type = type_check(node.then_block, env)
        if (node.else_block) {
            const else_type = type_check(node.else_block, env)
            expect_type(then_type, else_type, node.then_block.span)
        }
        return add_attrs(then_type)
    }

    if (node instanceof ast.Loop) {
        return add_attrs(type_check(node.block, env))
    }

    if (node instanceof ast.ClosureDefinition) {
        // FIXME: infer parameter types.
        // const closure_env = new TypeEnvironment(env)
        // for (const param of node.parameters) {
        //     closure_env.add_type(param.name, param.type_declaration, node.span)
        // }
        const parameter_types = node.parameters.map((p) => env.get_type(p.type_declaration!))
        const type = new FunctionType(
            "<function>",
            node.parameters.map((p) => p.name),
            false,
            env,
            node.span,
        )
        for (let i = 0; i < parameter_types.length; i++) {
            type.env.add_type(node.parameters[i].name, parameter_types[i])
        }
        type.env.add_type("return", type_check(node.block, type.env))
        return add_attrs(type)
    }

    if (node instanceof ast.Block) {
        // Forward declare contained declarations and definitions.
        const block_env = new TypeEnvironment(env)
        add_declarations_to_env(node.body, block_env)
        let return_type: Type = env.builtin_type("()")
        for (const block_node of node.body) {
            return_type = type_check(block_node, block_env)
        }
        return add_attrs(return_type)
    }

    if (node instanceof ast.FunctionDefinition) {
        const function_env = new TypeEnvironment(env)
        for (const param of node.declaration.parameters) {
            function_env.add_type(param.name, env.get_type(param.type))
        }
        const block_type = type_check(node.block, function_env)
        if (node.declaration.return_type) {
            const declaration_return_type = function_env.get_type(node.declaration.return_type)
            expect_type(block_type, declaration_return_type, node.span)
        }
        return add_attrs(block_type)
    }

    if (node instanceof ast.VariableDeclaration) {
        let var_type: Type | undefined
        let value_type: Type | undefined
        if (node.type) {
            var_type = env.get_type(node.type)
        }
        if (node.value) {
            value_type = type_check(node.value, env)
        }
        if (!var_type && !value_type) {
            throw new TypeError(`Cannot infer type of variable ${quote(node.name)}`, node.span)
        }
        if (var_type && value_type) {
            expect_type(value_type, var_type, node.span)
        }
        env.add_type(node.name, (var_type || value_type)!)
        return add_attrs(env.builtin_type("()"))
    }

    if (node instanceof ast.DeclarationOrDefinition) {
        // Declarations and definitions are handled in the first pass.
        return add_attrs(env.builtin_type("()"))
    }

    // TODO: match

    // There should be no other node types.
    throw new TypeError(`Unknown node type ${quote(node.constructor.name)}`, node.span)
}

function resolve_type_parameters(
    node: ast.StructInstantiation | {type_parameters: ast.TypeDeclaration[]; span: Span},
    struct_type: StructuredType,
): StructuredType {
    const type_parameters = struct_type.type_parameters.copy()
    const type_arguments =
        node instanceof ast.StructInstantiation ? node.type_arguments : node.type_parameters
    for (let i = 0; i < type_arguments.length; i++) {
        const type_argument = type_arguments[i]
        let resolved = struct_type.env.get_type(type_argument)
        if (resolved instanceof StructuredType) {
            resolved = resolve_type_parameters(type_argument, resolved)
        }
        type_parameters.set_type_argument(i, resolved, type_arguments[i].span)
    }
    return struct_type.resolve_type_parameters(type_parameters, node.span)
}

function add_declarations_to_env(nodes: ast.ASTNode[], env: TypeEnvironment) {
    nodes = nodes.filter((x) => x instanceof ast.DeclarationOrDefinition)
    // Add all names as dummy types to the environment to allow for forward
    // and recursive declarations.
    for (const node of nodes) {
        if (node instanceof ast.FunctionDefinition || node instanceof ast.FunctionDeclaration) {
            env.add_type(node.name, new FunctionType(node.name, [], false, env, node.span))
        } else if (node instanceof ast.StructDeclaration) {
            env.add_type(node.name, new StructType(node.name, node, env, node.span))
        } else if (node instanceof ast.EnumDeclaration) {
            env.add_type(node.name, new EnumType(node.name, new Map(), node, env, node.span))
        } else if (node instanceof ast.TraitDeclaration) {
            env.add_type(node.name, new TraitType(node.name, new Map(), node, env, node.span))
        } else if (node instanceof ast.ImplDefinition) {
            // Nothing to do.
        } else if (node instanceof ast.ExternBlock) {
            for (const extern_node of node.contained_nodes()) {
                if (extern_node instanceof ast.FunctionDeclaration) {
                    env.add_type(
                        extern_node.name,
                        new FunctionType(extern_node.name, [], false, env, extern_node.span),
                    )
                } else if (extern_node instanceof ast.StructDeclaration) {
                    env.add_type(
                        extern_node.name,
                        new StructType(extern_node.name, extern_node, env, extern_node.span),
                    )
                } else if (extern_node instanceof ast.EnumDeclaration) {
                    env.add_type(
                        extern_node.name,
                        new EnumType(
                            extern_node.name,
                            new Map(),
                            extern_node,
                            env,
                            extern_node.span,
                        ),
                    )
                } else if (extern_node instanceof ast.ImplDefinition) {
                    // Nothing to do.
                } else {
                    throw new TypeError(
                        `Unknown extern type ${quote(extern_node.constructor.name)}`,
                        extern_node.span,
                    )
                }
            }
        } else if (node instanceof ast.VariableDeclaration) {
            // Variables cannot be forward declared.
        } else {
            throw new TypeError(
                `Unknown declaration type ${quote(node.constructor.name)}`,
                node.span,
            )
        }
    }
    // First add all new types (structs and enums).
    for (const node of nodes) {
        if (node instanceof ast.StructDeclaration) {
            update_struct(node, env)
        } else if (node instanceof ast.EnumDeclaration) {
            update_enum(node, env)
        } else if (node instanceof ast.ExternBlock) {
            for (const extern_node of node.contained_nodes()) {
                if (extern_node instanceof ast.StructDeclaration) {
                    update_struct(extern_node, env)
                } else if (extern_node instanceof ast.EnumDeclaration) {
                    update_enum(extern_node, env)
                }
            }
        }
    }
    // Now add all traits.
    for (const node of nodes) {
        if (node instanceof ast.TraitDeclaration) {
            update_trait(node, env)
        }
    }
    // Now add functions and impls.
    for (const node of nodes) {
        if (node instanceof ast.FunctionDefinition || node instanceof ast.FunctionDeclaration) {
            update_function(node, env)
        } else if (node instanceof ast.StructDeclaration) {
            // Already handled.
        } else if (node instanceof ast.EnumDeclaration) {
            // Already handled.
        } else if (node instanceof ast.TraitDeclaration) {
            // Already handled.
        } else if (node instanceof ast.ImplDefinition) {
            add_impl_to_type(node, env)
        } else if (node instanceof ast.ExternBlock) {
            for (const extern_node of node.contained_nodes()) {
                if (extern_node instanceof ast.FunctionDeclaration) {
                    update_function(extern_node, env)
                } else if (extern_node instanceof ast.StructDeclaration) {
                    // Already handled.
                } else if (extern_node instanceof ast.EnumDeclaration) {
                    // Already handled.
                } else if (extern_node instanceof ast.ImplDefinition) {
                    add_impl_to_type(extern_node, env)
                } else {
                    throw new TypeError(
                        `Unknown extern type ${quote(extern_node.constructor.name)}`,
                        extern_node.span,
                    )
                }
            }
        } else if (node instanceof ast.VariableDeclaration) {
            // Variables cannot be forward declared.
        } else {
            throw new TypeError(
                `Unknown declaration type ${quote(node.constructor.name)}`,
                node.span,
            )
        }
    }
}

function update_function(
    node: ast.FunctionDefinition | ast.FunctionDeclaration,
    env: TypeEnvironment,
) {
    const type = expect_function_type(env.get_type(node), node.span)
    const declaration = node instanceof ast.FunctionDefinition ? node.declaration : node
    type.arg_names = declaration.parameters.map((p) => p.name)
    const types = FunctionType.parameter_types_from_declaration(declaration, env, type)
    for (const [i, name] of type.arg_names.entries()) {
        type.env.add_type(name, types[i])
    }
    type.env.add_type("return", env.get_type(declaration.return_type))
}

function add_impl_to_type(node: ast.ImplDefinition, env: TypeEnvironment) {
    // TODO: check that member functions don't shadow static functions and vice versa.
    // TODO: check that functions are not defined twice.
    // TODO: handle type parameters if they differ from the target type and for trait types.
    const target_type = expect_structured_type(
        env.get_type({name: node.target_name, span: node.span}),
        node.span,
    )
    if (target_type instanceof StructType || target_type instanceof EnumType) {
        target_type.declaration.attributes.impls.push(node)
    }
    const impl_env = new TypeEnvironment(target_type.env)
    for (let i = 0; i < node.type_parameters.length; i++) {
        const p = node.type_parameters[i]
        const type_parameter = new TypeParameter(p.name, p.span)
        impl_env.add_type(p.name, type_parameter)
        target_type.type_parameters.add_type_parameter_mapping(
            type_parameter,
            target_type.type_parameters.get(i),
            p.span,
        )
    }
    if (node.trait_name) {
        target_type.traits.push(node.trait_name)
        let trait_type = expect_trait_type(
            env.get_type({name: node.trait_name, span: node.span}),
            node.span,
        )
        // fixme: check that types match
        // trait_type.type_parameters.replace_matching(target_type.type_parameters)
        node.attributes.trait_declaration = trait_type.declaration
        // Add all default implementations that are not overridden.
        for (const f of trait_type.declaration.functions.values()) {
            if (!node.functions.some((f2) => f2.name === f.name)) {
                const func = FunctionType.from_declaration(f, impl_env, target_type)
                // console.log("AAA trait-func", func.signature, target_type.signature)
                target_type.env.add_type(f.name, func)
            }
        }
    }
    for (const f of node.functions.map((f) => f.declaration)) {
        const func = FunctionType.from_declaration(f, impl_env, target_type)
        // console.log("AAA func", func.signature, target_type.signature)
        target_type.env.add_type(f.name, func)
    }
}

function update_struct(node: ast.StructDeclaration, env: TypeEnvironment) {
    const struct_type = expect_struct_type(env.get_type(node), node.span)
    node.type_parameters.forEach((p) =>
        struct_type.type_parameters.add(
            new TypeParameter(p.name, node.span),
            struct_type.env,
            null,
        ),
    )
    Object.entries(node.fields).forEach(([name, type]) => {
        let field_type = struct_type.env.get_type(type)
        if (field_type instanceof StructuredType) {
            struct_type.type_parameters.map_type_parameters(field_type.type_parameters)
        }
        struct_type.env.add_type(name, field_type)
    })
}

function update_enum(node: ast.EnumDeclaration, env: TypeEnvironment) {
    const enum_type = expect_structured_type(env.get_type(node), node.span)
    node.type_parameters.forEach((p) =>
        enum_type.type_parameters.add(new TypeParameter(p.name, node.span), enum_type.env, null),
    )
    Object.values(node.variants).forEach((variant) => {
        enum_type.env.add_type(
            variant.name,
            new TupleType(
                variant.fields.fields.map((f) => enum_type.env.get_type(f)),
                enum_type.env,
                variant.span,
            ),
        )
    })
}

function update_trait(node: ast.TraitDeclaration, env: TypeEnvironment) {
    const trait_type = expect_trait_type(env.get_type(node), node.span)
    node.type_parameters.forEach((p) =>
        trait_type.type_parameters.add(new TypeParameter(p.name, node.span), trait_type.env, null),
    )
    node.functions.forEach((func) => {
        const declaration = func instanceof ast.FunctionDefinition ? func.declaration : func
        trait_type.env.add_type(
            func.name,
            FunctionType.from_declaration(declaration, trait_type.env, trait_type),
        )
    })
}

function expect_type(actual: Type, expected: Type, span: Span): Type {
    if (expected === SelfType.SELF_TYPE) {
        return actual
    }
    if (actual === SelfType.SELF_TYPE) {
        return expected
    }
    // We might expect a trait type, see if `actual` implements it.
    if (expected instanceof TraitType) {
        expect_to_impl_trait(actual, expected.name, span)
        return actual
    }
    if (!actual.equals(expected)) {
        throw new TypeError(
            `Expected type ${quote(expected.signature)} but got ${quote(actual.signature)}`,
            span,
        )
    }
    return actual
}

function expect_structured_type(type: Type, span: Span): StructuredType {
    if (!(type instanceof StructuredType)) {
        throw new TypeError(`Expected a structured type but got ${quote(type.signature)}`, span)
    }
    return type
}

function expect_function_type(type: Type, span: Span): FunctionType {
    if (!(type instanceof FunctionType)) {
        throw new TypeError(`Expected a function type but got ${quote(type.signature)}`, span)
    }
    return type
}

function expect_trait_type(type: Type, span: Span): TraitType {
    if (!(type instanceof TraitType)) {
        throw new TypeError(`Expected a trait type but got ${quote(type.signature)}`, span)
    }
    return type
}

function expect_struct_type(type: Type, span: Span): StructType {
    if (!(type instanceof StructType)) {
        throw new TypeError(`Expected a struct type but got ${quote(type.signature)}`, span)
    }
    return type
}

function expect_numeric_type(type: Type, span: Span): NumericType {
    if (!(type instanceof NumericType)) {
        throw new TypeError(`Expected a numeric type but got ${quote(type.signature)}`, span)
    }
    return type
}

function expect_to_impl_trait(type: Type, trait: string, span: Span): StructuredType {
    const structured_type = expect_structured_type(type, span)
    if (!structured_type.implements(trait)) {
        throw new TypeError(
            `${quote(type.name)} does not implement the ${quote(trait)} trait`,
            span,
        )
    }
    return structured_type
}

function expect_to_have_function(type: Type, name: string, span: Span): FunctionType {
    const structured_type = expect_structured_type(type, span)
    const function_type = structured_type.env.get_type({name, span})
    if (!(function_type instanceof FunctionType)) {
        throw new TypeError(
            `Expected function type but got ${quote(function_type.signature)}`,
            span,
        )
    }
    return function_type
}

export class TypeError extends Error {
    constructor(
        message: string,
        public span: Span,
    ) {
        super(`${message} at ${span}`)
    }
}

class Type {
    constructor(
        public _name: string,
        public span: Span,
    ) {}

    equals(other: Type): boolean {
        if (this === other) {
            return true
        }
        if (this.name !== other.name) {
            return false
        }
        return true
    }

    get name(): string {
        return this._name
    }

    get signature(): string {
        return this.name
    }

    copy(): Type {
        return new Type(this.name, this.span)
    }

    resolve_type_parameters(
        _type_parameters: TypeParameters,
        _span: Span,
        _resolved?: Map<Type, Type>,
    ): Type {
        return this
    }
}

class TypeParameter extends Type {
    static instance_counter = 0
    private instance_id: number

    constructor(name: string, span: Span) {
        super(name, span)
        this.instance_id = TypeParameter.instance_counter++
    }

    equals(other: Type): boolean {
        // Type parameters are only equal to themselves. We expcet to have one instance
        // per type parameter.
        return this === other
    }

    get signature(): string {
        return `TypeParameter(${this.name}#${this.instance_id} at ${this.span.toString()})`
    }

    copy(): TypeParameter {
        // We don't copy type parameters.
        return this
    }

    resolve_type_parameters(
        type_parameters: TypeParameters,
        span: Span,
        _resolved?: Map<Type, Type>,
    ): Type {
        return type_parameters.get_type_argument_or_null(this, span) || this
    }
}

class SelfType extends TypeParameter {
    static SELF_TYPE = new SelfType("Self", new Span(0, 0, "<builtin>", ""))

    private constructor(name: string, span: Span) {
        super(name, span)
    }

    resolve_type_parameters(
        _type_parameters: TypeParameters,
        _span: Span,
        _resolved?: Map<Type, Type>,
    ): Type {
        return this
    }
}

/**
 * A structured type is a type that contains fields or methods modeled
 * as an inner type environment.
 */
class StructuredType extends Type {
    env: TypeEnvironment
    traits: string[] = []
    type_parameters: TypeParameters

    constructor(name: string, parent_env: TypeEnvironment, span: Span) {
        super(name, span)
        this.env = new TypeEnvironment(parent_env)
        this.type_parameters = new TypeParameters()
    }

    equals(other: Type): boolean {
        if (!super.equals(other)) {
            return false
        }
        if (!(other instanceof StructuredType)) {
            return false
        }
        if (this === other) {
            return true
        }
        // Two structured types are only equal if they resolve to the same type parameters.
        return this.type_parameters.equals(other.type_parameters)
    }

    implements(trait: string): boolean {
        return this.traits.includes(trait)
    }

    get signature(): string {
        if (this.type_parameters.size === 0) {
            return super.signature
        }
        return `${this.constructor.name}(${this.name}<${this.type_parameters.signature}>)`
    }

    copy(): StructuredType {
        return this.finish_copy(new StructuredType(this.name, this.env, this.span))
    }

    finish_copy<T extends StructuredType>(target_type: T): T {
        target_type.traits = [...this.traits]
        target_type.type_parameters = this.type_parameters.copy()
        return target_type
    }

    resolve_type_parameters(
        type_parameters: TypeParameters,
        span: Span,
        resolved?: Map<Type, Type>,
    ): StructuredType {
        let existing = resolved?.get(this)
        if (existing) {
            return existing as StructuredType
        }
        type_parameters = type_parameters.copy()
        const new_type = this.copy()
        new_type.type_parameters = type_parameters
        const new_env = new TypeEnvironment(this.env.parent!)
        resolved = resolved || new Map()
        resolved.set(this, new_type)
        for (const name of this.env.types.keys()) {
            let type = this.env
                .get_type({name, span: this.span})
                .resolve_type_parameters(type_parameters, span, resolved)
            if (type === SelfType.SELF_TYPE) {
                type = new_type
            } else if (type instanceof TypeParameter) {
                type = type_parameters.get_type_argument_or_null(type, span) || type
            }
            new_env.add_type(name, type)
        }
        new_type.env = new_env
        return new_type
    }
}

class TupleType extends StructuredType {
    constructor(
        public types: Type[],
        parent_env: TypeEnvironment,
        span: Span,
    ) {
        super(`(${types.map((t) => t.name).join(", ")})`, parent_env, span)
        for (const [i, type] of types.entries()) {
            this.env.add_type(`${i}`, type)
        }
    }

    copy(): TupleType {
        return this.finish_copy(new TupleType(this.types, this.env, this.span))
    }
}

class StructType extends StructuredType {
    constructor(
        name: string,
        public declaration: ast.StructDeclaration,
        parent_env: TypeEnvironment,
        span: Span,
    ) {
        super(name, parent_env, span)
    }

    copy(): StructType {
        return this.finish_copy(new StructType(this.name, this.declaration, this.env, this.span))
    }
}

/**
 * Enum are structured types with numbered fields.
 */
class EnumType extends StructuredType {
    constructor(
        name: string,
        public fields: Map<string, Type>,
        public declaration: ast.EnumDeclaration,
        parent_env: TypeEnvironment,
        span: Span,
    ) {
        super(name, parent_env, span)
        for (const [name, type] of fields.entries()) {
            this.env.add_type(name, type)
        }
    }

    copy(): EnumType {
        return this.finish_copy(
            new EnumType(
                this.name,
                new Map(this.fields.entries()),
                this.declaration,
                this.env,
                this.span,
            ),
        )
    }
}

class TraitType extends StructuredType {
    constructor(
        name: string,
        public functions: Map<string, FunctionType>,
        public declaration: ast.TraitDeclaration,
        parent_env: TypeEnvironment,
        span: Span,
    ) {
        super(name, parent_env, span)
        for (const [name, type] of functions.entries()) {
            this.env.add_type(name, type)
        }
    }

    copy(): TraitType {
        return this.finish_copy(
            new TraitType(this.name, this.functions, this.declaration, this.env, this.span),
        )
    }
}

class FunctionType extends StructuredType {
    static from_declaration(
        declaration_or_definition: ast.FunctionDeclaration | ast.FunctionDefinition,
        env: TypeEnvironment,
        target_type: StructuredType | null,
    ): FunctionType {
        let declaration =
            declaration_or_definition instanceof ast.FunctionDefinition
                ? declaration_or_definition.declaration
                : declaration_or_definition
        const args = FunctionType.parameter_types_from_declaration(declaration, env, target_type)
        const f = new FunctionType(
            declaration.name,
            declaration.parameters.map((a) => a.name),
            declaration.parameters[0]?.name === "self",
            env,
            declaration.span,
            declaration_or_definition,
        )
        for (let i = 0; i < args.length; i++) {
            f.env.add_type(f.arg_names[i], args[i])
        }
        f.env.add_type("return", env.get_type(declaration.return_type))
        return f
    }

    static parameter_types_from_declaration(
        declaration: ast.FunctionDeclaration,
        env: TypeEnvironment,
        target_type: StructuredType | null,
    ): Type[] {
        return declaration.parameters.map((p, i) => {
            if (p.name === "self") {
                if (i !== 0) {
                    throw new TypeError(
                        `The self parameter must be the first parameter of a function`,
                        p.span,
                    )
                }
                if (!target_type) {
                    throw new TypeError(`The self parameter can only be used in methods`, p.span)
                }
                return SelfType.SELF_TYPE
            }
            if (p.type instanceof ast.FunctionTypeDeclaration) {
                // This is a function type parameter.
                return FunctionType.from_function_type(p.type, env)
            }
            return env.get_type(p.type)
        })
    }

    static from_function_type(
        type: ast.FunctionTypeDeclaration,
        env: TypeEnvironment,
    ): FunctionType {
        const arg_names = type.arg_types.map((_, i) => `arg${i}`)
        const f = new FunctionType("<function>", arg_names, false, env, type.span)
        for (const [i, arg] of type.arg_types.entries()) {
            f.env.add_type(arg_names[i], env.get_type(arg))
        }
        f.env.add_type("return", env.get_type(type.return_type))
        return f
    }

    constructor(
        name: string,
        public arg_names: string[],
        public is_method: boolean,
        parent_env: TypeEnvironment,
        span: Span,
        public declaration_or_definition?: ast.FunctionDeclaration | ast.FunctionDefinition,
    ) {
        super(name, parent_env, span)
    }

    equals(other: Type): boolean {
        if (!super.equals(other)) {
            return false
        }
        if (!(other instanceof FunctionType)) {
            return false
        }
        if (this.arg_names.length !== other.arg_names.length) {
            return false
        }
        for (let i = 0; i < this.arg_names.length; i++) {
            const name = {name: this.arg_names[i], span: this.span}
            if (!this.env.get_type(name).equals(other.env.get_type(name))) {
                return false
            }
        }
        return this.env
            .get_type({name: "return", span: this.span})
            .equals(other.env.get_type({name: "return", span: this.span}))
    }

    get signature(): string {
        console.log("AAA ???", new Error().stack)
        const args = []
        for (const name of this.arg_names) {
            args.push(`${name} ${this.env.get_type({name, span: this.span}).name}`)
        }
        const return_signature = this.env.get_type({name: "return", span: this.span}).name
        return `fn ${this.name}(${args.join(", ")}) ${return_signature}`
    }

    copy(): FunctionType {
        return this.finish_copy(
            new FunctionType(
                this.name,
                [...this.arg_names],
                this.is_method,
                this.env,
                this.span,
                this.declaration_or_definition,
            ),
        )
    }

    get args(): Type[] {
        return this.arg_names.map((name) => this.env.get_type({name, span: this.span}))
    }

    get return_type(): Type {
        return this.env.get_type({name: "return", span: this.span})
    }
}

class NumericType extends StructuredType {
    constructor(name: string, parent_env: TypeEnvironment, span: Span) {
        super(name, parent_env, span)
    }

    is_assignable_to(other: Type): boolean {
        if (this === other) {
            return true
        }
        if (!(other instanceof NumericType)) {
            return false
        }
        if (this.name === "u32" && other.name === "usize") {
            return true
        }
        return false
    }

    copy(): NumericType {
        return this.finish_copy(new NumericType(this.name, this.env, this.span))
    }
}

class TypeParameters {
    constructor(
        private type_parameters: TypeParameter[] = [],
        private type_arguments: (Type | null)[] = [],
        private type_parameter_map: [TypeParameter, TypeParameter][] = [],
    ) {
        assert.equal(type_parameters.length, type_arguments.length)
    }

    add(type_parameter: TypeParameter, target_env: TypeEnvironment, type_argument: Type | null) {
        this.type_parameters.push(type_parameter)
        this.type_arguments.push(type_argument)
        this.type_parameter_map.push([type_parameter, type_parameter])
        target_env.add_type(type_parameter.name, type_argument || type_parameter)
    }

    get(index: number): TypeParameter {
        if (index >= this.type_parameters.length) {
            throw new TypeError(
                `There are only ${this.type_parameters.length} type parameters`,
                this.type_parameters[0].span,
            )
        }
        return this.type_parameters[index]
    }

    set_type_argument(index: number, type_argument: Type, span: Span) {
        if (index >= this.type_arguments.length) {
            throw new TypeError(
                `There are only ${this.type_arguments.length} type arguments in ${this.signature}`,
                span,
            )
        }
        if (type_argument instanceof TypeParameter) {
            throw new TypeError(
                `Cannot use type parameter ${quote(type_argument.signature)} as type argument for ${
                    this.type_parameters[index].signature
                }`,
                span,
            )
        }
        this.type_arguments[index] = type_argument
    }

    get_type_argument_or_null(type_parameter: TypeParameter, span: Span): Type | null {
        const mapped = this.type_parameter_map.find((x) => x[0] === type_parameter)
        if (mapped) {
            type_parameter = mapped[1]
        }
        const index = this.type_parameters.indexOf(type_parameter)
        if (index === -1) {
            // console.log(
            //     "AAA ",
            //     this.type_parameter_map.map((x) => [x[0].signature, x[1].signature]),
            // )
            throw new TypeError(
                `Unknown type parameter ${quote(type_parameter.signature)} in ${this.signature}`,
                span,
            )
        }
        return this.type_arguments[index]
    }

    add_type_parameter_mapping(src: TypeParameter, dst: TypeParameter, span: Span) {
        const index = this.type_parameters.indexOf(dst)
        if (index === -1) {
            throw new TypeError(
                `Unknown type parameter ${quote(dst.signature)} in ${this.signature}`,
                span,
            )
        }
        const existing = this.type_parameter_map.find((x) => x[0] === src && x[1] === dst)
        if (!existing) {
            this.type_parameter_map.push([src, dst])
        }
    }

    /**
     * Add a mapping of type parameters with the same name from `other` to `this`.
     */
    map_type_parameters(other: TypeParameters) {
        for (const p of this.type_parameters) {
            const other_p = other.type_parameters.find((x) => x.name === p.name)
            if (other_p) {
                this.add_type_parameter_mapping(other_p, p, other_p.span)
                for (const [src, dst] of other.type_parameter_map) {
                    if (dst === other_p) {
                        this.add_type_parameter_mapping(src, p, dst.span)
                    }
                }
            }
        }
    }

    get signature(): string {
        const res = []
        for (let i = 0; i < this.type_parameters.length; i++) {
            const type_parameter = this.type_parameters[i]
            const type_argument = this.type_arguments[i]
            res.push(`${type_parameter.signature}=${type_argument?.signature ?? "?"}`)
        }
        return res.join(", ")
    }

    copy(): TypeParameters {
        return new TypeParameters(
            [...this.type_parameters],
            [...this.type_arguments],
            [...this.type_parameter_map],
        )
    }

    equals(other: TypeParameters): boolean {
        if (this === other) {
            return true
        }
        if (this.type_parameters.length !== other.type_parameters.length) {
            return false
        }
        for (let i = 0; i < this.type_parameters.length; i++) {
            const this_arg = this.type_arguments[i]
            const other_arg = other.type_arguments[i]
            if (!this_arg && !other_arg) {
                continue
            }
            if (!this_arg || !other_arg) {
                return false
            }
            if (!this_arg.equals(other_arg)) {
                return false
            }
        }
        return true
    }

    get size(): number {
        return this.type_parameters.length
    }
}

/**
 * A type environment is a mapping from names to types.
 * It is used to keep track of the types of variables and declarations.
 */
export class TypeEnvironment {
    static global() {
        const span = new Span(0, 0, "<builtin>", "")
        const env = new TypeEnvironment()
        env.add_type("i32", new NumericType("i32", env, span))
        env.add_type("u32", new NumericType("u32", env, span))
        env.add_type("usize", new NumericType("usize", env, span))
        env.add_type("bool", new StructuredType("bool", env, span))
        env.add_type("()", new Type("()", span))
        return env
    }

    builtin_type(name: "i32" | "u32" | "usize" | "bool" | "()"): Type {
        return this.get_type({name, span: new Span(0, 0, "<builtin>", "")})
    }
    types: Map<string, Type> = new Map()

    constructor(public parent?: TypeEnvironment) {}

    get_type(type_identifier: ast.TypeDeclaration | Type | {name: string; span: Span}): Type {
        if (type_identifier instanceof ast.FunctionTypeDeclaration) {
            return FunctionType.from_function_type(type_identifier, this)
        }
        if (type_identifier.name === SelfType.SELF_TYPE.name) {
            return SelfType.SELF_TYPE
        }
        if (type_identifier instanceof ast.TupleTypeDeclaration) {
            return new TupleType(
                type_identifier.fields.map((t) => this.get_type(t)),
                this,
                type_identifier.span,
            )
        }
        const name = type_identifier.name
        const type = this.get_type_or_null(name)
        if (type) {
            return type
        }
        throw new TypeError(`Unknown type ${quote(name)}`, type_identifier.span)
    }

    get_type_or_null(name: string): Type | null {
        const type = this.types.get(name)
        if (type) {
            return type
        }
        if (!this.parent) {
            return null
        }
        return this.parent.get_type_or_null(name)
    }

    add_type(name: string, type: Type) {
        if (!(type instanceof Type)) {
            console.log("AAA ?????????????????????????????????????", name, new Error().stack)
        }
        if (this.types.has(name)) {
            throw new TypeError(`Name ${quote(name)} already defined in current scope`, type.span)
        }
        this.types.set(name, type)
    }

    all_type_names(): Set<string> {
        const res = new Set<string>()
        for (const [name] of this.types) {
            res.add(name)
        }
        if (this.parent) {
            for (const name of this.parent.all_type_names()) {
                res.add(name)
            }
        }
        return res
    }

    str_type(): Type {
        return this.get_type({name: "str", span: new Span(0, 0, "<builtin>", "")})
    }

    to_debug_str(indent = 0): string {
        return this.to_debug_str_shallow(indent) + (this.parent?.to_debug_str(indent + 2) ?? "")
    }

    to_debug_str_shallow(indent = 0): string {
        let s = `${" ".repeat(indent)}TypeEnvironment\n`
        for (const [name, type] of this.types) {
            s += `${" ".repeat(indent)}${name}: ${type.signature}\n`
        }
        return s
    }

    copy(): TypeEnvironment {
        const env = new TypeEnvironment(this.parent)
        env.types = new Map(this.types)
        return env
    }
}
