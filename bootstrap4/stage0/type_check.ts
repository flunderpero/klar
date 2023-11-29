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
    if (node instanceof ast.String_) {
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
                `Expected struct type but got ${quote(struct_type.name)}`,
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
        const instance_type = new StructType(
            node.target_struct_name,
            struct_type.declaration,
            struct_type.env,
            node.span,
        )
        // Resolve all type parameters and set self-type.
        for (let i = 0; i < node.type_arguments.length; i++) {
            const type_parameter = struct_type.env.type_parameters[i]
            type_parameter.resolved = env.get_type(node.type_arguments[i])
        }
        instance_type.env.self_type = new TypeParameter("Self", node.span)
        instance_type.env.self_type.resolved = instance_type
        return add_attrs(instance_type)
    }

    if (node instanceof ast.FunctionCall) {
        const function_type_ = expect_function_type(type_check(node.target, env), node.span)
        let function_type = function_type_
        let target_env = env
        if (
            node.target.attributes.target_type &&
            node.target.attributes.target_type instanceof StructuredType
        ) {
            target_env = node.target.attributes.target_type.env
            const is_member_function = function_type.args[0]?.name === "Self"
            function_type = function_type_.re_evaluate_types(target_env)
            // Remove the first argument if it is the self type.
            if (is_member_function) {
                function_type.args = function_type.args.slice(1)
            }
        }
        const args = node.args.map((a) => type_check(a, env))
        if (args.length !== function_type.args.length) {
            throw new TypeError(
                `Expected ${function_type.args.length} arguments but got ${args.length}`,
                node.span,
            )
        }
        for (let i = 0; i < args.length; i++) {
            const arg = args[i]
            let f_arg = function_type.args[i]
            if (f_arg instanceof FunctionType) {
                f_arg = f_arg.re_evaluate_types(target_env)
                console.log("AAA f_arg", function_type.name, f_arg.signature, target_env.to_debug_str())
            }
            expect_type(arg, f_arg, node.span)
        }
        let return_type = function_type.return_type
        if (function_type.return_type instanceof FunctionType) {
            return_type = function_type.return_type.re_evaluate_types(target_env)
        }
        return add_attrs(return_type)
    }

    if (node instanceof ast.IdentifierReference) {
        let target_type: StructuredType | undefined
        if (node.type_parameters.length > 0) {
            // Create a new type if we have type parameters.
            const type = expect_struct_type(env.get_type(node), node.span)
            // fixme: enum
            target_type = new StructType(type.name, type.declaration, type.env.parent!, node.span)
            for (let i = 0; i < node.type_parameters.length; i++) {
                const type_parameter = type.env.type_parameters[i]
                type_parameter.resolved = env.get_type(node.type_parameters[i])
            }
            target_type.env.self_type = new TypeParameter("Self", node.span)
            target_type.env.self_type.resolved = target_type
        }
        return add_attrs(env.get_type(node), {target_type})
    }

    if (node instanceof ast.Assignment) {
        const target_type = type_check(node.target, env)
        const value_type = type_check(node.value, env)
        expect_type(value_type, target_type, node.span)
        return add_attrs(value_type)
    }

    if (node instanceof ast.FieldAccess) {
        const target_type = expect_structured_type(type_check(node.target, env), node.span)
        const field_type = target_type.env.get_type({name: node.field, span: node.span})
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
            throw new TypeError(`Expected usize but got ${quote(index_type.name)}`, node.span)
        }
        const target_type = expect_structured_type(type_check(node.target, env), node.span)
        if (node.is_write) {
            expect_to_impl_trait(target_type, "IndexedSet", node.span)
            const set_function_ = expect_to_have_function(target_type, "set", node.span)
            // We have to re-evaluate the types of the `set_function_` because it might be part
            // of a trait or an impl, or may contain type parameters.
            const set_function = set_function_.re_evaluate_types(target_type.env)
            return add_attrs(set_function.return_type)
        }
        expect_to_impl_trait(target_type, "IndexedGet", node.span)
        const get_function_ = expect_to_have_function(target_type, "get", node.span)
        // We have to re-evaluate the types of the `get_function_` because it might be part
        // of a trait or an impl, or may contain type parameters.
        const get_function = get_function_.re_evaluate_types(target_type.env)
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
        type.env.parent!.type_parameters[0].resolved = element_type
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
        const op_function_ = expect_to_have_function(lhs_type, func, node.lhs.span)
        // We have to re-evaluate the types of the `op_function_` because it might be part
        // of a trait or an impl, or may contain type parameters.
        const op_function = op_function_.re_evaluate_types(lhs_type.env)
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
        const function_env = new TypeEnvironment(env)
        for (const param of node.parameters) {
            function_env.add_type(param.name, env.get_type(param.type_declaration!))
        }
        const return_type = type_check(node.block, function_env)
        const type = new FunctionType("<function>", parameter_types, return_type, node.span)
        return add_attrs(type)
    }

    if (node instanceof ast.Block) {
        // Forward declare contained declarations and definitions.
        console.log("AAA ", new Error().stack)
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
            function_env.add_type(param.name, env.get_type(param.type_declaration))
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

function add_declarations_to_env(nodes: ast.ASTNode[], env: TypeEnvironment) {
    nodes = nodes.filter((x) => x instanceof ast.DeclarationOrDefinition)
    // Add all names as dummy types to the environment to allow for forward
    // and recursive declarations.
    for (const node of nodes) {
        if (node instanceof ast.FunctionDefinition || node instanceof ast.FunctionDeclaration) {
            env.add_type(
                node.name,
                new FunctionType(node.name, [], env.builtin_type("()"), node.span),
            )
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
                        new FunctionType(
                            extern_node.name,
                            [],
                            env.builtin_type("()"),
                            extern_node.span,
                        ),
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
    type.args = FunctionType.args_from_declaration(declaration, env)
    type.return_type = env.get_type(declaration.return_type)
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
    if (node.trait_name) {
        target_type.traits.push(node.trait_name)
        const trait_type = expect_trait_type(
            env.get_type({name: node.trait_name, span: node.span}),
            node.span,
        )
        node.attributes.trait_declaration = trait_type.declaration
        const new_env = new TypeEnvironment(target_type.env.parent)
        target_type.env.parent = new_env
        for (const type of trait_type.env.types.values()) {
            new_env.add_type(type.name, type)
        }
        if (node.trait_type_parameters.length !== trait_type.declaration.type_parameters.length) {
            throw new TypeError(
                `Expected ${trait_type.declaration.type_parameters.length} type arguments but got ${node.trait_type_parameters.length}`,
                node.span,
            )
        }
        // Resolve trait parameters if they are an actual type.
        for (let i = 0; i < node.trait_type_parameters.length; i++) {
            const type_parameter = node.trait_type_parameters[i]
            const type = target_type.env.get_type_or_null(type_parameter.name)
            if (type) {
                const trait_type_parameter = trait_type.declaration.type_parameters[i]
                new_env.add_type(trait_type_parameter.name, type)
            }
        }
        return
    }
    for (const f of node.functions.map((f) => f.declaration)) {
        target_type.env.add_type(f.name, FunctionType.from_declaration(f, target_type.env))
    }
}

function update_struct(node: ast.StructDeclaration, env: TypeEnvironment) {
    const struct_type = expect_struct_type(env.get_type(node), node.span)
    struct_type.env.self_type = new TypeParameter("Self", node.span)
    node.type_parameters.forEach((p) =>
        struct_type.env.add_type_parameter(p.name, new TypeParameter(p.name, node.span)),
    )
    Object.entries(node.fields).forEach(([name, type]) => {
        const field_type = struct_type.env.get_type(type)
        struct_type.env.add_type(name, field_type)
    })
}

function update_enum(node: ast.EnumDeclaration, env: TypeEnvironment) {
    const enum_type = expect_structured_type(env.get_type(node), node.span)
    enum_type.env.self_type = new TypeParameter("Self", node.span)
    node.type_parameters.forEach((p) =>
        enum_type.env.add_type_parameter(p.name, new TypeParameter(p.name, node.span)),
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
    trait_type.env.self_type = new TypeParameter("Self", node.span)
    node.type_parameters.forEach((p) =>
        trait_type.env.add_type_parameter(p.name, new TypeParameter(p.name, node.span)),
    )
    node.functions.forEach((func) => {
        const declaration = func instanceof ast.FunctionDefinition ? func.declaration : func
        trait_type.env.add_type(
            func.name,
            FunctionType.from_declaration(declaration, trait_type.env),
        )
    })
}

function expect_type(actual: Type, expected: Type, span: Span): Type {
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
        throw new TypeError(`Expected a structured type but got ${quote(type.name)}`, span)
    }
    return type
}

function expect_function_type(type: Type, span: Span): FunctionType {
    if (!(type instanceof FunctionType)) {
        throw new TypeError(`Expected a function type but got ${quote(type.name)}`, span)
    }
    return type
}

function expect_trait_type(type: Type, span: Span): TraitType {
    if (!(type instanceof TraitType)) {
        throw new TypeError(`Expected a trait type but got ${quote(type.name)}`, span)
    }
    return type
}

function expect_struct_type(type: Type, span: Span): StructType {
    if (!(type instanceof StructType)) {
        throw new TypeError(`Expected a struct type but got ${quote(type.name)}`, span)
    }
    return type
}

function expect_numeric_type(type: Type, span: Span): NumericType {
    if (!(type instanceof NumericType)) {
        throw new TypeError(`Expected a numeric type but got ${quote(type.name)}`, span)
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
        throw new TypeError(`Expected function type but got ${quote(function_type.name)}`, span)
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
}

class TypeParameter extends Type {
    private _resolved?: Type

    constructor(name: string, span: Span) {
        super(name, span)
    }

    equals(other: Type): boolean {
        if (!super.equals(other)) {
            return false
        }
        if (!(other instanceof TypeParameter)) {
            return false
        }
        return true
    }

    get resolved(): Type | undefined {
        return this._resolved
    }

    set resolved(type: Type) {
        if (type instanceof TypeParameter) {
            throw new TypeError(
                `Cannot resolve type parameter to another type parameter`,
                this.span,
            )
        }
        this._resolved = type
    }

    get signature(): string {
        return `${this.name}=${this._resolved?.signature ?? "?"}`
    }
}

/**
 * A structured type is a type that contains fields or methods modeled
 * as an inner type environment.
 */
class StructuredType extends Type {
    env: TypeEnvironment
    traits: string[] = []

    constructor(name: string, parent_env: TypeEnvironment, span: Span) {
        super(name, span)
        this.env = new TypeEnvironment(parent_env)
    }

    equals(other: Type): boolean {
        if (!super.equals(other)) {
            return false
        }
        if (!(other instanceof StructuredType)) {
            return false
        }
        // Two structured types are only equal if they resolve to the same type parameters.
        const this_type_arguments = this.env.type_parameters.map((p) => p.resolved)
        const other_type_arguments = other.env.type_parameters.map((p) => p.resolved)
        if (this_type_arguments.length !== other_type_arguments.length) {
            return false
        }
        for (let i = 0; i < this_type_arguments.length; i++) {
            const this_type = this_type_arguments[i]
            const other_type = other_type_arguments[i]
            if (!this_type) {
                if (!other_type) {
                    continue
                }
                return false
            }
            if (!other_type) {
                return false
            }
            if (!this_type.equals(other_type!)) {
                return false
            }
        }
        return true
    }

    implements(trait: string): boolean {
        return this.traits.includes(trait)
    }

    get signature(): string {
        const type_parameters = this.env.type_parameters.map((p) => p.signature).join(", ")
        if (type_parameters.length === 0) {
            return super.signature
        }
        return `${this.name}<${type_parameters}>`
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
}

class FunctionType extends Type {
    static from_declaration(
        declaration_or_definition: ast.FunctionDeclaration | ast.FunctionDefinition,
        env: TypeEnvironment,
    ): FunctionType {
        let declaration =
            declaration_or_definition instanceof ast.FunctionDefinition
                ? declaration_or_definition.declaration
                : declaration_or_definition
        return new FunctionType(
            declaration.name,
            FunctionType.args_from_declaration(declaration, env),
            env.get_type(declaration.return_type),
            declaration.span,
            declaration_or_definition,
        )
    }

    static args_from_declaration(declaration: ast.FunctionDeclaration, env: TypeEnvironment) {
        return declaration.parameters.map((p, i) => {
            if (p.name === "self") {
                if (i !== 0) {
                    throw new TypeError(
                        `The self parameter must be the first parameter of a function`,
                        p.span,
                    )
                }
                return env.self_type!
            }
            if (p.type_declaration instanceof ast.FunctionTypeDeclaration) {
                // This is a function type parameter.
                return FunctionType.from_function_type(p.type_declaration, env)
            }
            return env.get_type(p.type_declaration)
        })
    }

    static from_function_type(
        type: ast.FunctionTypeDeclaration,
        env: TypeEnvironment,
    ): FunctionType {
        return new FunctionType(
            "<function>",
            type.arg_types.map((p) => env.get_type(p)),
            env.get_type(type.return_type),
            type.span,
        )
    }

    constructor(
        name: string,
        public args: Type[],
        public return_type: Type,
        span: Span,
        public declaration_or_definition?: ast.FunctionDeclaration | ast.FunctionDefinition,
    ) {
        super(name, span)
    }

    equals(other: Type): boolean {
        if (!super.equals(other)) {
            return false
        }
        if (!(other instanceof FunctionType)) {
            return false
        }
        if (this.args.length !== other.args.length) {
            return false
        }
        for (let i = 0; i < this.args.length; i++) {
            if (!this.args[i].equals(other.args[i])) {
                return false
            }
        }
        return this.return_type.equals(other.return_type)
    }

    re_evaluate_types(env: TypeEnvironment): FunctionType {
        const new_args = []
        for (const arg of this.args) {
            if (arg instanceof FunctionType) {
                new_args.push(arg.re_evaluate_types(env))
                continue
            }
            new_args.push(env.get_type(arg))
        }
        let new_return_type
        if (this.return_type instanceof FunctionType) {
            new_return_type = this.return_type.re_evaluate_types(env)
        } else {
            new_return_type = env.get_type(this.return_type)
        }
        return new FunctionType(
            this.name,
            new_args,
            new_return_type,
            this.span,
            this.declaration_or_definition,
        )
    }

    get signature(): string {
        return `${this.name}(${this.args.map((a) => a.signature).join(", ")}) -> ${
            this.return_type.signature
        }`
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
        // Add self types to all.
        for (const type of env.types.values()) {
            if (type instanceof StructuredType) {
                type.env.self_type = new TypeParameter("Self", span)
                type.env.self_type.resolved = type
            }
        }
        return env
    }

    builtin_type(name: "i32" | "u32" | "usize" | "bool" | "()"): Type {
        return this.get_type(
            new ast.TypeDeclaration({name, type_parameters: []}, new Span(0, 0, "<builtin>", "")),
        )
    }

    public types: Map<string, Type> = new Map()
    public type_parameters: TypeParameter[] = []
    public self_type?: TypeParameter

    constructor(public parent?: TypeEnvironment) {}

    public get_type(
        type_identifier: ast.TypeDeclaration | Type | {name: string; span: Span},
    ): Type {
        if (type_identifier.name === "Self") {
            const self_type = this.get_self_type_or_null()
            if (self_type) {
                return self_type.resolved || self_type
            }
        }
        if (type_identifier instanceof ast.FunctionTypeDeclaration) {
            return FunctionType.from_function_type(type_identifier, this)
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
            if (type instanceof TypeParameter) {
                return type.resolved || type
            }
            return type
        }
        const type_parameter = this.get_type_parameter_or_null(name)
        if (type_parameter) {
            return type_parameter.resolved || type_parameter
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

    private get_self_type_or_null(): TypeParameter | null {
        if (this.self_type) {
            return this.self_type
        }
        if (!this.parent) {
            return null
        }
        return this.parent.get_self_type_or_null()
    }

    public add_type(name: string, type: Type) {
        if (!(type instanceof Type)) {
            console.log("AAA ?????????????????????????????????????", name, new Error().stack)
        }
        if (this.types.has(name) || this.get_type_parameter_or_null(name)) {
            throw new TypeError(`Name ${quote(name)} already defined in current scope`, type.span)
        }
        this.types.set(name, type)
    }

    public get_type_parameter(name: string, span: Span): TypeParameter {
        const type_parameter = this.get_type_parameter_or_null(name)
        if (!type_parameter) {
            throw new TypeError(`Type parameter ${quote(name)} not found`, span)
        }
        return type_parameter
    }

    private get_type_parameter_or_null(name: string): TypeParameter | null {
        const type_parameter = this.type_parameters.find((p) => p.name === name)
        if (type_parameter) {
            return type_parameter
        }
        return this.parent?.get_type_parameter_or_null(name) ?? null
    }

    public add_type_parameter(name: string, type: TypeParameter) {
        // Type parameters cannot shadow outer type parameters and other types.
        if (this.get_type_parameter_or_null(name)) {
            throw new TypeError(`Type parameter ${quote(name)} already defined`, type.span)
        }
        const existing_type = this.get_type_or_null(name)
        if (existing_type) {
            throw new TypeError(
                `Type parameter ${quote(name)} at ${type.span} shadows type ${quote(
                    existing_type,
                )}`,
                existing_type.span,
            )
        }
        this.type_parameters.push(type)
    }

    str_type(): Type {
        return this.get_type({name: "str", span: new Span(0, 0, "<builtin>", "")})
    }

    equals(other: TypeEnvironment): boolean {
        if (this === other) {
            return true
        }
        if (this.types.size !== other.types.size) {
            return false
        }
        for (const [name, type] of this.types) {
            const other_type = other.types.get(name)
            if (!other_type || !type.equals(other_type)) {
                return false
            }
        }
        return true
    }

    to_debug_str(indent = 0): string {
        let s = `${" ".repeat(indent)}TypeEnvironment<${this.type_parameters
            .map((p) => `${p.name} = ${p.resolved?.name || "?"}`)
            .join(", ")}>\n`
        s += `${" ".repeat(indent)}Self: ${
            this.self_type?.resolved?.name || this.self_type?.name
        }\n`
        for (const [name, type] of this.types) {
            if (type instanceof TypeParameter) {
                s += `${" ".repeat(indent)}${name}: ${type.name} = ${type.resolved?.name || "?"}\n`
            } else {
                s += `${" ".repeat(indent)}${name}\n`
            }
        }
        return s + "\n" + (this.parent?.to_debug_str(indent + 2) ?? "")
    }

    copy(): TypeEnvironment {
        const env = new TypeEnvironment(this.parent)
        env.types = new Map(this.types)
        env.type_parameters = this.type_parameters.slice()
        return env
    }
}
