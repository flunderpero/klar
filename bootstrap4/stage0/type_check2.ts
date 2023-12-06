import * as ast from "./parser"
import {lexer} from "./lexer"
import {TokenStream, parse} from "./parser"
import assert from "assert"
import {Span, quote} from "./common"

export function type_check_ast(ast: ast.AST, env: TypeEnvironment): Type {
    let type: Type = Type.unit
    parse_declarations_and_definitions(ast, env)
    for (let i = 0; i < ast.body.length; i++) {
        type = type_check(ast.body[i], env, {used_in_expression: i === ast.body.length - 1})
    }
    return type
}

type Context = {
    used_in_expression: boolean
}

function type_check(node: ast.ASTNode, env: TypeEnvironment, ctx: Context): Type {
    let type: Type
    if (node instanceof ast.Number_) {
        type = NumericType.i32
    } else if (node instanceof ast.Bool) {
        type = Type.bool
    } else if (node instanceof ast.If) {
        const condition_type = type_check(node.condition, env, {used_in_expression: true})
        expect_equal_types(Type.bool, condition_type, node.span)
        const then_type = type_check(node.then_block, env, {used_in_expression: false})
        if (node.else_block) {
            const else_type = type_check(node.else_block, env, {used_in_expression: false})
            if (ctx.used_in_expression) {
                expect_equal_types(then_type, else_type, node.span)
            }
        }
        type = ctx.used_in_expression ? then_type : Type.unit
    } else if (node instanceof ast.Block) {
        const block_env = new TypeEnvironment(env)
        let block_type: Type = Type.unit
        for (let i = 0; i < node.body.length; i++) {
            block_type = type_check(node.body[i], block_env, {
                // The last expression in a block is used as the block's type.
                used_in_expression: ctx.used_in_expression && i === node.body.length - 1,
            })
        }
        type = block_type
    } else if (node instanceof ast.VariableDeclaration) {
        const value_type = type_check(node.value, env, {used_in_expression: true})
        if (node.type) {
            const declared_type = env.get(node.type.name, node.span)
            expect_equal_types(declared_type, value_type, node.span)
        }
        env.add(node.name, value_type, node.span)
        type = Type.unit
    } else if (node instanceof ast.FieldAccess) {
        // fixme: support all other types like enums, tuples, bool, numeric types, etc.
        const target_type = expect_struct_type(
            type_check(node.target, env, {used_in_expression: false}),
            node.span,
        )
        const field_type = target_type.field(node.field, node.span)
        type = field_type
    } else if (node instanceof ast.Return) {
        if (node.value) {
            type = type_check(node.value, env, {used_in_expression: true})
        } else {
            type = Type.unit
        }
    } else if (node instanceof ast.FunctionCall) {
        type = function_call(node, env)
    } else if (node instanceof ast.StructInstantiation) {
        type = struct_instantiation(node, env)
    } else if (node instanceof ast.ParenthesizedExpression) {
        type = type_check(node.expression, env, ctx)
    } else if (node instanceof ast.IdentifierReference) {
        type = env.get(node.name, node.span)
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
    if (expected !== got) {
        throw new TypeMismatchError(expected, got, span)
    }
}

function expect_equal_type_signatures(expected: Type, got: Type, span: Span) {
    if (expected.signature !== got.signature) {
        throw new TypeMismatchError(expected, got, span)
    }
}

function expect_function_type(type: Type, span: Span): FunctionType {
    if (!(type instanceof FunctionType)) {
        throw new TypeCheckError(`Expected function type but got ${quote(type.signature)}`, span)
    }
    return type
}

function expect_struct_type(type: Type, span: Span): StructType {
    if (!(type instanceof StructType)) {
        throw new TypeCheckError(`Expected struct type but got ${quote(type.signature)}`, span)
    }
    return type
}

function function_call(node: ast.FunctionCall, env: TypeEnvironment): Type {
    let function_type = expect_function_type(
        type_check(node.target, env, {used_in_expression: true}),
        node.span,
    )
    let parameters = function_type.parameters_without_self
    if (parameters.length !== node.args.length) {
        throw new TypeCheckError(
            `Expected ${function_type.parameters.length} arguments but got ${node.args.length}`,
            node.span,
        )
    }
    const type_arguments = parse_type_arguments(node.type_arguments, env, node.span)
    function_type = function_type.with_type_arguments(type_arguments)
    parameters = function_type.parameters_without_self
    for (let i = 0; i < node.args.length; i++) {
        const arg_type = type_check(node.args[i], env, {used_in_expression: true})
        expect_equal_types(parameters[i], arg_type, node.span)
    }
    return function_type.return_type
}

function struct_instantiation(node: ast.StructInstantiation, env: TypeEnvironment): StructType {
    const type_arguments = parse_type_arguments(node.type_arguments, env, node.span)
    let struct_type = StructType.from_env(node.target_struct_name, env, node.span)
    struct_type = struct_type.with_type_arguments(type_arguments)
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
        if (type_argument.contains_type_variable()) {
            throw new TypeCheckError(
                `Unexpected type variable ${quote(type_argument.signature)}`,
                span,
            )
        }
        result.push(env.get(type_argument.signature, span))
    }
    return result
}

function parse_declarations_and_definitions(block: ast.Block, env: TypeEnvironment) {
    // First we add all types to the environment without parsing their bodies.
    // This is sufficient for type checking and for resolving recursive and forward type definitions.
    const nodes = block.body.filter((x) => x instanceof ast.DeclarationOrDefinition)
    for (const node of nodes) {
        if (node instanceof ast.StructDeclaration) {
            const struct_type = StructType.from_declaration(node, env)
            env.add(struct_type.name, struct_type, node.span)
        } else if (node instanceof ast.FunctionDefinition) {
            const function_type = FunctionType.from_declaration(node.declaration, env)
            env.add(function_type.name, function_type, node.span)
        } else if (node instanceof ast.TraitDeclaration) {
            const trait_type = TraitType.from_declaration(node, env)
            env.add(trait_type.name, trait_type, node.span)
        } else if (node instanceof ast.VariableDeclaration) {
            // Variable declarations are not forward declared.
        } else if (node instanceof ast.ImplDefinition) {
            // Will be evaluated next.
        } else {
            throw new TypeCheckError(
                `Not implemented for node type ${quote(node.constructor.name)}`,
                node.span,
            )
        }
    }

    // Now parse struct fields.
    for (const node of nodes.filter(
        (x) => x instanceof ast.StructDeclaration,
    ) as ast.StructDeclaration[]) {
        parse_struct_fields(node, env)
    }

    // Now parse trait functions.
    for (const node of nodes.filter(
        (x) => x instanceof ast.TraitDeclaration,
    ) as ast.TraitDeclaration[]) {
        parse_trait_functions(node, env)
    }

    // Now we parse the impls.
    for (const node of nodes.filter(
        (x) => x instanceof ast.ImplDefinition,
    ) as ast.ImplDefinition[]) {
        parse_impl(node, env)
    }

    // Now parse all functions.
    for (const node of nodes.filter(
        (x) => x instanceof ast.FunctionDefinition,
    ) as ast.FunctionDefinition[]) {
        const function_type = FunctionType.from_declaration(node.declaration, env)
        parse_function_definition_body(node, function_type, env)
    }
}

function parse_struct_fields(node: ast.StructDeclaration, env: TypeEnvironment) {
    const struct_type = StructType.from_env(node.name, env, node.span)
    const struct_env = struct_type.create_type_environment(env, node.span)
    for (const [name, type] of Object.entries(node.fields)) {
        let field_type = struct_env.get(type.name, type.span)
        const type_parameters = TypeParameters.from_declaration(type.type_parameters, struct_env)
        struct_type.add_field(name, field_type, type_parameters, type.span)
    }
}

function parse_function_definition_body(
    node: ast.FunctionDefinition,
    function_type: FunctionType,
    env: TypeEnvironment,
) {
    const function_env = new TypeEnvironment(env)
    for (const name of function_type.parameter_names) {
        const type = function_type.parameter(name, node.span)
        function_env.add(name, type, node.span)
    }
    const block_type = type_check(node.block, function_env, {used_in_expression: true})
    expect_equal_type_signatures(function_type.return_type, block_type, node.span)
}

function parse_trait_functions(node: ast.TraitDeclaration, env: TypeEnvironment) {
    const trait_type = TraitType.from_env(node.name, env, node.span)
    const trait_env = trait_type.create_type_environment(env, node.span)
    trait_env.add("Self", TypeVariable.Self, node.span)
    for (const method of node.functions) {
        const declaration = method instanceof ast.FunctionDeclaration ? method : method.declaration
        const function_type = FunctionType.from_declaration(declaration, trait_env)
        if (trait_type.data.fields.has(function_type.name)) {
            throw new TypeCheckError(
                `Field ${quote(function_type.name)} already defined in trait ${quote(
                    trait_type.signature,
                )}`,
                method.span,
            )
        }
        trait_type.data.fields.set(function_type.name, function_type)
        if (method instanceof ast.FunctionDefinition) {
            parse_function_definition_body(method, function_type, trait_env)
        }
    }
}

function parse_impl(node: ast.ImplDefinition, env: TypeEnvironment) {
    const struct_type = StructType.from_env(node.target_name, env, node.span)
    const struct_env = new TypeEnvironment(env)
    struct_env.add("Self", struct_type, node.span)
    const type_variables = node.type_parameters.map((p) =>
        TypeVariable.from_declaration(p, struct_env),
    )
    if (type_variables.length !== struct_type.type_variables.length) {
        throw new TypeCheckError(
            `Expected ${struct_type.type_variables.length} type parameters but got ${type_variables.length}`,
            node.span,
        )
    }
    for (let i = 0; i < type_variables.length; i++) {
        struct_env.add(type_variables[i].name, struct_type.type_variables[i], node.span)
    }
    for (const method of node.functions) {
        const function_type = FunctionType.from_declaration(method.declaration, struct_env)
        if (struct_type.data.fields.has(function_type.name)) {
            throw new TypeCheckError(
                `Field ${quote(function_type.name)} already defined in struct ${quote(
                    struct_type.signature,
                )}`,
                method.span,
            )
        }
        const type_parameters = TypeParameters.from_declaration(
            method.declaration.type_parameters,
            struct_env,
        )
        struct_type.add_field(function_type.name, function_type, type_parameters, method.span)
        if (method instanceof ast.FunctionDefinition) {
            parse_function_definition_body(method, function_type, struct_env)
        }
    }
    if (node.trait_name) {
        let trait_type = TraitType.from_env(node.trait_name, struct_env, node.span)
        const type_variable_map = TypeVariable.create_type_variable_map(
            trait_type.type_variables,
            struct_type.type_variables,
        )
        type_variable_map.set(TypeVariable.Self, struct_type)
        for (let [name, type] of trait_type.fields) {
            assert(type instanceof FunctionType)
            if (!struct_type.fields.has(name)) {
                // See, if we have a default implementation.
                const default_impl = trait_type.data.methods_with_default_impl.includes(name)
                if (!default_impl) {
                    throw new TypeCheckError(
                        `Method ${quote(name)} is not implemented in struct ${quote(
                            struct_type.signature,
                        )}`,
                        node.span,
                    )
                }
                // Add the default impl to the struct.
                type = type.with_scope_type_variables(type_variable_map)
                assert(type instanceof FunctionType)
                type = struct_type.add_field_immediate(name, type)
            } else {
                type = type.with_scope_type_variables(type_variable_map)
            }
            const struct_field_type = struct_type.field(name, node.span)
            expect_equal_type_signatures(type, struct_field_type, node.span)
        }
    }
}

class TypeCheckError extends Error {
    constructor(
        public message: string,
        public span: Span,
    ) {
        super(`${message} at ${span.toString()}`)
    }

    toString() {
        return this.message
    }
}

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
    static bool = new Type("bool")

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

    with_type_arguments(_type_arguments: Type[]): Type {
        throw new Error(`Cannot add type arguments to ${quote(this.signature)}`)
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
            // if (!type_argument) {
            //     throw new TypeCheckError(
            //         `Missing type argument for ${quote(type_variable.signature)}`,
            //         builtin_span,
            //     )
            // }
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
 * Parse something like Num<V> into a type parameter.
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

abstract class ComplexType<T extends {name: string; fields: Map<string, Type>}> extends Type {
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

    with_type_arguments(type_arguments: (Type | null)[]): this {
        if (type_arguments.length !== this.type_variables.length) {
            throw new TypeCheckError(
                `Expected ${this.type_variables.length} type arguments but got ${type_arguments.length}`,
                builtin_span,
            )
        }
        const new_type_arguments = new Map<TypeVariable, Type>(this.type_arguments)
        for (let i = 0; i < this.type_variables.length; i++) {
            const type_variable = this.type_variables[i]
            const type_argument = type_arguments[i]
            if (!type_argument) {
                throw new TypeCheckError(
                    `Missing type argument for ${quote(type_variable.signature)}`,
                    builtin_span,
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

    protected get type_signature(): string {
        const type_variables = this.type_variables.map((v) => this.type_variable_map.get(v) ?? v)
        return TypeParameter.to_str(type_variables, this.type_arguments)
    }

    protected get fields(): Map<string, Type> {
        if (this._fields) {
            return this._fields
        }
        this._fields = new Map()
        for (let [name, type] of this.data.fields) {
            type = type.resolve_type_variables(this.type_arguments)
            this._fields.set(name, type)
        }
        return this._fields
    }

    protected field(name: string, span: Span): Type {
        const field = this.fields.get(name)
        if (!field) {
            throw new TypeCheckError(
                `Unknown field ${quote(name)} in ${quote(this.signature)}`,
                span,
            )
        }
        return field
    }

    protected field_signature(type: Type): string {
        // Respect `this.type_variable_map` here.
        if (type instanceof TypeVariable) {
            type = this.type_variable_map.get(type) ?? type
        }
        return type.signature
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

    get fields() {
        return super.fields
    }

    field(name: string, span: Span): Type {
        return super.field(name, span)
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
        public name: string,
        public fields: Map<string, Type> = new Map(),
    ) {}
}

export class TraitType extends ComplexType<TraitData> {
    static from_declaration(declaration: ast.TraitDeclaration, env: TypeEnvironment): TraitType {
        const type_variables = declaration.type_parameters.map((p) =>
            TypeVariable.from_declaration(p, env),
        )
        const methods_with_default_impl = declaration.functions
            .filter((f) => f instanceof ast.FunctionDefinition)
            .map((f) => f.name)
        const struct_data = new TraitData(declaration.name, new Map(), methods_with_default_impl)
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

    get fields() {
        return super.fields
    }

    field(name: string, span: Span): Type {
        return super.field(name, span)
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
}

class TraitData {
    constructor(
        public name: string,
        public fields: Map<string, Type> = new Map(),
        public methods_with_default_impl: string[],
    ) {}
}

export class FunctionType extends ComplexType<FunctionData> {
    static from_declaration(
        declaration: ast.FunctionDeclaration,
        env: TypeEnvironment,
    ): FunctionType {
        const type_variables = declaration.type_parameters.map((p) =>
            TypeVariable.from_declaration(p, env),
        )
        const function_env = new TypeEnvironment(env)
        for (const type_variable of type_variables) {
            function_env.add(type_variable.name, type_variable, declaration.span)
        }
        function handle_complex_type(type: Type): Type {
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
        const fields = new Map<string, Type>()
        for (const parameter of declaration.parameters) {
            let type = function_env.get(parameter.type.name, parameter.type.span)
            type = handle_complex_type(type)
            fields.set(parameter.name, type)
        }
        let return_type = handle_complex_type(
            function_env.get(declaration.return_type.name, declaration.return_type.span),
        )
        fields.set("return", return_type)
        const data = new FunctionData(
            declaration.name,
            fields,
            declaration.parameters.map((p) => p.name),
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

    is_method(): boolean {
        return this.data.parameter_names[0] === "self"
    }

    get parameters_without_self() {
        if (!this.is_method()) {
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
        const parameters = this.parameters.map((p) => this.field_signature(p)).join(",")
        const return_type = this.field_signature(this.return_type)
        return `${this.name}${this.type_signature}(${parameters})->${return_type}`
    }
}

class FunctionData extends Type {
    constructor(
        public readonly name: string,
        public readonly fields: Map<string, Type>,
        public readonly parameter_names: string[],
    ) {
        super(name)
    }
}

export class NumericType extends ComplexType<NumericData> {
    static i32 = new NumericType("i32")

    private constructor(name: string) {
        super(new NumericData(name, new Map()), [])
        if (Type.known_types.has(name)) {
            throw new Error(`Type ${quote(name)} already defined`)
        }
        Type.known_types.set(name, this)
    }

    new_instance(): this {
        return this
    }
}

class NumericData {
    constructor(
        public name: string,
        public fields: Map<string, Type>,
    ) {}
}

const builtin_span = new Span(0, 0, "<builtin>", "")

export class TypeEnvironment {
    static global() {
        const env = new TypeEnvironment()
        env.add(NumericType.i32.name, NumericType.i32, builtin_span)
        env.add(Type.unit.name, Type.unit, builtin_span)
        env.add(Type.bool.name, Type.bool, builtin_span)
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
}

// Tests

const test = {
    parse(src: string) {
        Type.clear_known_types()
        return parse(new TokenStream(lexer({src, file: "test"})))
    },

    type_check(src: string): {type: Type; env: TypeEnvironment} {
        const ast = test.parse(src)
        const env = TypeEnvironment.global()
        return {type: type_check_ast(ast, env), env}
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
            /Expected `bool \(Type\)` but got `i32 \(NumericType\)`/,
        )
    },

    test_identifier_reference_variable() {
        const {type} = test.type_check("let a = 1 a")
        assert.strictEqual(type, NumericType.i32)
    },

    test_identifier_reference_unknown_variable() {
        assert.throws(() => test.type_check("a"), /Unknown `a` in type environment/)
    },

    test_identifier_reference_simple_type() {
        const {type} = test.type_check("i32")
        assert.strictEqual(type, NumericType.i32)
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
            /Expected `i32 \(NumericType\)` but got `bool \(Type\)`/,
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

    test_method_call() {
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
                    self
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
                    self
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

    test_trait_impl_missing_method() {
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
            /Method `foo` is not implemented/,
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
            /Expected `bool \(Type\)` but got `i32 \(NumericType\)`/,
        )
    },

    test_if_with_branch_type_mismatch() {
        assert.throws(
            () => test.type_check("let a = if true => 1 else => true"),
            /Expected `i32 \(NumericType\)` but got `bool \(Type\)`/,
        )
    },

    test_if_with_branch_type_mismatch_when_used_as_condition_in_if() {
        assert.throws(
            () => test.type_check("if (if true => 1 else => true) => 1 else => 2"),
            /Expected `i32 \(NumericType\)` but got `bool \(Type\)`/,
        )
    },

    test_if_with_branch_type_mismatch_when_used_in_function_call() {
        assert.throws(
            () => test.type_check("fn foo(a i32): end foo(if true => 1 else => true)"),
            /Expected `i32 \(NumericType\)` but got `bool \(Type\)`/,
        )
    },

    test_if_with_branch_type_mismatch_when_used_in_struct_instantiation() {
        assert.throws(
            () =>
                test.type_check(`
            struct Foo: a i32 end
            Foo{a: if true => 1 else => true}
        `),
            /Expected `i32 \(NumericType\)` but got `bool \(Type\)`/,
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
            /Expected `i32 \(NumericType\)` but got `bool \(Type\)`/,
        )
    },

    test_if_branch_types_do_not_have_to_match_if_expression_is_not_used() {
        const {type} = test.type_check(`
            if true => 1 else => true
            1 -- We add a this expression so the if expression is 
              -- not the last expression in the block.
        `)
        assert.equal(type, NumericType.i32)
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
        const {type} = test.type_check(`
            fn foo() i32: 
                return 1
            end
            foo()
        `)
        assert.equal(type, NumericType.i32)
    },

    test_if_with_branch_type_mismatch_when_used_in_return() {
        assert.throws(
            () =>
                test.type_check(`
            fn foo() i32:
                return if true => 1 else => true
            end
        `),
            /Expected `i32 \(NumericType\)` but got `bool \(Type\)`/,
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