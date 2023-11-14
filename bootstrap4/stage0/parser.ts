import {HasKindAndSpan, quote, Span} from "./common"
import {Identifier, LexicalToken, NumberToken, Token} from "./lexer"

export class TokenStream {
    index = 0

    constructor(public tokens: Token[]) {}

    at_end() {
        return this.index >= this.tokens.length
    }

    peek(): Token | undefined {
        return this.tokens[this.index]
    }

    consume() {
        if (this.at_end()) {
            throw new ParseError("Unexpected EOF", this.tokens[this.index - 1]?.span)
        }
        return this.tokens[this.index++]
    }

    expect_identifier() {
        const token = this.peek()
        if (!token) {
            throw new ParseError(
                "Expected identifier but got EOF",
                this.tokens[this.index - 1]?.span,
            )
        }
        if (!(token instanceof Identifier)) {
            throw new ParseError(
                `Expected identifier but got ${quote(token.toString())}`,
                token.span,
            )
        }
        return this.consume() as Identifier
    }

    expect(token_kind: typeof LexicalToken.prototype.value) {
        let actual = this.peek()
        if (!actual) {
            throw new ParseError(
                `Expected token ${quote(token_kind)} but got EOF`,
                this.tokens[this.index - 1]?.span,
            )
        }
        if (!(actual instanceof LexicalToken) || actual.value !== token_kind) {
            throw new ParseError(
                `Expected token ${quote(token_kind)} but got ${quote(actual.toString())}`,
                actual.span,
            )
        }
        return this.consume()
    }

    simple_peek() {
        const token = this.peek()
        return token instanceof LexicalToken ? token.value : undefined
    }
}

export class ParseError extends Error {
    constructor(
        public error: string,
        public span: Span,
    ) {
        super(`${error} at ${span}`)
    }
}

export class Expression extends HasKindAndSpan {
    kind = "expression"
    constructor(span: Span) {
        super(span)
    }
}

export class FunctionDefinition extends Expression {
    kind = "function definition"
    signature: FunctionSignature
    block: Block
    extern?: boolean
    transpile?: (fn: FunctionCall, args: string[]) => string

    constructor(
        data: {
            signature: FunctionSignature
            block: Block
            transpile?: (fn: FunctionCall, args: string[]) => string
            extern?: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionDefinition.prototype)
    }

    to_signature_string() {
        return this.signature.to_signature_string()
    }
}

export class TypeDefinition extends Expression {
    kind = "type definition"
    name: string
    members: Record<string, Type>
    type_parameters: Type[]
    impls: ImplDefinition[]
    extern?: boolean

    constructor(
        data: {
            name: string
            members: Record<string, Type>
            type_parameters: Type[]
            impls: ImplDefinition[]
            extern?: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof TypeDefinition.prototype)
    }

    to_signature_string() {
        if (this.type_parameters.length === 0) {
            return this.name
        }
        return `${this.name}<${this.type_parameters.map((t) => t.name).join(", ")}>`
    }
}

export class Type extends HasKindAndSpan {
    kind = "type"
    name: string
    transpile?: (value: any) => string
    is_type_parameter: boolean
    // The type parameters can differ from this.definition.type_parameters.
    type_parameters: Type[]
    definition: TypeDefinition

    constructor(
        data: {
            name: string
            transpile?: (value: any) => string
            is_type_parameter: boolean
            type_parameters: Type[]
            definition: TypeDefinition
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof Type.prototype)
    }

    to_signature_string(): string {
        return this.definition.to_signature_string()
    }
}

export class Return extends Expression {
    kind = "return"
    value: Expression

    constructor(data: {value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Return.prototype)
    }
}
export class Number_ extends Expression {
    kind = "number"
    value: string

    constructor(data: {value: string}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Number_.prototype)
    }
}

export class Bool extends Expression {
    kind = "bool"
    value: boolean

    constructor(data: {value: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Bool.prototype)
    }
}

export class Not extends Expression {
    kind = "not"
    value: Expression

    constructor(data: {value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Not.prototype)
    }
}

export class FunctionCall extends Expression {
    kind = "function call"
    target: Expression
    arguments: Expression[]

    constructor(data: {target: Expression; arguments: Expression[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionCall.prototype)
    }
}

export class StructInstantiation extends Expression {
    kind = "struct instantiation"
    type_arguments: Type[]
    target: TypeDefinition
    values: Record<string, Expression>

    constructor(
        data: {
            type_arguments: Type[]
            target: TypeDefinition
            values: Record<string, Expression>
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof StructInstantiation.prototype)
    }
}

export class VariableDeclaration extends Expression {
    kind = "variable declaration"
    name: string
    value: Expression
    type: Type
    mutable: boolean

    constructor(
        data: {
            name: string
            value: Expression
            type: Type
            mutable: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof VariableDeclaration.prototype)
    }
}

export class Assignment extends Expression {
    kind = "assignment"
    target: Variable | FieldAccess
    value: Expression

    constructor(data: {target: Variable | FieldAccess; value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Assignment.prototype)
    }
}

export class FieldAccess extends Expression {
    kind = "field access"
    object: Expression
    field: string

    constructor(data: {object: Expression; field: string}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FieldAccess.prototype)
    }
}

export class NamespaceAccess extends Expression {
    kind = "namespace access"
    namespace: Namespace

    constructor(data: {namespace: Namespace}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof NamespaceAccess.prototype)
    }
}

type BinaryOperator = "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "and" | "or"

export class BinaryExpression extends Expression {
    static precedence: Record<BinaryOperator, number> = {
        or: 1,
        and: 2,
        "==": 3,
        "!=": 3,
        "<": 4,
        "<=": 4,
        ">": 4,
        ">=": 4,
        "+": 5,
        "-": 5,
        "*": 6,
        "/": 6,
    }
    kind = "binary"
    operator: BinaryOperator
    lhs: Expression
    rhs: Expression

    constructor(data: {operator: BinaryOperator; lhs: Expression; rhs: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof BinaryExpression.prototype)
    }
}

export class If extends Expression {
    kind = "if"
    condition: Expression
    then_block: Block
    else_block?: Block

    constructor(data: {condition: Expression; then_block: Block; else_block?: Block}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof If.prototype)
    }
}

export class Loop extends Expression {
    kind = "loop"
    block: Block

    constructor(data: {block: Block}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Loop.prototype)
    }
}

export class Break extends Expression {
    kind = "break"

    constructor(span: Span) {
        super(span)
    }
}

export class Continue extends Expression {
    kind = "continue"

    constructor(span: Span) {
        super(span)
    }
}

export class UnitTypeExpression extends Expression {
    kind = "unit-type"

    constructor(span: Span) {
        super(span)
    }
}

export class Parameter extends HasKindAndSpan {
    kind = "parameter"
    name: string
    type: Type
    mutable: boolean

    constructor(data: {name: string; type: Type; mutable: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Parameter.prototype)
    }
}

export class Block extends HasKindAndSpan {
    kind = "block"
    body: Expression[]

    constructor(data: {body: Expression[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Block.prototype)
    }
}

export class FunctionSignature extends HasKindAndSpan {
    kind = "function signature"
    name: string
    parameters: Parameter[]
    return_type: Type

    constructor(data: {name: string; parameters: Parameter[]; return_type: Type}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionSignature.prototype)
    }

    to_signature_string() {
        return `${this.name}(${this.parameters
            .map((p) => `${p.name}: ${p.type.to_signature_string()}`)
            .join(", ")}): ${this.return_type.to_signature_string()}`
    }
}

export class ImplDefinition extends HasKindAndSpan {
    kind = "impl_definition"
    trait?: TraitDefinition
    type: TypeDefinition
    member_functions: FunctionDefinition[]
    static_functions: FunctionDefinition[]
    extern?: boolean

    constructor(
        data: {
            trait?: TraitDefinition
            type: TypeDefinition
            member_functions: FunctionDefinition[]
            static_functions: FunctionDefinition[]
            extern?: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof ImplDefinition.prototype)
    }
}

export class TraitDefinition extends HasKindAndSpan {
    kind = "trait_definition"
    name: string
    member_function_signatures: FunctionSignature[]
    static_function_signatures: FunctionSignature[]
    member_function_default_impls: Record<string, FunctionDefinition>
    static_function_default_impls: Record<string, FunctionDefinition>
    type_parameters: Type[]
    extern?: boolean

    constructor(
        data: {
            name: string
            member_function_signatures: FunctionSignature[]
            static_function_signatures: FunctionSignature[]
            member_function_default_impls: Record<string, FunctionDefinition>
            static_function_default_impls: Record<string, FunctionDefinition>
            type_parameters: Type[]
            extern?: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof TraitDefinition.prototype)
    }

    to_signature_string() {
        if (this.type_parameters.length === 0) {
            return this.name
        }
        return `${this.name}<${this.type_parameters
            .map((t) => t.to_signature_string())
            .join(", ")}>`
    }
}

export class Namespace extends HasKindAndSpan {
    kind = "namespace"
    name: string
    functions: Record<string, FunctionDefinition>

    constructor(data: {name: string; functions: Record<string, FunctionDefinition>}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Namespace.prototype)
    }
}

export const builtin_types: Record<string, Type> = {
    i32: new Type(
        {
            name: "i32",
            transpile: (value: any) => value,
            is_type_parameter: false,
            type_parameters: [],
            definition: new TypeDefinition(
                {name: "i32", members: {}, type_parameters: [], impls: []},
                new Span(0, 0, "<builtin>", ""),
            ),
        },
        new Span(0, 0, "<builtin>", ""),
    ),
    bool: new Type(
        {
            name: "bool",
            transpile: (value: any) => value,
            is_type_parameter: false,
            type_parameters: [],
            definition: new TypeDefinition(
                {name: "bool", members: {}, type_parameters: [], impls: []},
                new Span(0, 0, "<builtin>", ""),
            ),
        },
        new Span(0, 0, "<builtin>", ""),
    ),
    unit_type: new Type(
        {
            name: "unit_type",
            transpile: () => "undefined",
            is_type_parameter: false,
            type_parameters: [],
            definition: new TypeDefinition(
                {name: "()", members: {}, type_parameters: [], impls: []},
                new Span(0, 0, "<builtin>", ""),
            ),
        },
        new Span(0, 0, "<builtin>", ""),
    ),
    Self: new Type(
        {
            name: "Self",
            transpile: () => "undefined",
            is_type_parameter: false,
            type_parameters: [],
            definition: new TypeDefinition(
                {name: "Self", members: {}, type_parameters: [], impls: []},
                new Span(0, 0, "<builtin>", ""),
            ),
        },
        new Span(0, 0, "<builtin>", ""),
    ),
}

// Environment

export class Variable extends HasKindAndSpan {
    kind = "variable"
    name: string
    type: Type
    mutable: boolean

    constructor(
        data: {
            name: string
            type: Type
            mutable: boolean
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof Variable.prototype)
    }
}

export class Environment {
    functions: Record<string, FunctionDefinition> = {}
    variables: Record<string, Variable> = {}
    types: Record<string, Type> = {}
    traits: Record<string, TraitDefinition> = {}
    namespaces: Record<string, Namespace> = {}
    outer?: Environment

    find<T extends HasKindAndSpan>(
        kind: "functions" | "variables" | "types" | "traits" | "namespaces",
        name: string,
    ): T | undefined {
        if (this[kind][name]) {
            return this[kind][name] as any as T
        }
        if (this.outer) {
            return this.outer.find(kind, name)
        }
        return undefined
    }

    expect<T extends HasKindAndSpan>(
        kind: "functions" | "variables" | "types" | "traits" | "namespaces",
        name: string,
        span: Span,
    ): T {
        const value = this.find<T>(kind, name)
        if (!value) {
            throw new ParseError(`Unknown ${kind.slice(0, -1)} ${quote(name)}`, span)
        }
        return value
    }

    set_types(types: Record<string, Type> | Type[]) {
        if (Array.isArray(types)) {
            this.types = {}
            for (const type of types) {
                this.types[type.name] = type
            }
        } else {
            this.types = types
        }
    }

    find_function(name: string): FunctionDefinition | undefined {
        return this.find<FunctionDefinition>("functions", name)
    }

    find_variable(name: string): Variable | undefined {
        return this.find<Variable>("variables", name)
    }

    find_type(name: string): Type | undefined {
        return this.find<Type>("types", name)
    }

    find_trait(name: string): TraitDefinition | undefined {
        return this.find<TraitDefinition>("traits", name)
    }

    find_namespace(name: string): Namespace | undefined {
        return this.find<Namespace>("namespaces", name)
    }

    expect_function(name: string, span: Span): FunctionDefinition {
        return this.expect<FunctionDefinition>("functions", name, span)
    }

    expect_variable(name: string, span: Span): Variable {
        return this.expect<Variable>("variables", name, span)
    }

    expect_type(name: string, span: Span): Type {
        return this.expect<Type>("types", name, span)
    }

    expect_trait(name: string, span: Span): TraitDefinition {
        return this.expect<TraitDefinition>("traits", name, span)
    }

    expect_namespace(name: string, span: Span): Namespace {
        return this.expect<Namespace>("namespaces", name, span)
    }
}

export type AST = Expression[]

type Ctx = {
    env: Environment
    tokens: TokenStream
}

/**
 * Parse tokens into an AST.
 */
export function parse(tokens: TokenStream, env: Environment): AST {
    const ast: AST = []
    const ctx = {env, tokens}
    run_in_new_scope(ctx, () => {
        while (!ctx.tokens.at_end()) {
            ast.push(parse_expression(ctx))
        }
    })
    return ast
}

function run_in_new_scope<T>(ctx: Ctx, fn: (ctx: Ctx) => T): T {
    const new_env = new Environment()
    new_env.outer = ctx.env
    return fn({...ctx, env: new_env})
}

/**
 * Parse an expression and respect binary operator precedence.
 */
function parse_expression(ctx: Ctx): Expression {
    function parse_binary_expression(min_precedence: number) {
        let lhs = parse_primary(ctx)
        while (true) {
            const op = ctx.tokens.simple_peek()
            const op_precedence = BinaryExpression.precedence[op as BinaryOperator]
            if (!op_precedence) {
                if (op === "=") {
                    return parse_assignment(lhs, ctx)
                }
                return lhs
            }
            if (op_precedence >= min_precedence) {
                ctx.tokens.consume()
                // Parse the right-hand side of the expression
                // with a higher precedence than this one.
                // This way, we can parse expressions like
                // `1 + 2 * 3` as `1 + (2 * 3)` instead of
                // `(1 + 2) * 3`.
                let rhs = parse_binary_expression(op_precedence + 1)
                lhs = new BinaryExpression(
                    {operator: op as BinaryOperator, lhs, rhs},
                    Span.combine(lhs, rhs),
                )
            } else {
                return lhs
            }
        }
    }
    return parse_binary_expression(1)
}

function parse_primary(ctx: Ctx): Expression {
    const token = ctx.tokens.peek()
    if (!token) {
        throw new ParseError(`Unexpected EOF`, ctx.tokens.tokens[ctx.tokens.index - 1]?.span)
    }
    let expression: Expression | undefined
    if (token instanceof LexicalToken) {
        const simple_token = token.value
        if (simple_token === "fn") {
            expression = parse_function_definition("no_impl", ctx)
        } else if (simple_token === "let" || simple_token === "mut") {
            expression = parse_variable_declaration(ctx)
        } else if (simple_token === "true" || simple_token === "false") {
            const span = ctx.tokens.consume().span
            expression = new Bool({value: simple_token === "true"}, span)
        } else if (simple_token === "not") {
            const span = ctx.tokens.consume().span
            const value = parse_expression(ctx)
            expression = new Not({value}, Span.combine(span, value))
        } else if (simple_token === "if") {
            expression = parse_if(ctx)
        } else if (simple_token === "loop") {
            expression = parse_loop(ctx)
        } else if (simple_token === "(") {
            ctx.tokens.consume()
            expression = parse_expression(ctx)
            ctx.tokens.expect(")")
        } else if (simple_token === "break") {
            const span = ctx.tokens.consume().span
            expression = new Break(span)
        } else if (simple_token === "continue") {
            const span = ctx.tokens.consume().span
            expression = new Continue(span)
        } else if (simple_token === "extern") {
            expression = parse_extern_block(ctx)
        } else if (simple_token === "struct") {
            expression = parse_type_definition(ctx)
        } else if (simple_token === "trait") {
            const trait = parse_trait_definition(ctx)
            expression = new UnitTypeExpression(trait.span)
        } else if (simple_token === "impl") {
            const impl = parse_impl("all", ctx)
            expression = new UnitTypeExpression(impl.span)
        } else if (simple_token === "return") {
            expression = parse_return_expression(ctx)
        }
    } else if (token instanceof NumberToken) {
        ctx.tokens.consume()
        expression = new Number_({value: token.value}, token.span)
    } else if (token instanceof Identifier) {
        const token = ctx.tokens.expect_identifier()
        expression =
            try_parse_function_call(token, ctx) ||
            try_parse_variable(token, ctx) ||
            try_parse_struct_initialization(token, ctx) ||
            try_parse_namespace_access(token, ctx)
        if (!expression) {
            throw new ParseError(`Unknown identifier ${quote(token.value)}`, token.span)
        }
    }
    if (!expression) {
        throw new ParseError(`Unexpected token ${quote(token)}`, token.span)
    }
    while (true) {
        if (ctx.tokens.simple_peek() === "(") {
            expression = try_parse_function_call(expression, ctx) || expression
        } else if (ctx.tokens.simple_peek() === ".") {
            expression = parse_field_access(expression, ctx)
        } else {
            break
        }
    }
    return expression
}

function parse_assignment(target: Expression, ctx: Ctx): Assignment {
    if (!(target instanceof Variable || target instanceof FieldAccess)) {
        throw new ParseError(`${quote(target.kind)} cannot be assigned a value`, target.span)
    }
    const span = ctx.tokens.consume().span
    const value = parse_expression(ctx)
    return new Assignment({target, value}, Span.combine(span, value))
}

function parse_field_access(target: Expression, ctx: Ctx): Expression {
    while (ctx.tokens.simple_peek() === ".") {
        const span = ctx.tokens.consume().span
        const field = ctx.tokens.expect_identifier()
        target = new FieldAccess({object: target, field: field.value}, Span.combine(span, field))
    }
    return target
}

function parse_extern_block(ctx: Ctx): UnitTypeExpression {
    const span = ctx.tokens.consume().span
    ctx.tokens.expect(":")
    while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== "end") {
        const type = ctx.tokens.simple_peek()
        if (type === "fn") {
            const signature = parse_function_signature("no_impl", ctx)
            ctx.env.functions[signature.name] = new FunctionDefinition(
                {
                    signature,
                    block: new Block({body: []}, signature.span),
                    extern: true,
                },
                signature.span,
            )
        } else if (type === "struct") {
            const struct = parse_type_definition(ctx)
            struct.extern = true
        } else if (type === "impl") {
            const impl = parse_impl("signatures_only", ctx)
            impl.extern = true
        } else {
            throw new ParseError(`Unknown ${quote("extern")} type ${quote(type)}`, span)
        }
    }
    const end_span = ctx.tokens.expect("end").span
    return new UnitTypeExpression(Span.combine(span, end_span))
}

function parse_impl(mode: "signatures_only" | "all", ctx: Ctx): ImplDefinition {
    const span = ctx.tokens.consume().span
    const name_or_trait = ctx.tokens.expect_identifier().value
    let type_parameters = try_parse_generic_type_parameters(ctx)
    let trait: TraitDefinition | undefined
    let target_name = name_or_trait
    if (ctx.tokens.simple_peek() === "for") {
        ctx.tokens.consume()
        target_name = ctx.tokens.expect_identifier().value
        trait = ctx.env.expect_trait(name_or_trait, span)
        trait = new TraitDefinition({...trait}, trait.span)
        type_parameters = try_parse_generic_type_parameters(ctx)
    }
    const target_type = ctx.env.expect_type(target_name, span)
    const target = target_type.definition
    const types: Record<string, Type> = type_parameters.reduce(
        (acc, type) => {
            acc[type.name] = type
            return acc
        },
        {} as Record<string, Type>,
    )
    ctx.tokens.expect(":")
    const member_functions: FunctionDefinition[] = []
    const static_functions: FunctionDefinition[] = []
    run_in_new_scope(ctx, (ctx) => {
        ctx.env.types = types
        while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== "end") {
            let fn: FunctionDefinition
            if (mode === "signatures_only") {
                const signature = parse_function_signature(target_type, ctx)
                fn = new FunctionDefinition(
                    {signature, block: new Block({body: []}, signature.span)},
                    signature.span,
                )
            } else {
                fn = parse_function_definition(target_type, ctx)
            }
            if (fn.signature.parameters[0]?.name !== "self") {
                static_functions.push(fn)
                const namespace = ctx.env.expect_namespace(target.name, target.span)
                namespace.functions[fn.signature.name] = fn
            } else {
                member_functions.push(fn)
            }
        }
    })
    const end_span = ctx.tokens.expect("end").span
    const impl = new ImplDefinition(
        {
            trait,
            type: target,
            member_functions,
            static_functions,
        },
        Span.combine(span, end_span),
    )
    target.impls.push(impl)
    return impl
}

function parse_trait_definition(ctx: Ctx): TraitDefinition {
    const span = ctx.tokens.consume().span
    const name = ctx.tokens.expect_identifier().value
    const type_parameters = try_parse_generic_type_parameters(ctx)
    ctx.tokens.expect(":")
    const trait = new TraitDefinition(
        {
            name,
            member_function_signatures: [],
            static_function_signatures: [],
            member_function_default_impls: {},
            static_function_default_impls: {},
            type_parameters,
        },
        span,
    )
    run_in_new_scope(ctx, (ctx) => {
        ctx.env.set_types(type_parameters)
        while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== "end") {
            // We call this with a dummy type to enable the handling of `self` parameters.
            const signature = parse_function_signature(builtin_types.unit_type, ctx)
            if (![":", "=>"].includes(ctx.tokens.simple_peek() ?? "")) {
                // Ok, this is a signature only.
                if (signature.parameters[0].name !== "self") {
                    trait.static_function_signatures.push(signature)
                } else {
                    trait.member_function_signatures.push(signature)
                }
            } else {
                const fn = parse_function_body(signature, "no_impl", ctx)
                if (fn.signature.parameters[0].name !== "self") {
                    trait.static_function_default_impls[fn.signature.name] = fn
                } else {
                    trait.member_function_default_impls[fn.signature.name] = fn
                }
            }
        }
    })
    const end_span = ctx.tokens.expect("end").span
    trait.span = Span.combine(span, end_span)
    ctx.env.traits[name] = trait
    return trait
}

function parse_type_definition(ctx: Ctx): TypeDefinition {
    let span = ctx.tokens.consume().span
    const name = ctx.tokens.expect_identifier().value
    const members: Record<string, Type> = {}
    const type_parameters = try_parse_generic_type_parameters(ctx)
    span = Span.combine(span, ctx.tokens.expect(":").span)
    run_in_new_scope(ctx, (ctx) => {
        ctx.env.set_types(type_parameters)
        while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== "end") {
            const name = ctx.tokens.expect_identifier().value
            const type = parse_type({}, ctx)
            members[name] = type
        }
    })
    ctx.tokens.expect("end")
    const definition = new TypeDefinition({name, members, type_parameters, impls: []}, span)
    ctx.env.types[name] = new Type(
        {name, definition, is_type_parameter: false, type_parameters},
        span,
    )
    ctx.env.namespaces[name] = new Namespace({name, functions: {}}, span)
    return definition
}

function parse_block(mode: "normal" | "if_else", ctx: Ctx) {
    const block_type = ctx.tokens.consume()
    if (!(block_type instanceof LexicalToken) || !["=>", ":"].includes(block_type.value)) {
        throw new ParseError(
            `Expected start of a block (':' or '=>') but got ${quote(block_type.toString())}`,
            block_type.span,
        )
    }
    const block: Block = new Block({body: []}, block_type.span)
    if (block_type.value === "=>") {
        block.body.push(parse_expression(ctx))
    } else {
        const end_tokens = ["end"]
        if (mode === "if_else") {
            end_tokens.push("else")
        }
        while (!ctx.tokens.at_end() && !end_tokens.includes(ctx.tokens.simple_peek() ?? "")) {
            block.body.push(parse_expression(ctx))
        }
        if (mode === "normal") {
            ctx.tokens.expect("end")
        }
    }
    return block
}

function parse_loop(ctx: Ctx): Loop {
    const span = ctx.tokens.expect("loop").span
    const block = parse_block("normal", ctx)
    return new Loop({block}, Span.combine(span, block.span))
}

function parse_if(ctx: Ctx): If {
    const span = ctx.tokens.expect("if").span
    const condition = parse_expression(ctx)
    const if_ = new If(
        {condition, then_block: parse_block("if_else", ctx)},
        Span.combine(span, condition.span),
    )
    if (ctx.tokens.simple_peek() === "else") {
        ctx.tokens.consume()
        if_.else_block = parse_block("normal", ctx)
    } else if (ctx.tokens.simple_peek() === "end") {
        ctx.tokens.consume()
    }
    return if_
}

function parse_variable_declaration(ctx: Ctx): VariableDeclaration {
    const mutable = ctx.tokens.simple_peek() === "mut"
    const span = ctx.tokens.consume().span
    const name = ctx.tokens.expect_identifier().value
    const token = ctx.tokens.peek()
    let type = builtin_types.unit_type
    if (token instanceof Identifier) {
        type = ctx.env.expect_type(token.value, token.span)
        ctx.tokens.consume()
    }
    ctx.tokens.expect("=")
    const value = parse_expression(ctx)
    const let_ = new VariableDeclaration(
        {name, value, type, mutable},
        Span.combine(span, value.span),
    )
    ctx.env.variables[name] = new Variable({name, type, mutable}, Span.combine(span, value.span))
    return let_
}

function parse_function_signature(impl_type: Type | "no_impl", ctx: Ctx): FunctionSignature {
    const span = ctx.tokens.expect("fn").span
    const name = ctx.tokens.expect_identifier().value
    ctx.tokens.expect("(")
    const parameters: Parameter[] = []
    while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== ")") {
        if (parameters.length > 0) {
            ctx.tokens.expect(",")
        }
        let mutable = false
        if (ctx.tokens.simple_peek() === "mut") {
            mutable = true
            ctx.tokens.consume()
        }
        const name_token = ctx.tokens.expect_identifier()
        if (impl_type !== "no_impl") {
            if (name_token.value === "self") {
                parameters.push(
                    new Parameter({name: "self", type: builtin_types.Self, mutable}, span),
                )
                continue
            }
        }
        const type = parse_type({}, ctx)
        parameters.push(new Parameter({name: name_token.value, type, mutable}, name_token.span))
    }
    ctx.tokens.expect(")")
    let return_type = builtin_types.unit_type
    if (ctx.tokens.peek() instanceof Identifier) {
        return_type = parse_type({}, ctx)
    }
    return new FunctionSignature({name, parameters, return_type}, span)
}

function parse_function_body(
    signature: FunctionSignature,
    impl_type: Type | "no_impl",
    ctx: Ctx,
): FunctionDefinition {
    const fn_def = new FunctionDefinition(
        {signature, block: new Block({body: []}, signature.span)},
        signature.span,
    )
    run_in_new_scope(ctx, (ctx) => {
        if (impl_type === "no_impl") {
            ctx.env.outer!.functions[signature.name] = fn_def
            ctx.env.functions[signature.name] = fn_def
        }
        for (const parameter of fn_def.signature.parameters) {
            ctx.env.variables[parameter.name] = new Variable({...parameter}, parameter.span)
        }
        fn_def.block = parse_block("normal", ctx)
        fn_def.span = Span.combine(signature.span, fn_def.block.span)
    })
    return fn_def
}

function parse_function_definition(impl_type: Type | "no_impl", ctx: Ctx): FunctionDefinition {
    const signature = parse_function_signature(impl_type, ctx)
    return parse_function_body(signature, impl_type, ctx)
}

function parse_return_expression(ctx: Ctx): Return {
    const span = ctx.tokens.expect("return").span
    const value = parse_expression(ctx)
    return new Return({value}, Span.combine(span, value.span))
}

function try_parse_namespace_access(token: Identifier, ctx: Ctx): NamespaceAccess | undefined {
    const namespace = ctx.env.find_namespace(token.value)
    if (!namespace) {
        return
    }
    return new NamespaceAccess({namespace}, token.span)
}

function try_parse_struct_initialization(
    token: Identifier,
    ctx: Ctx,
): StructInstantiation | undefined {
    let type_arguments: Type[] = []
    if (ctx.tokens.simple_peek() === "<") {
        ctx.tokens.consume()
        while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== ">") {
            if (type_arguments.length > 0) {
                ctx.tokens.expect(",")
            }
            const type: Type = parse_type({}, ctx)
            type_arguments.push(type)
        }
        ctx.tokens.expect(">")
    }
    if (ctx.tokens.simple_peek() !== "{") {
        return
    }
    ctx.tokens.consume()
    let target = ctx.env.expect_type(token.value, token.span).definition
    const args: Record<string, Expression> = {}
    while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== "}") {
        if (Object.keys(args).length > 0) {
            ctx.tokens.expect(",")
        }
        const name = ctx.tokens.expect_identifier().value
        ctx.tokens.expect(":")
        const value = parse_expression(ctx)
        args[name] = value
    }
    if (type_arguments.length !== target.type_parameters.length) {
        throw new ParseError(
            `Expected ${target.type_parameters.length} argument types but got ${
                type_arguments.length
            } for type ${quote(target.to_signature_string())}`,
            token.span,
        )
    }
    if (type_arguments.length > 0) {
        // Create a new type with the argument types filled in.
        target = new TypeDefinition({...target}, target.span)
        for (let i = 0; i < type_arguments.length; i++) {
            const type = type_arguments[i]
            if (type.is_type_parameter) {
                target.members[target.type_parameters[i].name] = type
                for (const impl of target.impls) {
                    for (const fn of impl.member_functions) {
                        for (const parameter of fn.signature.parameters) {
                            if (parameter.type.name === type.name) {
                                parameter.type = type
                            }
                        }
                        if (fn.signature.return_type.name === type.name) {
                            fn.signature.return_type = type
                        }
                    }
                }
            }
        }
    }
    const span = Span.combine(ctx.tokens.expect("}").span, token.span)
    return new StructInstantiation({target, values: args, type_arguments}, span)
}

function try_parse_function_call(
    target_candidate: Expression | Token,
    ctx: Ctx,
): FunctionCall | undefined {
    if (ctx.tokens.simple_peek() !== "(") {
        return
    }
    ctx.tokens.consume()
    let target: FunctionDefinition | FieldAccess | NamespaceAccess | undefined
    if (target_candidate instanceof Identifier) {
        target_candidate
        target = ctx.env.find_function(target_candidate.value)
    } else {
        if (
            !(
                target_candidate instanceof FieldAccess ||
                target_candidate instanceof NamespaceAccess
            )
        ) {
            throw new ParseError(
                `${quote(target_candidate.kind)} is not callable`,
                target_candidate.span,
            )
        }
        target = target_candidate
    }
    if (!target) {
        return
    }
    const args = []
    while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== ")") {
        if (args.length > 0) {
            ctx.tokens.expect(",")
        }
        args.push(parse_expression(ctx))
    }
    const span = Span.combine(ctx.tokens.expect(")").span, target_candidate.span)
    return new FunctionCall({target, arguments: args}, span)
}

/**
 * Try to parse `<T, U>` as a type parameter list.
 * Return an empty array if there are no type parameters.
 */
function try_parse_variable(token: Identifier, ctx: Ctx): Variable | undefined {
    return ctx.env.find_variable(token.value)
}

function try_parse_generic_type_parameters(ctx: Ctx): Type[] {
    const type_parameters: Type[] = []
    if (ctx.tokens.simple_peek() === "<") {
        ctx.tokens.consume()
        while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== ">") {
            if (type_parameters.length > 0) {
                ctx.tokens.expect(",")
            }
            const type_parameter = parse_type({expect_only_type_parameters: true}, ctx)
            type_parameters.push(type_parameter)
        }
        ctx.tokens.expect(">")
    }
    return type_parameters
}

function parse_type(opts: {expect_only_type_parameters?: boolean}, ctx: Ctx): Type {
    const token = ctx.tokens.expect_identifier()
    let end_span = token.span
    let base_type = ctx.env.find_type(token.value)
    if (opts.expect_only_type_parameters) {
        base_type =
            ctx.env.find_type(token.value) ??
            new Type(
                {
                    name: token.value,
                    is_type_parameter: true,
                    type_parameters: [],
                    definition: new TypeDefinition(
                        {name: token.value, members: {}, type_parameters: [], impls: []},
                        token.span,
                    ),
                },
                token.span,
            )
    } else {
        base_type = ctx.env.expect_type(token.value, token.span)
    }
    const type_parameters: Type[] = []
    if (ctx.tokens.simple_peek() === "<") {
        ctx.tokens.consume()
        while (!ctx.tokens.at_end() && ctx.tokens.simple_peek() !== ">") {
            if (type_parameters.length > 0) {
                ctx.tokens.expect(",")
            }
            const type_argument = parse_type({}, ctx)
            type_parameters.push(type_argument)
        }
        end_span = ctx.tokens.expect(">").span
    }
    if (opts.expect_only_type_parameters && !base_type.is_type_parameter) {
        throw new ParseError(
            `Expected type parameter but got the concrete type ${quote(base_type.name)}`,
            token.span,
        )
    }
    // fixme: do we really need to always create a new type.
    return new Type(
        {
            ...base_type,
            type_parameters,
        },
        Span.combine(token.span, end_span),
    )
}
