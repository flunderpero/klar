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

export class Type extends HasKindAndSpan {
    kind = "type"
    name: string
    transpile?: (value: any) => string
    members?: Record<string, Type>
    is_type_parameter: boolean
    type_parameters: Type[]

    constructor(
        data: {
            name: string
            transpile?: (value: any) => string
            members?: Record<string, Type>
            is_type_parameter: boolean
            type_parameters: Type[]
        },
        span: Span,
    ) {
        super(span)
        Object.assign(this as typeof data, data as typeof Type.prototype)
    }

    to_signature_string(): string {
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
        },
        new Span(0, 0, "<builtin>", ""),
    ),
    bool: new Type(
        {
            name: "bool",
            transpile: (value: any) => value,
            is_type_parameter: false,
            type_parameters: [],
        },
        new Span(0, 0, "<builtin>", ""),
    ),
    unit_type: new Type(
        {
            name: "unit_type",
            transpile: () => "undefined",
            is_type_parameter: false,
            type_parameters: [],
        },
        new Span(0, 0, "<builtin>", ""),
    ),
    Self: new Type(
        {
            name: "Self",
            transpile: () => "undefined",
            is_type_parameter: false,
            type_parameters: [],
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
    type_definitions: Record<string, TypeDefinition> = {}
    traits: Record<string, TraitDefinition> = {}
    namespaces: Record<string, Namespace> = {}
    outer?: Environment

    find<T extends HasKindAndSpan>(
        kind: "functions" | "variables" | "types" | "type_definitions" | "traits" | "namespaces",
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
        kind: "functions" | "variables" | "types" | "type_definitions" | "traits" | "namespaces",
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

    find_type_definition(name: string): TypeDefinition | undefined {
        return this.find<TypeDefinition>("type_definitions", name)
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

    expect_type_definition(name: string, span: Span): TypeDefinition {
        return this.expect<TypeDefinition>("type_definitions", name, span)
    }

    expect_trait(name: string, span: Span): TraitDefinition {
        return this.expect<TraitDefinition>("traits", name, span)
    }

    expect_namespace(name: string, span: Span): Namespace {
        return this.expect<Namespace>("namespaces", name, span)
    }
}

export type AST = Expression[]

/**
 * Parse tokens into an AST.
 */
export function parse(tokens: TokenStream, env: Environment): AST {
    function run_in_sub_env<T>(fn: () => T): T {
        const new_env = new Environment()
        new_env.outer = env
        env = new_env
        try {
            return fn()
        } finally {
            env = env.outer!
        }
    }
    const ast: AST = []
    while (!tokens.at_end()) {
        ast.push(parse_expression())
    }
    /**
     * Parse an expression and respect binary operator precedence.
     */
    function parse_expression(): Expression {
        function parse_binary_expression(min_precedence: number) {
            let lhs = parse_primary()
            while (true) {
                const op = tokens.simple_peek()
                const op_precedence = BinaryExpression.precedence[op as BinaryOperator]
                if (!op_precedence) {
                    if (op === "=") {
                        return parse_assignment(lhs)
                    }
                    return lhs
                }
                if (op_precedence >= min_precedence) {
                    tokens.consume()
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
    function parse_primary(): Expression {
        const token = tokens.peek()
        if (!token) {
            throw new ParseError(`Unexpected EOF`, tokens.tokens[tokens.index - 1]?.span)
        }
        let expression: Expression | undefined
        if (token instanceof LexicalToken) {
            const simple_token = token.value
            if (simple_token === "fn") {
                expression = parse_function_definition()
            } else if (simple_token === "let" || simple_token === "mut") {
                expression = parse_variable_declaration()
            } else if (simple_token === "true" || simple_token === "false") {
                const span = tokens.consume().span
                expression = new Bool({value: simple_token === "true"}, span)
            } else if (simple_token === "not") {
                const span = tokens.consume().span
                const value = parse_expression()
                expression = new Not({value}, Span.combine(span, value))
            } else if (simple_token === "if") {
                expression = parse_if()
            } else if (simple_token === "loop") {
                expression = parse_loop()
            } else if (simple_token === "(") {
                tokens.consume()
                expression = parse_expression()
                tokens.expect(")")
            } else if (simple_token === "break") {
                const span = tokens.consume().span
                expression = new Break(span)
            } else if (simple_token === "continue") {
                const span = tokens.consume().span
                expression = new Continue(span)
            } else if (simple_token === "extern") {
                expression = parse_extern_block()
            } else if (simple_token === "struct") {
                expression = parse_type_definition()
            } else if (simple_token === "trait") {
                const trait = parse_trait_definition()
                expression = new UnitTypeExpression(trait.span)
            } else if (simple_token === "impl") {
                const impl = parse_impl()
                expression = new UnitTypeExpression(impl.span)
            } else if (simple_token === "return") {
                expression = parse_return_expression()
            }
        } else if (token instanceof NumberToken) {
            tokens.consume()
            expression = new Number_({value: token.value}, token.span)
        } else if (token instanceof Identifier) {
            const token = tokens.expect_identifier()
            expression =
                try_parse_function_call(token) ||
                try_parse_variable(token) ||
                try_parse_struct_initialization(token) ||
                try_parse_namespace_access(token)
            if (!expression) {
                throw new ParseError(`Unknown identifier ${quote(token.value)}`, token.span)
            }
        }
        if (!expression) {
            throw new ParseError(`Unexpected token ${quote(token)}`, token.span)
        }
        while (true) {
            if (tokens.simple_peek() === "(") {
                expression = try_parse_function_call(expression) || expression
            } else if (tokens.simple_peek() === ".") {
                expression = parse_field_access(expression)
            } else {
                break
            }
        }
        return expression
    }
    function parse_assignment(target: Expression): Assignment {
        if (!(target instanceof Variable || target instanceof FieldAccess)) {
            throw new ParseError(`${quote(target.kind)} cannot be assigned a value`, target.span)
        }
        const span = tokens.consume().span
        const value = parse_expression()
        return new Assignment({target, value}, Span.combine(span, value))
    }
    function parse_field_access(target: Expression): Expression {
        while (tokens.simple_peek() === ".") {
            const span = tokens.consume().span
            const field = tokens.expect_identifier()
            target = new FieldAccess(
                {object: target, field: field.value},
                Span.combine(span, field),
            )
        }
        return target
    }
    function parse_extern_block(): UnitTypeExpression {
        const span = tokens.consume().span
        tokens.expect(":")
        while (!tokens.at_end() && tokens.simple_peek() !== "end") {
            const type = tokens.simple_peek()
            if (type === "fn") {
                const signature = parse_function_signature()
                env.functions[signature.name] = new FunctionDefinition(
                    {
                        signature,
                        block: new Block({body: []}, signature.span),
                        extern: true,
                    },
                    signature.span,
                )
            } else if (type === "struct") {
                const struct = parse_type_definition()
                struct.extern = true
            } else if (type === "impl") {
                const impl = parse_impl("signatures_only")
                impl.extern = true
            } else {
                throw new ParseError(`Unknown ${quote("extern")} type ${quote(type)}`, span)
            }
        }
        const end_span = tokens.expect("end").span
        return new UnitTypeExpression(Span.combine(span, end_span))
    }
    function parse_impl(signatures_only?: "signatures_only"): ImplDefinition {
        const span = tokens.consume().span
        const name_or_trait = tokens.expect_identifier().value
        let type_parameters = try_parse_generic_type_parameters()
        let trait: TraitDefinition | undefined
        let target_name = name_or_trait
        if (tokens.simple_peek() === "for") {
            tokens.consume()
            target_name = tokens.expect_identifier().value
            trait = env.expect_trait(name_or_trait, span)
            trait = new TraitDefinition({...trait}, trait.span)
            type_parameters = try_parse_generic_type_parameters()
        }
        const target = env.expect_type_definition(target_name, span)
        const target_type = env.expect_type(target_name, span)
        const types: Record<string, Type> = type_parameters.reduce(
            (acc, type) => {
                acc[type.name] = type
                return acc
            },
            {} as Record<string, Type>,
        )
        tokens.expect(":")
        const member_functions: FunctionDefinition[] = []
        const static_functions: FunctionDefinition[] = []
        run_in_sub_env(() => {
            env.types = types
            while (!tokens.at_end() && tokens.simple_peek() !== "end") {
                let fn: FunctionDefinition
                if (signatures_only) {
                    const signature = parse_function_signature(target_type)
                    fn = new FunctionDefinition(
                        {signature, block: new Block({body: []}, signature.span)},
                        signature.span,
                    )
                } else {
                    fn = parse_function_definition(target_type)
                }
                if (fn.signature.parameters[0]?.name !== "self") {
                    static_functions.push(fn)
                    const namespace = env.expect_namespace(target.name, target.span)
                    namespace.functions[fn.signature.name] = fn
                } else {
                    member_functions.push(fn)
                }
            }
        })
        const end_span = tokens.expect("end").span
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
    function parse_trait_definition(): TraitDefinition {
        const span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const type_parameters = try_parse_generic_type_parameters()
        tokens.expect(":")
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
        run_in_sub_env(() => {
            env.set_types(type_parameters)
            while (!tokens.at_end() && tokens.simple_peek() !== "end") {
                // We call this with a dummy type to enable the handling of `self` parameters.
                const signature = parse_function_signature(builtin_types.unit_type)
                if (![":", "=>"].includes(tokens.simple_peek() ?? "")) {
                    // Ok, this is a signature only.
                    if (signature.parameters[0].name !== "self") {
                        trait.static_function_signatures.push(signature)
                    } else {
                        trait.member_function_signatures.push(signature)
                    }
                } else {
                    const fn = parse_function_body(signature)
                    if (fn.signature.parameters[0].name !== "self") {
                        trait.static_function_default_impls[fn.signature.name] = fn
                    } else {
                        trait.member_function_default_impls[fn.signature.name] = fn
                    }
                }
            }
        })
        const end_span = tokens.expect("end").span
        trait.span = Span.combine(span, end_span)
        env.traits[name] = trait
        return trait
    }
    function parse_type_definition(): TypeDefinition {
        let span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const members: Record<string, Type> = {}
        const type_parameters = try_parse_generic_type_parameters()
        span = Span.combine(span, tokens.expect(":").span)
        run_in_sub_env(() => {
            env.set_types(type_parameters)
            while (!tokens.at_end() && tokens.simple_peek() !== "end") {
                const name = tokens.expect_identifier().value
                const type = parse_type()
                members[name] = type
            }
        })
        tokens.expect("end")
        const type_def = new TypeDefinition({name, members, type_parameters, impls: []}, span)
        env.types[name] = new Type({name, members, is_type_parameter: false, type_parameters}, span)
        env.type_definitions[name] = type_def
        env.namespaces[name] = new Namespace({name, functions: {}}, span)
        return type_def
    }
    function parse_block(if_else_mode?: "if_else_mode") {
        const block_type = tokens.consume()
        if (!(block_type instanceof LexicalToken) || !["=>", ":"].includes(block_type.value)) {
            throw new ParseError(
                `Expected start of a block (':' or '=>') but got ${quote(block_type.toString())}`,
                block_type.span,
            )
        }
        const block: Block = new Block({body: []}, block_type.span)
        if (block_type.value === "=>") {
            block.body.push(parse_expression())
        } else {
            const end_tokens = ["end"]
            if (if_else_mode) {
                end_tokens.push("else")
            }
            while (!tokens.at_end() && !end_tokens.includes(tokens.simple_peek() ?? "")) {
                block.body.push(parse_expression())
            }
            if (!if_else_mode) {
                tokens.expect("end")
            }
        }
        return block
    }
    function parse_loop(): Loop {
        const span = tokens.expect("loop").span
        const block = parse_block()
        return new Loop({block}, Span.combine(span, block.span))
    }
    function parse_if() {
        const span = tokens.expect("if").span
        const condition = parse_expression()
        const if_ = new If(
            {condition, then_block: parse_block("if_else_mode")},
            Span.combine(span, condition.span),
        )
        if (tokens.simple_peek() === "else") {
            tokens.consume()
            if_.else_block = parse_block()
        } else if (tokens.simple_peek() === "end") {
            tokens.consume()
        }
        return if_
    }
    function parse_variable_declaration() {
        const mutable = tokens.simple_peek() === "mut"
        const span = tokens.consume().span
        const name = tokens.expect_identifier().value
        const token = tokens.peek()
        let type = builtin_types.unit_type
        if (token instanceof Identifier) {
            type = env.expect_type(token.value, token.span)
            tokens.consume()
        }
        tokens.expect("=")
        const value = parse_expression()
        const let_ = new VariableDeclaration(
            {name, value, type, mutable},
            Span.combine(span, value.span),
        )
        env.variables[name] = new Variable({name, type, mutable}, Span.combine(span, value.span))
        return let_
    }
    function parse_function_signature(impl_type?: Type): FunctionSignature {
        const span = tokens.expect("fn").span
        const name = tokens.expect_identifier().value
        tokens.expect("(")
        const parameters: Parameter[] = []
        while (!tokens.at_end() && tokens.simple_peek() !== ")") {
            if (parameters.length > 0) {
                tokens.expect(",")
            }
            let mutable = false
            if (tokens.simple_peek() === "mut") {
                mutable = true
                tokens.consume()
            }
            const name_token = tokens.expect_identifier()
            if (impl_type) {
                if (name_token.value === "self") {
                    parameters.push(
                        new Parameter({name: "self", type: builtin_types.Self, mutable}, span),
                    )
                    continue
                }
            }
            const type = parse_type()
            parameters.push(new Parameter({name: name_token.value, type, mutable}, name_token.span))
        }
        tokens.expect(")")
        let return_type = builtin_types.unit_type
        if (tokens.peek() instanceof Identifier) {
            return_type = parse_type()
        }
        return new FunctionSignature({name, parameters, return_type}, span)
    }
    function parse_function_body(
        signature: FunctionSignature,
        impl_type?: Type,
    ): FunctionDefinition {
        const fn_def = new FunctionDefinition(
            {signature, block: new Block({body: []}, signature.span)},
            signature.span,
        )
        run_in_sub_env(() => {
            if (!impl_type) {
                env.outer!.functions[signature.name] = fn_def
                env.functions[signature.name] = fn_def
            }
            for (const parameter of fn_def.signature.parameters) {
                env.variables[parameter.name] = new Variable({...parameter}, parameter.span)
            }
            fn_def.block = parse_block()
            fn_def.span = Span.combine(signature.span, fn_def.block.span)
        })
        return fn_def
    }
    function parse_function_definition(impl_type?: Type): FunctionDefinition {
        const signature = parse_function_signature(impl_type)
        return parse_function_body(signature, impl_type)
    }
    function parse_return_expression(): Return {
        const span = tokens.expect("return").span
        const value = parse_expression()
        return new Return({value}, Span.combine(span, value.span))
    }
    function try_parse_namespace_access(token: Identifier): NamespaceAccess | undefined {
        const namespace = env.find_namespace(token.value)
        if (!namespace) {
            return
        }
        return new NamespaceAccess({namespace}, token.span)
    }
    function try_parse_struct_initialization(token: Identifier): StructInstantiation | undefined {
        let type_arguments: Type[] = []
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                if (type_arguments.length > 0) {
                    tokens.expect(",")
                }
                const type: Type = parse_type()
                type_arguments.push(type)
            }
            tokens.expect(">")
        }
        if (tokens.simple_peek() !== "{") {
            return
        }
        tokens.consume()
        let target = env.expect_type_definition(token.value, token.span)
        const args: Record<string, Expression> = {}
        while (!tokens.at_end() && tokens.simple_peek() !== "}") {
            if (Object.keys(args).length > 0) {
                tokens.expect(",")
            }
            const name = tokens.expect_identifier().value
            tokens.expect(":")
            const value = parse_expression()
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
        const span = Span.combine(tokens.expect("}").span, token.span)
        return new StructInstantiation({target, values: args, type_arguments}, span)
    }
    function try_parse_function_call(
        target_candidate: Expression | Token,
    ): FunctionCall | undefined {
        if (tokens.simple_peek() !== "(") {
            return
        }
        tokens.consume()
        let target: FunctionDefinition | FieldAccess | NamespaceAccess | undefined
        if (target_candidate instanceof Identifier) {
            target_candidate
            target = env.find_function(target_candidate.value)
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
        while (!tokens.at_end() && tokens.simple_peek() !== ")") {
            if (args.length > 0) {
                tokens.expect(",")
            }
            args.push(parse_expression())
        }
        const span = Span.combine(tokens.expect(")").span, target_candidate.span)
        return new FunctionCall({target, arguments: args}, span)
    }
    /**
     * Try to parse `<T, U>` as a type parameter list.
     * Return an empty array if there are no type parameters.
     */
    function try_parse_variable(token: Identifier): Variable | undefined {
        return env.find_variable(token.value)
    }
    function try_parse_generic_type_parameters(): Type[] {
        const type_parameters: Type[] = []
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                if (type_parameters.length > 0) {
                    tokens.expect(",")
                }
                const type_parameter = parse_type({expect_only_type_parameters: true})
                type_parameters.push(type_parameter)
            }
            tokens.expect(">")
        }
        return type_parameters
    }
    function parse_type(opts: {expect_only_type_parameters?: boolean} = {}): Type {
        const token = tokens.expect_identifier()
        let end_span = token.span
        let base_type = env.find_type(token.value)
        if (opts.expect_only_type_parameters) {
            base_type =
                env.find_type(token.value) ??
                new Type(
                    {name: token.value, is_type_parameter: true, type_parameters: []},
                    token.span,
                )
        } else {
            base_type = env.expect_type(token.value, token.span)
        }
        const type_parameters: Type[] = []
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                if (type_parameters.length > 0) {
                    tokens.expect(",")
                }
                const type_argument = parse_type()
                type_parameters.push(type_argument)
            }
            end_span = tokens.expect(">").span
        }
        if (opts.expect_only_type_parameters && !base_type.is_type_parameter) {
            throw new ParseError(
                `Expected type parameter but got the concrete type ${quote(base_type.name)}`,
                token.span,
            )
        }
        return new Type(
            {
                ...base_type,
                type_parameters,
            },
            Span.combine(token.span, end_span),
        )
    }
    return ast
}
