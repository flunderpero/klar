/**
 * Transpile Klar to JavaScript.
 */
// @ts-ignore
const dir = import.meta.dir
const prettify = Bun.argv.includes("--pretty")
const debug = Bun.argv.includes("--debug")
const debug_tokens = Bun.argv.includes("--debug-tokens") || debug
const debug_ast = Bun.argv.includes("--debug-ast") || debug
const debug_transpiled = Bun.argv.includes("--debug-transpiled") || debug

class Span {
    static combine(a: Span | HasKindAndSpan, b: Span | HasKindAndSpan) {
        if (a instanceof HasKindAndSpan) {
            a = a.span
        }
        if (b instanceof HasKindAndSpan) {
            b = b.span
        }
        assert(a.file === b.file, "Cannot combine spans from different files")
        return new Span(a.start, b.end, a.file, a.src)
    }

    constructor(
        public start: number,
        public end: number,
        public file: string,
        private src: string,
    ) {
        Object.defineProperty(this, "src", {enumerable: false})
    }

    toString() {
        let row = 1
        let col = 1
        for (let i = 0; i < this.start; i++) {
            if (this.src[i] === "\n") {
                row++
                col = 1
            } else {
                col++
            }
        }
        return `${this.file}:${row}:${col}`
    }
}

class HasKindAndSpan {
    kind = "node"
    constructor(public span: Span) {}

    to_json() {
        return to_json(this)
    }

    toString() {
        return `${this.kind} at ${this.span}`
    }
}

// Tokenizer

class Token extends HasKindAndSpan {
    kind = "token"
    constructor(span: Span) {
        super(span)
    }
}

class SimpleToken extends Token {
    kind = "simple"
    constructor(
        public value:
            | "("
            | ")"
            | "{"
            | "}"
            | "<"
            | ">"
            | "."
            | ","
            | ":"
            | "=>"
            | "="
            | "=="
            | "!="
            | ">"
            | "<"
            | ">="
            | "<="
            | "and"
            | "or"
            | "not"
            | "fn"
            | "end"
            | "return"
            | "+"
            | "-"
            | "*"
            | "/"
            | "let"
            | "mut"
            | "if"
            | "else"
            | "loop"
            | "break"
            | "continue"
            | "struct"
            | "trait"
            | "for"
            | "extern"
            | "impl"
            | "true"
            | "false",
        span: Span,
    ) {
        super(span)
    }
}

class NumberToken extends Token {
    kind = "number"
    constructor(
        public value: string,
        span: Span,
    ) {
        super(span)
    }
}

class Identifier extends Token {
    kind = "identifier"
    constructor(
        public value: string,
        span: Span,
    ) {
        super(span)
    }
}

class TokenStream {
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
            throw new Error(`Unexpected EOF`)
        }
        return this.tokens[this.index++]
    }

    expect_identifier() {
        const token = this.peek()
        if (!token) {
            throw new Error(`Expected identifier but got EOF`)
        }
        if (!(token instanceof Identifier)) {
            throw new Error(`Expected identifier at ${token.span}`)
        }
        return this.consume() as Identifier
    }

    expect(token_kind: typeof SimpleToken.prototype.value) {
        let actual = this.peek()
        if (!actual) {
            throw new Error(`Expected token of kind ${token_kind} but EOF`)
        }
        if (!(actual instanceof SimpleToken) || actual.value !== token_kind) {
            throw new Error(`Expected token of kind ${token_kind} but got ${actual.kind}`)
        }
        return this.consume()
    }

    simple_peek() {
        const token = this.peek()
        return token instanceof SimpleToken ? token.value : undefined
    }
}

// AST

class Expression extends HasKindAndSpan {
    kind = "expression"
    constructor(span: Span) {
        super(span)
    }
}

class FunctionDefinition extends Expression {
    kind = "function_definition"
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
}

class TypeDefinition extends Expression {
    kind = "type_definition"
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
}

class Return extends Expression {
    kind = "return"
    value: Expression

    constructor(data: {value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Return.prototype)
    }
}
class Number_ extends Expression {
    kind = "number"
    value: string

    constructor(data: {value: string}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Number_.prototype)
    }
}

class Bool extends Expression {
    kind = "bool"
    value: boolean

    constructor(data: {value: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Bool.prototype)
    }
}

class Not extends Expression {
    kind = "not"
    value: Expression

    constructor(data: {value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Not.prototype)
    }
}

class FunctionCall extends Expression {
    kind = "call"
    target: Expression
    arguments: Expression[]

    constructor(data: {target: Expression; arguments: Expression[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionCall.prototype)
    }
}

class StructInstantiation extends Expression {
    kind = "struct_instantiation"
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

class VariableDeclaration extends Expression {
    kind = "variable_declaration"
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

class Assignment extends Expression {
    kind = "assignment"
    target: Variable | FieldAccess
    value: Expression

    constructor(data: {target: Variable | FieldAccess; value: Expression}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Assignment.prototype)
    }
}

class FieldAccess extends Expression {
    kind = "field_access"
    object: Expression
    field: string

    constructor(data: {object: Expression; field: string}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FieldAccess.prototype)
    }
}

class NamespaceAccess extends Expression {
    kind = "namespace_access"
    namespace: Namespace

    constructor(data: {namespace: Namespace}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof NamespaceAccess.prototype)
    }
}

type BinaryOperator = "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "and" | "or"

class BinaryExpression extends Expression {
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

class If extends Expression {
    kind = "if"
    condition: Expression
    then_block: Block
    else_block?: Block

    constructor(data: {condition: Expression; then_block: Block; else_block?: Block}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof If.prototype)
    }
}

class Loop extends Expression {
    kind = "loop"
    block: Block

    constructor(data: {block: Block}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Loop.prototype)
    }
}

class Break extends Expression {
    kind = "break"

    constructor(span: Span) {
        super(span)
    }
}

class Continue extends Expression {
    kind = "continue"

    constructor(span: Span) {
        super(span)
    }
}

class UnitTypeExpression extends Expression {
    kind = "unit_type"

    constructor(span: Span) {
        super(span)
    }
}

class Parameter extends HasKindAndSpan {
    kind = "parameter"
    name: string
    type: Type
    mutable: boolean

    constructor(data: {name: string; type: Type; mutable: boolean}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Parameter.prototype)
    }
}

class Block extends HasKindAndSpan {
    kind = "block"
    body: Expression[]

    constructor(data: {body: Expression[]}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Block.prototype)
    }
}

class FunctionSignature extends HasKindAndSpan {
    kind = "function_signature"
    name: string
    parameters: Parameter[]
    return_type: Type

    constructor(data: {name: string; parameters: Parameter[]; return_type: Type}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof FunctionSignature.prototype)
    }
}

class ImplDefinition extends HasKindAndSpan {
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

class TraitDefinition extends HasKindAndSpan {
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
}

class Type extends HasKindAndSpan {
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
}

class Namespace extends HasKindAndSpan {
    kind = "namespace"
    name: string
    functions: Record<string, FunctionDefinition>

    constructor(data: {name: string; functions: Record<string, FunctionDefinition>}, span: Span) {
        super(span)
        Object.assign(this as typeof data, data as typeof Namespace.prototype)
    }
}

const builtin_types: Record<string, Type> = {
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
}

// Environment

class Variable extends HasKindAndSpan {
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

class Environment {
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
            throw new Error(`Unknown ${kind} ${name} at ${span}`)
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

type AST = Expression[]

/**
 * Lex Klar source code into tokens.
 */
function lexer({file, src}: {file: string; src: string}): Token[] {
    const tokens: Token[] = []
    let i = 0
    function span(relative_to?: Span): Span {
        if (relative_to) {
            return new Span(relative_to.start, i, file, src)
        }
        return new Span(i, i, file, src)
    }
    function peek(ahead = 0) {
        return src[i + ahead]
    }
    function consume() {
        return src[i++]
    }
    function skip(num = 1) {
        i += num
    }
    while (i < src.length) {
        const c = peek()
        if (c === "-" && peek(1) === "-" && peek(2) === "-") {
            skip(3)
            while (i < src.length - 1 && !(peek() === "-" && peek(1) === "-" && peek(2) === "-")) {
                skip()
            }
            skip(3)
        } else if (c === "-" && peek(1) === "-") {
            skip(2)
            while (i < src.length - 1 && peek() !== "\n") {
                skip()
            }
        } else if (c === "=" && peek(1) === "=") {
            tokens.push(new SimpleToken("==", span()))
            skip(2)
        } else if (c === "!" && peek(1) === "=") {
            tokens.push(new SimpleToken("!=", span()))
            skip(2)
        } else if (c === "<" && peek(1) === "=") {
            tokens.push(new SimpleToken("<=", span()))
            skip(2)
        } else if (c === ">" && peek(1) === "=") {
            tokens.push(new SimpleToken(">=", span()))
            skip(2)
        } else if (c === "=" && peek(1) === ">") {
            tokens.push(new SimpleToken("=>", span()))
            skip(2)
        } else if (
            [
                "(",
                ")",
                "{",
                "}",
                "<",
                ">",
                ":",
                ".",
                ",",
                "=",
                "+",
                "-",
                "*",
                "/",
                ">",
                "<",
            ].includes(c)
        ) {
            tokens.push(new SimpleToken(c as any, span()))
            skip()
        } else if (c.match(/\s/)) {
            skip()
        } else if (c.match(/[a-zA-Z_]/)) {
            let value = ""
            let start_span = span()
            while (i < src.length && peek().match(/[a-zA-Z0-9_]/)) {
                value += consume()
            }
            if (
                [
                    "return",
                    "fn",
                    "end",
                    "let",
                    "mut",
                    "if",
                    "else",
                    "loop",
                    "break",
                    "continue",
                    "struct",
                    "trait",
                    "for",
                    "extern",
                    "impl",
                    "true",
                    "false",
                    "and",
                    "or",
                    "not",
                ].includes(value)
            ) {
                tokens.push(new SimpleToken(value as any, span(start_span)))
            } else {
                tokens.push(new Identifier(value, span(start_span)))
            }
        } else if (c.match(/[0-9]/)) {
            let value = ""
            const start_span = span()
            while (peek().match(/[0-9]/)) {
                value += consume()
            }
            tokens.push(new NumberToken(value, span(start_span)))
        } else {
            throw new Error(`Unexpected character '${c}' at ${span()}`)
        }
    }
    return tokens
}

/**
 * Parse tokens into an AST.
 */
function parse(tokens: TokenStream, env: Environment): AST {
    function run_in_sub_env<T>(fn: () => T): T {
        const new_env = new Environment()
        new_env.outer = env
        env = new_env
        try {
            return fn.call(env)
        } finally {
            env = env.outer!
        }
    }

    const ast: AST = []
    while (!tokens.at_end()) {
        ast.push(parse_expression())
    }
    function parse_expression(): Expression {
        return parse_binary_expression(1)
    }
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
    function parse_primary(): Expression {
        const token = tokens.peek()
        if (!token) {
            throw new Error(`Unexpected EOF`)
        }
        let expression: Expression | undefined
        if (token instanceof SimpleToken) {
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
                throw new Error(`Unknown identifier ${token.value} at ${token.span}`)
            }
        }
        if (!expression) {
            throw new Error(`Unexpected token ${to_json(token)} at ${token.span}`)
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
        assert(
            target instanceof Variable || target instanceof FieldAccess,
            `Invalid target for field access: ${to_json(target)} at ${target.span}`,
        )
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
    function parse_type(
        treat_unknown_types_as_type_parameters?: "treat_unknown_types_as_type_parameters",
    ): Type {
        const token = tokens.expect_identifier()
        let end_span = token.span
        let base_type = env.find_type(token.value)
        if (treat_unknown_types_as_type_parameters) {
            base_type =
                env.find_type(token.value) ??
                new Type(
                    {name: token.value, is_type_parameter: true, type_parameters: []},
                    token.span,
                )
        } else {
            base_type = env.expect_type(token.value, token.span)
        }
        assert(!!base_type, `Unknown type ${token.value} at ${token.span}`)
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
        return new Type(
            {
                ...base_type,
                type_parameters,
            },
            Span.combine(token.span, end_span),
        )
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
                throw new Error(`Unknown extern type ${type} at ${span}`)
            }
        }
        const end_span = tokens.expect("end").span
        return new UnitTypeExpression(Span.combine(span, end_span))
    }
    function parse_impl(signatures_only?: "signatures_only"): ImplDefinition {
        const span = tokens.consume().span
        const name_or_trait = tokens.expect_identifier().value
        let type_parameters: Type[] = []
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                if (type_parameters.length > 0) {
                    tokens.expect(",")
                }
                const type_parameter = parse_type("treat_unknown_types_as_type_parameters")
                if (!type_parameter.is_type_parameter) {
                    throw new Error(
                        `Expected type parameter at ${type_parameter.span} but got the concrete type ${type_parameter.name}`,
                    )
                }
                type_parameters.push(type_parameter)
            }
            tokens.expect(">")
        }
        let trait: TraitDefinition | undefined
        let target_name = name_or_trait
        if (tokens.simple_peek() === "for") {
            tokens.consume()
            target_name = tokens.expect_identifier().value
            trait = env.expect_trait(name_or_trait, span)
            trait = new TraitDefinition({...trait, type_parameters: []}, trait.span)
            type_parameters = []
            if (tokens.simple_peek() === "<") {
                tokens.consume()
                while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                    if (type_parameters.length > 0) {
                        tokens.expect(",")
                    }
                    const type_parameter = parse_type("treat_unknown_types_as_type_parameters")
                    if (!type_parameter.is_type_parameter) {
                        throw new Error(
                            `Expected type parameter at ${type_parameter.span} but got the concrete type ${type_parameter.name}`,
                        )
                    }
                    type_parameters.push(type_parameter)
                }
                tokens.expect(">")
            }
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
        const type_parameters: Type[] = []
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                if (type_parameters.length > 0) {
                    tokens.expect(",")
                }
                const type_parameter = parse_type("treat_unknown_types_as_type_parameters")
                if (!type_parameter.is_type_parameter) {
                    throw new Error(
                        `Expected type parameter at ${type_parameter.span} but got the concrete type ${type_parameter.name}`,
                    )
                }
                type_parameters.push(type_parameter)
            }
            tokens.expect(">")
        }
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
        const type_parameters: Type[] = []
        if (tokens.simple_peek() === "<") {
            tokens.consume()
            while (!tokens.at_end() && tokens.simple_peek() !== ">") {
                if (type_parameters.length > 0) {
                    tokens.expect(",")
                }
                const type_parameter = parse_type("treat_unknown_types_as_type_parameters")
                if (!type_parameter.is_type_parameter) {
                    throw new Error(
                        `Expected type parameter at ${
                            type_parameter.span
                        } but got the concrete type ${type_parameter.name} at ${tokens.peek()
                            ?.span}`,
                    )
                }
                type_parameters.push(type_parameter)
            }
            tokens.expect(">")
        }
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
        if (!(block_type instanceof SimpleToken) || !["=>", ":"].includes(block_type.value)) {
            throw new Error(
                `Expected block start (':' or '=>') but got ${to_json(block_type)} at ${
                    block_type.span
                }`,
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
                    parameters.push(new Parameter({name: "self", type: impl_type, mutable}, span))
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
                if (!type) {
                    throw new Error(`Unknown type ${token.value} at ${token.span}`)
                }
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
            throw new Error(
                `Expected ${target.type_parameters.length} argument types but got ${type_arguments.length} at ${token.span}`,
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
                throw new Error(
                    `Invalid target for function call: ${to_json(target_candidate)} at ${
                        target_candidate.span
                    }`,
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
    function try_parse_variable(token: Identifier): Variable | undefined {
        return env.find_variable(token.value)
    }
    return ast
}

function check(ast: AST) {
    const visited: any[] = []
    for (const expression of ast) {
        apply_checks(expression)
    }
    function apply_checks(obj: any) {
        if (obj === undefined) {
            return
        }
        if (visited.includes(obj)) {
            return
        }
        visited.push(obj)
        if (obj.kind) {
            check_mutability(obj)
            check_all_trait_functions_are_implemented(obj)
        }
        for (const key of Object.keys(obj)) {
            const value = obj[key]
            if (Array.isArray(value)) {
                for (const child of value) {
                    apply_checks(child)
                }
            } else {
                apply_checks(value)
            }
        }
    }
    function check_mutability(expression: Expression) {
        if (expression instanceof Assignment) {
            const target = expression.target
            if (target instanceof Variable) {
                if (!target.mutable) {
                    throw new Error(
                        `Cannot assign to immutable variable ${target.name} at ${target.span}`,
                    )
                }
            }
            // fixme: check field access
        }
    }
    function check_all_trait_functions_are_implemented(expression: Expression) {
        if (!(expression instanceof TypeDefinition)) {
            return
        }
        for (const impl of expression.impls) {
            if (!impl.trait) {
                continue
            }
            const all_signatures = impl.trait.member_function_signatures.concat(
                impl.trait.static_function_signatures,
            )
            for (const signature of all_signatures) {
                const fn = impl.member_functions.find((x) => x.signature.name === signature.name)
                if (!fn) {
                    throw new Error(
                        `Missing implementation for function ${signature.name} of trait ${impl.trait.name} at ${signature.span}`,
                    )
                }
            }
        }
    }
    return ast
}

/**
 * Transpile AST to JavaScript.
 */
function transpile(ast: AST) {
    let res = ""
    for (const expression of ast) {
        res += transpile_expression(expression)
        res += "\n"
    }
    function transpile_expression(e: Expression): string {
        if (e instanceof FunctionDefinition) {
            const parameters = e.signature.parameters.map((x) => x.name).join(",")
            const block = transpile_expression(e.block)
            return `function ${e.signature.name}(${parameters})${block}`
        } else if (e instanceof Return) {
            return `return ${transpile_expression(e.value)};`
        } else if (e instanceof Bool || e instanceof Number_) {
            return `${e.value}`
        } else if (e instanceof Variable) {
            return e.name
        } else if (e instanceof FunctionCall) {
            const args = e.arguments.map(transpile_expression)
            if (e.target instanceof FunctionDefinition) {
                if (e.target.transpile) {
                    return e.target.transpile(e, args)
                }
                return `${e.target.signature.name}(${args.join(",")})\n`
            } else {
                const target = transpile_expression(e.target)
                return `${target}(${args.join(",")})\n`
            }
        } else if (e instanceof StructInstantiation) {
            const create_instance = `let _ = new ${e.target.name}()`
            const assign_members = Object.entries(e.values).map(
                ([name, value]) => `_.${name} = ${transpile_expression(value)};`,
            )
            return `(() => {${create_instance};${assign_members.join("\n")} return _})()`
        } else if (e instanceof VariableDeclaration) {
            const kind = e.mutable ? "let" : "const"
            const value = transpile_expression(e.value)
            return `${kind} ${e.name} = ${value};`
        } else if (e instanceof Assignment) {
            const target = transpile_expression(e.target)
            const value = transpile_expression(e.value)
            return `${target} = ${value};`
        } else if (e instanceof Block) {
            const body = e.body.map(transpile_expression).join("")
            return `{${body}}`
        } else if (e instanceof If) {
            const condition = transpile_expression(e.condition)
            const then = transpile_expression(e.then_block)
            let s = `if (${condition}) ${then}`
            if (e.else_block) {
                const else_ = transpile_expression(e.else_block)
                s += ` else ${else_}`
            }
            return s
        } else if (e instanceof TypeDefinition) {
            const members = Object.keys(e.members).join(";")
            const parameters = Object.keys(e.members)
                .map((x) => `${x}`)
                .join(",")
            const constructor_body = Object.keys(e.members)
                .map((x) => `this.${x} = ${x};`)
                .join("")

            let impls = ""
            function transpile_impl_function(fn: FunctionDefinition, is_static: boolean) {
                const parameters = fn.signature.parameters
                    .filter((x) => x.name !== "self")
                    .map((x) => x.name)
                    .join(",")
                const block = transpile_expression(fn.block).replace("{", "{const self = this;")
                impls += `${is_static ? "static " : ""}${fn.signature.name}(${parameters})${block}`
            }
            for (const impl of e.impls) {
                for (const fn of impl.member_functions) {
                    transpile_impl_function(fn, false)
                }
                for (const fn of impl.static_functions) {
                    transpile_impl_function(fn, true)
                }
            }
            const constructor = `constructor (${parameters}) {${constructor_body}}`
            return `class ${e.name} {${members}\n${constructor}\n${impls}}`
        } else if (e instanceof UnitTypeExpression) {
            return "undefined"
        } else if (e instanceof Loop) {
            const block = transpile_expression(e.block)
            return `while (true) ${block}`
        } else if (e instanceof Break) {
            return "break;"
        } else if (e instanceof Continue) {
            return "continue;"
        } else if (e instanceof BinaryExpression) {
            const lhs = transpile_expression(e.lhs)
            const rhs = transpile_expression(e.rhs)
            let operator: string = e.operator
            if (operator === "and") {
                operator = "&&"
            } else if (operator === "or") {
                operator = "||"
            } else if (operator === "==") {
                operator = "==="
            } else if (operator === "!=") {
                operator = "!=="
            }
            return `(${lhs} ${operator} ${rhs})`
        } else if (e instanceof FieldAccess) {
            const object = transpile_expression(e.object)
            return `${object}.${e.field}`
        } else if (e instanceof NamespaceAccess) {
            return e.namespace.name
        } else if (e instanceof Not) {
            const value = transpile_expression(e.value)
            return `!${value}`
        } else {
            throw new Error(`Unexpected expression ${to_json(e)} at ${e.span}`)
        }
    }
    return res
}

function assert(condition: boolean, msg: string): asserts condition {
    if (!condition) {
        throw new Error(msg)
    }
}

function to_json(obj: any, indent = 0) {
    function break_cycles() {
        const ancestors: any = []
        return function (_: any, value: any) {
            if (typeof value !== "object" || value === null) {
                return value
            }
            if (value instanceof Span) {
                return value.toString()
            }
            // `this` is the object that value is contained in,
            // i.e., its direct parent.
            // @ts-ignore
            while (ancestors.length > 0 && ancestors.at(-1) !== this) {
                ancestors.pop()
            }
            if (ancestors.includes(value)) {
                return "[Circular]"
            }
            ancestors.push(value)
            return value
        }
    }
    return JSON.stringify(obj, break_cycles(), indent)
}

async function compile({
    file,
    disable_debug,
    env,
}: {
    file: string
    env: Environment
    disable_debug?: boolean
}) {
    let src: string
    let log_prefix = `[${file.split("/").pop()}]`
    try {
        src = await Bun.file(file).text()
    } catch (error: any) {
        console.error(`${log_prefix} Error reading file: ${error.message}`)
        process.exit(1)
    }
    let transpiled
    try {
        const tokens = lexer({file: file.split("/").pop() ?? file, src})
        if (debug_tokens && !disable_debug) {
            console.log(`\n${log_prefix} TOKENS`)
            console.log(to_json(tokens, 2))
        }
        let ast = parse(new TokenStream(tokens), env)
        if (debug_ast && !disable_debug) {
            console.log(`\n${log_prefix} AST:`)
            console.log(to_json(ast, 2))
        }
        ast = check(ast)
        transpiled = transpile(ast)
    } catch (error: any) {
        if (debug) {
            throw error
        }
        console.error(`${log_prefix} Compile error: ${error.message}`)
        process.exit(1)
    }
    if (prettify) {
        const proc = Bun.spawn(["prettier", "--stdin-filepath", "transpiled.js"], {stdin: "pipe"})
        proc.stdin.write(transpiled)
        proc.stdin.end()
        transpiled = await new Response(proc.stdout).text()
    }
    if (debug_transpiled) {
        console.log(`\n${log_prefix} TRANSPILED:`)
        console.log(transpiled)
    }
    return transpiled
}

/**
 * Main entry point.
 */
async function cli() {
    const prelude = `#!/usr/bin/env bun --silent
process.exit(main())

`
    const env = new Environment()
    env.functions["print"] = new FunctionDefinition(
        {
            signature: new FunctionSignature(
                {
                    name: "print",
                    parameters: [
                        new Parameter(
                            {name: "value", mutable: false, type: builtin_types.i32},
                            new Span(0, 0, "<builtin>", ""),
                        ),
                    ],
                    return_type: builtin_types.unit_type,
                },
                new Span(0, 0, "<builtin>", ""),
            ),
            block: new Block({body: []}, new Span(0, 0, "<builtin>", "")),
            transpile: (_: FunctionCall, args: string[]) => `console.log(${args.join(",")});`,
        },
        new Span(0, 0, "<builtin>", ""),
    )
    env.types = {...builtin_types}
    const file = Bun.argv[2]
    const transpiled = await compile({file: file, env})
    const res = prelude + transpiled
    const dst = `${dir}/build/${(file.split("/")?.pop() ?? file).split(".")[0]}`
    await Bun.write(dst, res)
    const proc = Bun.spawn(["chmod", "+x", dst])
    await proc.exited
}

cli()
