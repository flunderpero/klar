/**
 * Transpile Klar to JavaScript.
 */
// @ts-ignore
const dir = import.meta.dir

class Span {
    static combine(a: Span, b: Span) {
        return new Span(a.start, b.end, a.src)
    }

    constructor(
        public start: number,
        public end: number,
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
        return `${row}:${col}`
    }
}

type Token = Identifier | Number_ | SimpleToken

type SimpleToken = {
    kind: "simple"
    value:
        | "("
        | ")"
        | "{"
        | "}"
        | "<"
        | ">"
        | "."
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
        | "false"
    span: Span
}

type Identifier = {
    kind: "identifier"
    value: string
    span: Span
}

type Parameter = {
    kind: "parameter"
    name: string
    type: Type
    span: Span
    mutable: boolean
}

type Block = {
    kind: "block"
    body: Expression[]
    span: Span
}

type FunctionSignature = {
    kind: "fn_signature"
    name: string
    parameters: Parameter[]
    return_type: Type
    span: Span
}

type FunctionDefinition = {
    kind: "fn"
    signature: FunctionSignature
    block: Block
    transpile?: (fn: FunctionCall, args: string[]) => string
    extern?: boolean
    span: Span
}

type Impl = {
    kind: "impl"
    member_functions: FunctionDefinition[]
    static_functions: FunctionDefinition[]
    trait?: Trait
    extern?: boolean
    span: Span
}

type Trait = {
    kind: "trait"
    name: string
    generic_types: Record<string, Type>
    member_function_signatures: FunctionSignature[]
    static_function_signatures: FunctionSignature[]
    member_function_default_impls: Record<string, FunctionDefinition>
    static_function_default_impls: Record<string, FunctionDefinition>
    extern?: boolean
    span: Span
}

type VariableDeclaration = {
    kind: "var"
    name: string
    value: Expression
    type: Type
    span: Span
    mutable: boolean
}

type Assignment = {
    kind: "assignment"
    target: Variable | FieldAccess
    value: Expression
    span: Span
}

type FunctionCall = {
    kind: "call"
    target: FunctionDefinition | FieldAccess | NamespaceAccess
    arguments: Expression[]
    span: Span
}

type StructConstructCall = {
    kind: "struct_construct"
    generic_types: Type[]
    target: TypeDefinition
    values: Record<string, Expression>
    span: Span
}

type ReturnExpression = {
    kind: "return"
    value: Expression
    span: Span
}

type Number_ = {
    kind: "number"
    value: string
    span: Span
    result_type: Type
}

type Bool = {
    kind: "bool"
    value: boolean
    span: Span
    result_type: Type
}

type Not = {
    kind: "not"
    value: Expression
    span: Span
}

type Type = {
    kind: "type"
    name: string
    transpile?: (value: any) => string
    span: Span
    members?: Record<string, Type>
    is_generic: boolean
}

type TypeDefinition = {
    kind: "type_definition"
    name: string
    span: Span
    members: Record<string, Type>
    generic_type_vars: string[]
    impls: Impl[]
    extern?: boolean
}

type Namespace = {
    type: "namespace"
    name: string
    functions: Record<string, FunctionDefinition>
}

type FieldAccess = {
    kind: "field_access"
    object: Expression
    field: string
    span: Span
}

type NamespaceAccess = {
    kind: "namespace_access"
    namespace: Namespace
    span: Span
}

type BinaryExpression = {
    kind: "binary"
    operator: "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "and" | "or"
    left: Expression
    right: Expression
    span: Span
}

type If = {
    kind: "if"
    condition: Expression
    then_block: Block
    else_block?: Block
    span: Span
}

type Loop = {
    kind: "loop"
    block: Block
    span: Span
}

type Break = {
    kind: "break"
    span: Span
}

type Continue = {
    kind: "continue"
    span: Span
}

type UnitTypeExpression = {
    kind: "unit_type"
    span: Span
}

const builtin_types = {
    i32: {
        kind: "type",
        name: "i32",
        transpile: (value: any) => value,
        span: new Span(0, 0, ""),
        is_generic: false,
    } as Type,
    bool: {
        kind: "type",
        name: "bool",
        transpile: (value: any) => value,
        span: new Span(0, 0, ""),
        is_generic: false,
    } as Type,
    unit_type: {
        kind: "type",
        name: "unit_type",
        transpile: () => "undefined",
        span: new Span(0, 0, ""),
        is_generic: false,
    } as Type,
}

type Variable = {
    kind: "variable"
    name: string
    type: Type
    span: Span
    mutable: boolean
}

type Expression =
    | FunctionDefinition
    | ReturnExpression
    | Number_
    | Bool
    | Not
    | FunctionCall
    | StructConstructCall
    | Variable
    | VariableDeclaration
    | BinaryExpression
    | If
    | Block
    | Assignment
    | Loop
    | Break
    | Continue
    | TypeDefinition
    | FieldAccess
    | NamespaceAccess
    | UnitTypeExpression

type AST = Expression[]

type Environment = {
    functions: Record<string, FunctionDefinition>
    variables: Record<string, Variable>
    types: Record<string, Type>
    type_definitions: Record<string, TypeDefinition>
    traits: Record<string, Trait>
    namespaces: Record<string, Namespace>
    outer?: Environment
}

/**
 * Lex Klar source code into tokens.
 */
function lexer(src: string) {
    const tokens: Token[] = []
    let i = 0
    function span(relative_to?: Span): Span {
        if (relative_to) {
            return new Span(relative_to.start, i, src)
        }
        return new Span(i, i, src)
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
            tokens.push({kind: "simple", value: "==", span: span()})
            skip(2)
        } else if (c === "!" && peek(1) === "=") {
            tokens.push({kind: "simple", value: "!=", span: span()})
            skip(2)
        } else if (c === "<" && peek(1) === "=") {
            tokens.push({kind: "simple", value: "<=", span: span()})
            skip(2)
        } else if (c === ">" && peek(1) === "=") {
            tokens.push({kind: "simple", value: ">=", span: span()})
            skip(2)
        } else if (c === "=" && peek(1) === ">") {
            tokens.push({kind: "simple", value: "=>", span: span()})
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
            tokens.push({kind: "simple", value: c as any, span: span()})
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
                tokens.push({kind: "simple", value: value as any, span: span(start_span)})
            } else {
                tokens.push({kind: "identifier", value, span: span(start_span)})
            }
        } else if (c.match(/[0-9]/)) {
            let value = ""
            const start_span = span()
            while (peek().match(/[0-9]/)) {
                value += consume()
            }
            tokens.push({
                kind: "number",
                value,
                span: span(start_span),
                result_type: builtin_types.i32,
            })
        } else {
            throw new Error(`Unexpected character '${c}' at ${span()}`)
        }
    }
    return tokens
}

/**
 * Parse tokens into an AST.
 */
function parse(tokens: Token[]): AST {
    const binary_op_precedence = {
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
        not: 7,
    }
    let env: Environment = {
        functions: {
            // Built-in functions:
            print: {
                kind: "fn",
                signature: {
                    kind: "fn_signature",
                    name: "print",
                    parameters: [
                        {
                            kind: "parameter",
                            name: "value",
                            mutable: false,
                            type: builtin_types.i32,
                            span: new Span(0, 0, ""),
                        },
                    ],
                    return_type: builtin_types.unit_type,
                    span: new Span(0, 0, ""),
                },
                block: {} as any,
                transpile: (_: FunctionCall, args: string[]) => `console.log(${args.join(",")});`,
                span: new Span(0, 0, ""),
            },
        },
        types: {...builtin_types},
        type_definitions: {},
        traits: {},
        variables: {},
        namespaces: {},
    }
    const ast: AST = []
    let i = 0
    function peek(ahead = 0) {
        return tokens[i + ahead] as Token & {kind?: any; value?: any}
    }
    function simple_peek(ahead = 0) {
        const token = peek(ahead)
        return token?.kind === "simple" ? token.value : undefined
    }
    function consume() {
        const value = peek()
        i++
        return value as Token & {kind?: any; value?: any}
    }
    function expect(
        token_kind:
            | "identifier"
            | "("
            | ")"
            | "{"
            | "}"
            | "<"
            | ">"
            | ":"
            | ","
            | "="
            | "end"
            | "return"
            | "fn"
            | "let"
            | "mut"
            | "if"
            | "loop"
            | "else",
    ) {
        let actual = peek() as any
        let actual_kind = actual.kind
        if (actual.kind === "simple") {
            actual_kind = actual.value
        }
        if (actual_kind !== token_kind) {
            throw new Error(
                `Expected token of kind ${token_kind} but got ${to_json(actual)} at ${actual.span}`,
            )
        }
        return consume() as Token & {kind?: any; value?: any}
    }
    while (i < tokens.length) {
        ast.push(parse_expression())
    }
    function parse_expression(): Expression {
        return parse_binary_expression(1)
    }
    function parse_binary_expression(min_precedence: number) {
        let lhs = parse_primary()
        while (true) {
            const op = simple_peek()
            if (!binary_op_precedence[op]) {
                if (op === "=") {
                    return parse_assignment(lhs)
                }
                return lhs
            }
            const op_precedence = binary_op_precedence[op]
            if (op_precedence >= min_precedence) {
                consume() // consume the operator
                let rhs = parse_binary_expression(op_precedence + 1)
                lhs = {
                    kind: "binary",
                    operator: op,
                    left: lhs,
                    right: rhs,
                    span: Span.combine(lhs.span, rhs.span),
                }
            } else {
                return lhs
            }
        }
    }
    function parse_primary(): Expression {
        const token = peek()
        const simple_token = token.kind === "simple" ? token.value : undefined
        let expression: Expression
        if (simple_token === "fn") {
            expression = parse_function_definition()
        } else if (simple_token === "let" || simple_token === "mut") {
            expression = parse_variable_declaration()
        } else if (simple_token === "true" || simple_token === "false") {
            expression = {
                kind: "bool",
                value: simple_token === "true",
                span: consume().span,
                result_type: builtin_types.bool,
            }
        } else if (simple_token === "not") {
            const span = consume().span
            const value = parse_expression()
            expression = {kind: "not", value, span: Span.combine(span, value.span)}
        } else if (simple_token === "if") {
            expression = parse_if()
        } else if (simple_token === "loop") {
            expression = parse_loop()
        } else if (simple_token === "(") {
            consume()
            expression = parse_expression()
            expect(")")
        } else if (simple_token === "break") {
            expression = {kind: "break", span: consume().span}
        } else if (simple_token === "continue") {
            expression = {kind: "continue", span: consume().span}
        } else if (simple_token === "extern") {
            expression = parse_extern_block()
        } else if (simple_token === "struct") {
            expression = parse_type_definition()
        } else if (simple_token === "trait") {
            const trait = parse_trait_definition()
            expression = {kind: "unit_type", span: trait.span}
        } else if (simple_token === "return") {
            expression = parse_return_expression()
        } else if (token.kind === "number") {
            expression = consume() as Number_
        } else if (token.kind === "identifier") {
            const token = expect("identifier")
            expression =
                parse_function_call(token) ||
                parse_variable(token) ||
                parse_struct_constructor(token) ||
                parse_namespace_access(token)
            if (!expression) {
                throw new Error(`Unknown identifier ${token.value} at ${token.span}`)
            }
        } else if (simple_token === "impl") {
            const impl = parse_impl()
            expression = {kind: "unit_type", span: impl.span}
        }
        if (!expression) {
            throw new Error(`Unexpected token ${to_json(token)} at ${token.span} ${simple_token}`)
        }
        while (true) {
            if (simple_peek() === "(") {
                expression = parse_function_call(expression) || expression
            } else if (simple_peek() === ".") {
                expression = parse_field_access(expression)
            } else {
                break
            }
        }
        return expression
    }
    function parse_assignment(target: Expression): Assignment {
        assert(
            target.kind === "variable" || target.kind === "field_access",
            `Invalid target for field access: ${to_json(target)} at ${target.span}`,
        )
        const span = consume().span
        const value = parse_expression()
        return {
            kind: "assignment",
            target: target as Variable | FieldAccess,
            value,
            span: Span.combine(span, value.span),
        }
    }
    function parse_field_access(expression: Expression): Expression {
        while (simple_peek() === ".") {
            const span = consume().span
            const field = expect("identifier")
            expression = {
                kind: "field_access",
                object: expression,
                field: field.value,
                span: Span.combine(span, field.span),
            }
        }
        return expression
    }
    function parse_extern_block(): UnitTypeExpression {
        const span = consume().span
        expect(":")
        while (i < tokens.length && simple_peek() !== "end") {
            const type = simple_peek()
            if (type === "fn") {
                const signature = parse_function_signature()
                env.functions[signature.name] = {
                    kind: "fn",
                    signature,
                    block: {} as any,
                    extern: true,
                    span: signature.span,
                }
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
        const end_span = expect("end").span
        return {kind: "unit_type", span: Span.combine(span, end_span)}
    }
    function parse_impl(signatures_only?: "signatures_only"): Impl {
        const span = consume().span
        const name_or_trait = expect("identifier").value
        let trait: Trait | undefined
        let target_name = name_or_trait
        if (simple_peek() === "for") {
            consume()
            target_name = expect("identifier").value
            trait = find_in_env("traits", name_or_trait)
            if (!trait) {
                throw new Error(`Unknown trait ${name_or_trait} at ${span}`)
            }
        }
        const target: TypeDefinition = find_in_env("type_definitions", target_name)
        if (!target) {
            throw new Error(`Unknown type ${target} at ${span}`)
        }
        const target_type: Type = find_in_env("types", target.name)
        const types = {}
        if (simple_peek() === "<") {
            consume()
            const name = expect("identifier").value
            types[name] = {kind: "type", name, is_generic: true, span: span}
            expect(">")
        }
        expect(":")
        env = {
            functions: {},
            types,
            variables: {},
            type_definitions: {},
            traits: {},
            namespaces: {},
            outer: env,
        }
        const member_functions: FunctionDefinition[] = []
        const static_functions: FunctionDefinition[] = []
        while (i < tokens.length && simple_peek() !== "end") {
            let fn: FunctionDefinition
            if (signatures_only) {
                const signature = parse_function_signature(target_type)
                fn = {kind: "fn", signature, block: {} as any, span: signature.span}
            } else {
                fn = parse_function_definition(target_type)
            }
            if (fn.signature.parameters[0]?.name !== "self") {
                static_functions.push(fn)
                const namespace: Namespace = find_in_env("namespaces", target.name)
                if (!namespace) {
                    throw new Error(`Unknown namespace ${target.name} at ${target.span}`)
                }
                namespace.functions[fn.signature.name] = fn
            } else {
                member_functions.push(fn)
            }
        }
        const end_span = expect("end").span
        env = env.outer
        const impl: Impl = {
            kind: "impl",
            member_functions,
            static_functions,
            trait,
            span: Span.combine(span, end_span),
        }
        target.impls.push(impl)
        return impl
    }
    function parse_trait_definition(): Trait {
        const span = consume().span
        const name = expect("identifier").value
        const generic_types = {}
        if (simple_peek() === "<") {
            consume()
            const name = expect("identifier").value
            generic_types[name] = {kind: "type", name, is_generic: true, span: span}
            expect(">")
        }
        expect(":")
        env = {
            functions: {},
            types: generic_types,
            variables: {},
            type_definitions: {},
            traits: {},
            namespaces: {},
            outer: env,
        }
        const trait: Trait = {
            kind: "trait",
            name,
            generic_types,
            member_function_signatures: [],
            static_function_signatures: [],
            member_function_default_impls: {},
            static_function_default_impls: {},
            span,
        }
        while (i < tokens.length && simple_peek() !== "end") {
            // We call this with a dummy type to enable the handling of `self` parameters.
            const signature = parse_function_signature(builtin_types.unit_type)
            if (![":", "=>"].includes(simple_peek())) {
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
        const end_span = expect("end").span
        trait.span = Span.combine(span, end_span)
        env = env.outer
        env.traits[name] = trait
        return trait
    }
    function parse_type_definition(): TypeDefinition {
        const span = consume().span
        const name = expect("identifier").value
        const members: Record<string, Type> = {}
        const generic_type_vars = []
        if (simple_peek() === "<") {
            consume()
            const name = expect("identifier").value
            generic_type_vars.push(name)
            expect(">")
        }
        expect(":")
        while (i < tokens.length && simple_peek() !== "end") {
            const name = expect("identifier").value
            const type_name = expect("identifier").value
            let type: Type
            if (generic_type_vars.includes(type_name)) {
                type = {kind: "type", name: type_name, is_generic: true, span: span}
            } else {
                type = find_in_env("types", type_name)
                if (!type) {
                    throw new Error(`Unknown type ${name} at ${name.span}`)
                }
            }
            members[name] = type
        }
        expect("end")
        const type_def: TypeDefinition = {
            kind: "type_definition",
            name,
            impls: [],
            generic_type_vars,
            span,
            members,
        }
        env.types[name] = {kind: "type", name, span, members, is_generic: false}
        env.type_definitions[name] = type_def
        env.namespaces[name] = {type: "namespace", name, functions: {}}
        return type_def
    }
    function parse_block(if_else_mode?: "if_else_mode") {
        const block_type = consume()
        if (block_type.kind !== "simple" || !["=>", ":"].includes(block_type.value)) {
            throw new Error(
                `Expected block start (':' or '=>') but got ${to_json(block_type)} at ${
                    block_type.span
                }`,
            )
        }
        const block: Block = {
            kind: "block",
            body: [],
            span: block_type.span,
        }
        if (block_type.value === "=>") {
            block.body.push(parse_expression())
        } else {
            const end_tokens = ["end"]
            if (if_else_mode) {
                end_tokens.push("else")
            }
            while (i < tokens.length && !end_tokens.includes(simple_peek())) {
                block.body.push(parse_expression())
            }
            if (!if_else_mode) {
                expect("end")
            }
        }
        return block
    }
    function parse_loop(): Loop {
        const span = expect("loop").span
        const block = parse_block()
        return {kind: "loop", block, span: Span.combine(span, block.span)}
    }
    function parse_if() {
        const span = expect("if").span
        const condition = parse_expression()
        const if_: If = {
            kind: "if",
            condition,
            then_block: parse_block("if_else_mode"),
            span: Span.combine(span, condition.span),
        }
        if (simple_peek() === "else") {
            consume()
            if_.else_block = parse_block()
        } else if (simple_peek() === "end") {
            consume()
        }
        return if_
    }
    function parse_variable_declaration() {
        const mutable = simple_peek() === "mut"
        const span = consume().span
        const name = expect("identifier").value
        const token = peek()
        let type = builtin_types.unit_type
        if (token.kind === "identifier") {
            type = find_in_env("types", token.value)
            if (!type) {
                throw new Error(`Unknown type ${token.value} at ${token.span}`)
            }
            consume()
        }
        expect("=")
        const value = parse_expression()
        const let_: VariableDeclaration = {
            kind: "var",
            name,
            value,
            type,
            span: Span.combine(span, value.span),
            mutable,
        }
        env.variables[name] = {
            kind: "variable",
            name,
            type,
            mutable,
            span: Span.combine(span, value.span),
        }
        return let_
    }
    function parse_function_signature(impl_type?: Type): FunctionSignature {
        const span = expect("fn").span
        const name = expect("identifier").value
        expect("(")
        const parameters = []
        while (i < tokens.length && simple_peek() !== ")") {
            if (parameters.length > 0) {
                expect(",")
            }
            let mutable = false
            if (simple_peek() === "mut") {
                mutable = true
                consume()
            }
            const name_token = expect("identifier")
            if (impl_type) {
                if (name_token.value === "self") {
                    parameters.push({
                        kind: "parameter",
                        name: name_token.value,
                        type: impl_type,
                        span: name_token.span,
                        mutable,
                    })
                    continue
                }
            }
            const type_token = expect("identifier")
            parameters.push({
                kind: "parameter",
                name: name_token.value,
                type: find_in_env("types", type_token.value),
                span: Span.combine(name_token.span, type_token.span),
                mutable,
            })
        }
        expect(")")
        let return_type = builtin_types.unit_type
        if (peek().kind === "identifier") {
            const return_type_name = expect("identifier").value
            return_type = find_in_env("types", return_type_name)
            if (!return_type) {
                throw new Error(`Unknown type ${return_type_name} at ${span}`)
            }
        }
        return {kind: "fn_signature", name, parameters, return_type, span}
    }
    function parse_function_body(
        signature: FunctionSignature,
        impl_type?: Type,
    ): FunctionDefinition {
        const fn_def: FunctionDefinition = {
            kind: "fn",
            signature,
            block: {} as any,
            span: signature.span,
        }
        env = {
            functions: {},
            types: {},
            variables: {},
            type_definitions: {},
            traits: {},
            namespaces: {},
            outer: env,
        }
        if (!impl_type) {
            env.outer.functions[signature.name] = fn_def
            env.functions[signature.name] = fn_def
        }
        for (const parameter of fn_def.signature.parameters) {
            env.variables[parameter.name] = {
                ...parameter,
                kind: "variable",
            }
        }
        fn_def.block = parse_block()
        fn_def.span = Span.combine(signature.span, fn_def.block.span)
        env = env.outer
        return fn_def
    }
    function parse_function_definition(impl_type?: Type): FunctionDefinition {
        const signature = parse_function_signature(impl_type)
        return parse_function_body(signature, impl_type)
    }
    function parse_return_expression(): ReturnExpression {
        const span = expect("return").span
        const value = parse_expression()
        return {kind: "return", value, span: Span.combine(span, value.span)}
    }
    function parse_namespace_access(token: Identifier): Expression | undefined {
        const namespace: Namespace = find_in_env("namespaces", token.value)
        if (!namespace) {
            return
        }
        return {kind: "namespace_access", namespace, span: token.span}
    }
    function parse_struct_constructor(token: Identifier): StructConstructCall | undefined {
        let generic_types: Type[] = []
        if (simple_peek() === "<") {
            consume()
            while (i < tokens.length && simple_peek() !== ">") {
                if (generic_types.length > 0) {
                    expect(",")
                }
                const type: Type = find_in_env("types", expect("identifier").value)
                if (!type) {
                    throw new Error(`Unknown type ${token.value} at ${token.span}`)
                }
                generic_types.push(type)
            }
            expect(">")
        }
        if (simple_peek() !== "{") {
            return
        }
        consume()
        let target: TypeDefinition = find_in_env("type_definitions", token.value)
        if (!target) {
            throw new Error(`Unknown type ${token.value} at ${token.span}`)
        }
        const args = {}
        while (i < tokens.length && simple_peek() !== "}") {
            if (Object.keys(args).length > 0) {
                expect(",")
            }
            const name = expect("identifier").value
            expect(":")
            const value = parse_expression()
            args[name] = value
        }
        if (generic_types.length !== target.generic_type_vars.length) {
            throw new Error(
                `Expected ${target.generic_type_vars.length} generic types but got ${generic_types.length} at ${token.span}`,
            )
        }
        if (generic_types.length > 0) {
            // Create a new type with the generic types filled in.
            target = {...target}
            for (let i = 0; i < generic_types.length; i++) {
                const type = generic_types[i]
                if (type.is_generic) {
                    target.members[target.generic_type_vars[i]] = type
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
        const span = Span.combine(expect("}").span, token.span)
        return {kind: "struct_construct", target, values: args, generic_types, span}
    }
    function parse_function_call(target_candidate: Expression | Token): FunctionCall | undefined {
        if (simple_peek() !== "(") {
            return
        }
        consume()
        let target: FunctionDefinition | FieldAccess | NamespaceAccess | undefined
        if (target_candidate.kind === "identifier") {
            target_candidate
            target = find_in_env("functions", target_candidate.value)
        } else {
            if (
                target_candidate.kind !== "field_access" &&
                target_candidate.kind !== "namespace_access"
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
        while (i < tokens.length && simple_peek() !== ")") {
            if (args.length > 0) {
                expect(",")
            }
            args.push(parse_expression())
        }
        const span = Span.combine(expect(")").span, target_candidate.span)
        return {kind: "call", target, arguments: args, span}
    }
    function parse_variable(token: Identifier): Variable | undefined {
        const name = token.value
        const variable: Variable | undefined = find_in_env("variables", name)
        if (variable) {
            return {...variable, span: token.span}
        }
    }
    function find_in_env<T>(
        kind: "functions" | "variables" | "types" | "type_definitions" | "namespaces" | "traits",
        name: string,
    ): T | undefined {
        let value
        let search_env = env
        while (!value && search_env) {
            value = search_env[kind][name]
            search_env = search_env.outer
        }
        return value
    }
    return ast
}

function check(ast: AST) {
    const visited = []
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
        if (expression.kind === "assignment") {
            const target = expression.target
            if (target.kind === "variable") {
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
        if (expression.kind !== "type_definition") {
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
    const generic_impls = {}
    for (const expression of ast) {
        res += transpile_expression(expression)
        res += "\n"
    }
    function transpile_expression(expression: Expression) {
        if (expression.kind === "fn") {
            const parameters = expression.signature.parameters.map((x) => x.name).join(",")
            const block = transpile_expression(expression.block)
            return `function ${expression.signature.name}(${parameters})${block}`
        } else if (expression.kind === "return") {
            return `return ${transpile_expression(expression.value)};`
        } else if (expression.kind === "number" || expression.kind === "bool") {
            return expression.value
        } else if (expression.kind === "variable") {
            return expression.name
        } else if (expression.kind === "call") {
            const args = expression.arguments.map(transpile_expression)
            if (expression.target.kind === "fn") {
                if (expression.target.transpile) {
                    return expression.target.transpile(expression, args)
                }
                return `${expression.target.signature.name}(${args.join(",")})\n`
            } else {
                const target = transpile_expression(expression.target)
                return `${target}(${args.join(",")})\n`
            }
        } else if (expression.kind === "struct_construct") {
            const create_instance = `let _ = new ${expression.target.name}()`
            const assign_members = Object.entries(expression.values).map(
                ([name, value]) => `_.${name} = ${transpile_expression(value)};`,
            )
            return `(() => {${create_instance};${assign_members.join("\n")} return _})()`
        } else if (expression.kind === "var") {
            const kind = expression.mutable ? "let" : "const"
            const value = transpile_expression(expression.value)
            return `${kind} ${expression.name} = ${value};`
        } else if (expression.kind === "assignment") {
            const target = transpile_expression(expression.target)
            const value = transpile_expression(expression.value)
            return `${target} = ${value};`
        } else if (expression.kind === "block") {
            const body = expression.body.map(transpile_expression).join("")
            return `{${body}}`
        } else if (expression.kind === "if") {
            const condition = transpile_expression(expression.condition)
            const then = transpile_expression(expression.then_block)
            let s = `if (${condition}) ${then}`
            if (expression.else_block) {
                const else_ = transpile_expression(expression.else_block)
                s += ` else ${else_}`
            }
            return s
        } else if (expression.kind === "type_definition") {
            const members = Object.keys(expression.members).join(";")
            const parameters = Object.keys(expression.members)
                .map((x) => `${x}`)
                .join(",")
            const constructor_body = Object.keys(expression.members)
                .map((x) => `this.${x} = ${x};`)
                .join("")

            let impls = ""
            function transpile_impl_function(fn: FunctionDefinition, is_static) {
                const parameters = fn.signature.parameters
                    .filter((x) => x.name !== "self")
                    .map((x) => x.name)
                    .join(",")
                const block = transpile_expression(fn.block).replace("{", "{const self = this;")
                impls += `${is_static ? "static " : ""}${fn.signature.name}(${parameters})${block}`
            }
            for (const impl of expression.impls) {
                for (const fn of impl.member_functions) {
                    transpile_impl_function(fn, false)
                }
                for (const fn of impl.static_functions) {
                    transpile_impl_function(fn, true)
                }
            }
            const constructor = `constructor (${parameters}) {${constructor_body}}`
            return `class ${expression.name} {${members}\n${constructor}\n${impls}}`
        } else if (expression.kind === "unit_type") {
            return "undefined"
        } else if (expression.kind === "loop") {
            const block = transpile_expression(expression.block)
            return `while (true) ${block}`
        } else if (expression.kind === "break") {
            return "break;"
        } else if (expression.kind === "continue") {
            return "continue;"
        } else if (expression.kind === "binary") {
            const lhs = transpile_expression(expression.left)
            const rhs = transpile_expression(expression.right)
            let operator = expression.operator
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
        } else if (expression.kind === "field_access") {
            const object = transpile_expression(expression.object)
            return `${object}.${expression.field}`
        } else if (expression.kind === "namespace_access") {
            return expression.namespace.name
        } else if (expression.kind === "not") {
            const value = transpile_expression(expression.value)
            return `!${value}`
        } else {
            assert_never(expression)
        }
    }
    return res
}

function assert(condition: boolean, msg: string) {
    if (!condition) {
        throw new Error(msg)
    }
}

function assert_never(x: never): any {
    throw new Error(`Unexpected object ${to_json(x)} at ${(x as any).span}`)
}

function to_json(obj: any, indent = 0) {
    function break_cycles() {
        const ancestors = []
        return function (_, value) {
            if (typeof value !== "object" || value === null) {
                return value
            }
            // `this` is the object that value is contained in,
            // i.e., its direct parent.
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

/**
 * Main entry point.
 */
async function cli() {
    const prelude = `#!/usr/bin/env bun --silent
process.exit(main())

`
    const src_file = Bun.argv[2]
    const prettify = Bun.argv.includes("--pretty")
    const debug = Bun.argv.includes("--debug")
    const debug_tokens = Bun.argv.includes("--debug-tokens") || debug
    const debug_ast = Bun.argv.includes("--debug-ast") || debug
    const debug_transpiled = Bun.argv.includes("--debug-transpiled") || debug
    const src = await Bun.file(src_file).text()
    let transpiled
    try {
        const tokens = lexer(src)
        if (debug_tokens) {
            console.log("\nTOKENS:")
            console.log(to_json(tokens, 2))
        }
        let ast = parse(tokens)
        if (debug_ast) {
            console.log("\nAST:")
            console.log(to_json(ast, 2))
        }
        ast = check(ast)
        transpiled = transpile(ast)
    } catch (error) {
        if (debug) {
            throw error
        }
        console.error(`Compile error: ${error.message}`)
        process.exit(1)
    }
    if (prettify) {
        const proc = Bun.spawn(["prettier", "--stdin-filepath", "transpiled.js"], {stdin: "pipe"})
        proc.stdin.write(transpiled)
        proc.stdin.end()
        transpiled = await new Response(proc.stdout).text()
    }
    if (debug_transpiled) {
        console.log("\nTRANSPILED:")
        console.log(transpiled)
    }
    const res = prelude + transpiled
    const dst = `${dir}/build/${src_file.split("/").pop().split(".")[0]}`
    await Bun.write(dst, res)
    Bun.spawnSync(["chmod", "+x", dst])
}

cli()
