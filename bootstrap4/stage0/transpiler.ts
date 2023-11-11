/**
 * Transpile Klar to JavaScript.
 */
declare const Bun: any
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
        | ":"
        | "=>"
        | "="
        | "=="
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
}

type Block = {
    kind: "block"
    body: Expression[]
    span: Span
}

type FunctionDefinition = {
    kind: "fn"
    name: string
    parameters: Parameter[]
    block: Block
    transpile?: (fn: FunctionCall, args: string[]) => string
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

type VariableAssignment = {
    kind: "variable_assignment"
    variable: Variable
    value: Expression
    span: Span
}

type FunctionCall = {
    kind: "call"
    name: string
    arguments: Expression[]
    transpile?: (fn: FunctionCall, args: string[]) => string
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
}

type Type = {
    kind: "type"
    name: string
    transpile: (value: any) => string
    span: Span
}

type BinaryExpression = {
    kind: "binary"
    operator: "+" | "-" | "*" | "/" | "=="
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

const builtin_types = {
    i32: {
        name: "i32",
        kind: "type",
        transpile: (value: any) => value,
        span: {start: 0, end: 0},
    } as Type,
}

const unknown_type: Type = {
    name: "unknown",
    kind: "type",
    transpile: () => {
        throw new Error("The 'unknown' type cannot be transpiled")
    },
    span: new Span(0, 0, ""),
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
    | FunctionCall
    | Variable
    | VariableDeclaration
    | BinaryExpression
    | If
    | Block
    | VariableAssignment
    | Loop
    | Break
    | Continue

type AST = Expression[]

type Environment = {
    functions: Record<string, FunctionDefinition>
    variables: Record<string, Variable>
    types: Record<string, Type>
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
        } else if (c === "=" && peek(1) === ">") {
            tokens.push({kind: "simple", value: "=>", span: span()})
            skip(2)
        } else if (["(", ")", ":", ",", "=", "+", "-", "*", "/"].includes(c)) {
            tokens.push({kind: "simple", value: c as any, span: span()})
            skip()
        } else if (c.match(/\s/)) {
            skip()
        } else if (c.match(/[a-z]/)) {
            let value = ""
            let start_span = span()
            while (peek().match(/[a-zA-Z0-9_]/)) {
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
            tokens.push({kind: "number", value, span: span(start_span)})
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
        "*": 3,
        "/": 3,
        "+": 2,
        "-": 2,
        "==": 1,
    }
    let env: Environment = {
        functions: {
            // Built-in functions:
            print: {
                kind: "fn",
                name: "print",
                parameters: [
                    {
                        kind: "parameter",
                        name: "value",
                        type: builtin_types.i32,
                        span: new Span(0, 0, ""),
                    },
                ],
                block: {} as any,
                transpile: (_: FunctionCall, args: string[]) => `console.log(${args.join(",")});`,
                span: new Span(0, 0, ""),
            },
        },
        types: {...builtin_types},
        variables: {},
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
                `Expected token of kind ${token_kind} but got ${JSON.stringify(actual)} at ${
                    actual.span
                }`,
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
                // This is not a binary operator.
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
        } else if (simple_token === "return") {
            expression = parse_return_expression()
        } else if (token.kind === "number") {
            expression = consume() as Number_
        } else if (token.kind === "identifier") {
            const token = expect("identifier")
            expression = parse_function_call(token) || parse_variable(token)
            if (!expression) {
                throw new Error(`Unknown identifier ${token.value} at ${token.span}`)
            }
            if (simple_peek() === "=") {
                consume()
                const value = parse_expression()
                if (expression.kind === "variable") {
                    expression = {
                        kind: "variable_assignment",
                        variable: expression,
                        value,
                        span: Span.combine(expression.span, value.span),
                    }
                } else {
                    throw new Error(
                        `Cannot assign to ${JSON.stringify(expression)} at ${expression.span}`,
                    )
                }
            }
        }
        if (!expression) {
            throw new Error(
                `Unexpected token ${JSON.stringify(token)} at ${token.span} ${simple_token}`,
            )
        }
        return expression
    }
    function parse_block(if_else_mode?: "if_else_mode") {
        const block_type = consume()
        if (block_type.kind !== "simple" || !["=>", ":"].includes(block_type.value)) {
            throw new Error(
                `Expected block start (':' or '=>') but got ${JSON.stringify(block_type)} at ${
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
        let type = unknown_type
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
    function parse_function_definition(): FunctionDefinition {
        const span = expect("fn").span
        const name = expect("identifier").value
        expect("(")
        const parameters = []
        while (i < tokens.length && simple_peek() !== ")") {
            if (parameters.length > 0) {
                expect(",")
            }
            const name_token = expect("identifier")
            const type_token = expect("identifier")
            parameters.push({
                kind: "parameter",
                name: name_token.value,
                type: find_in_env("types", type_token.value),
                span: Span.combine(name_token.span, type_token.span),
            })
        }
        expect(")")
        const fn_def: FunctionDefinition = {kind: "fn", name, parameters, block: {} as any, span}
        env.functions[name] = fn_def
        env = {functions: {}, types: {}, variables: {}, outer: env}
        env.functions[name] = fn_def
        for (const parameter of fn_def.parameters) {
            env.variables[parameter.name] = {
                kind: "variable",
                mutable: false,
                name: parameter.name,
                type: parameter.type,
                span: parameter.span,
            }
        }
        fn_def.block = parse_block()
        fn_def.span = Span.combine(span, fn_def.block.span)
        env = env.outer
        return fn_def
    }
    function parse_return_expression(): ReturnExpression {
        const span = expect("return").span
        const value = parse_expression()
        return {kind: "return", value, span: Span.combine(span, value.span)}
    }
    function parse_function_call(token: Identifier): FunctionCall | undefined {
        const name = token.value
        const fn: FunctionDefinition | undefined = find_in_env("functions", name)
        if (fn) {
            expect("(")
            const args = []
            while (i < tokens.length && simple_peek() !== ")") {
                if (args.length > 0) {
                    expect(",")
                }
                args.push(parse_expression())
            }
            const span = Span.combine(expect(")").span, token.span)
            return {kind: "call", name: fn.name, arguments: args, transpile: fn.transpile, span}
        }
    }
    function parse_variable(token: Identifier): Variable | undefined {
        const name = token.value
        const variable: Variable | undefined = find_in_env("variables", name)
        if (variable) {
            return {...variable, span: token.span}
        }
    }
    function find_in_env<T>(
        kind: "functions" | "variables" | "types",
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

/**
 * Transpile AST to JavaScript.
 */
function transpile(ast: AST) {
    let res = ""
    for (const expression of ast) {
        if (expression.kind === "fn") {
            const parameters = expression.parameters.map((x) => x.name).join(",")
            const block = transpile_expression(expression.block)
            res += `function ${expression.name}(${parameters})${block}`
        } else {
            throw new Error(
                `Unexpected expression ${JSON.stringify(expression)} at ${expression.span}`,
            )
        }
    }
    function transpile_expression(expression: Expression) {
        if (expression.kind === "return") {
            return `return ${transpile_expression(expression.value)};`
        } else if (expression.kind === "number") {
            return expression.value
        } else if (expression.kind === "variable") {
            return expression.name
        } else if (expression.kind === "call") {
            const args = expression.arguments.map(transpile_expression)
            if (expression.transpile) {
                return expression.transpile(expression, args)
            } else {
                return `${expression.name}(${args.join(",")});`
            }
        } else if (expression.kind === "var") {
            const kind = expression.mutable ? "let" : "const"
            const value = transpile_expression(expression.value)
            return `${kind} ${expression.name} = ${value};`
        } else if (expression.kind === "variable_assignment") {
            const value = transpile_expression(expression.value)
            return `${expression.variable.name} = ${value};`
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
            const operator = expression.operator === "==" ? "===" : expression.operator
            return `(${lhs} ${operator} ${rhs})`
        } else {
            throw new Error(
                `Unexpected expression ${JSON.stringify(expression)} at ${expression.span}`,
            )
        }
    }
    return res
}

function assert(condition: boolean, msg: string) {
    if (!condition) {
        throw new Error(msg)
    }
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
    const tokens = lexer(src)
    if (debug_tokens) {
        console.log("\nTOKENS:")
        console.log(JSON.stringify(tokens, null, 2))
    }
    const ast = parse(tokens)
    if (debug_ast) {
        console.log("\nAST:")
        console.log(JSON.stringify(ast, null, 2))
    }
    let transpiled = transpile(ast)
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
    await Bun.spawn(["chmod", "+x", dst])
}

cli()
