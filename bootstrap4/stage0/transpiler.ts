/**
 * Transpile Klar to JavaScript.
 */
declare const Bun: any
// @ts-ignore
const dir = import.meta.dir

type Token = ValueToken | "(" | ")" | ":" | "fn" | "end" | "return"

type ValueToken = Identifier | Number_

type Identifier = {
    kind: "identifier"
    value: string
}

type Parameter = {
    kind: "parameter"
    name: string
}

type FunctionDefinition = {
    kind: "fn"
    name: string
    parameters: Parameter[]
    body: Expression[]
    transpile?: (fn: FunctionCall, args: string[]) => string
}

type FunctionCall = {
    kind: "call"
    name: string
    arguments: Expression[]
    transpile?: (fn: FunctionCall, args: string[]) => string
}

type ReturnExpression = {
    kind: "return"
    value: Expression
}

type Number_ = {
    kind: "number"
    value: string
}

type Expression = FunctionDefinition | ReturnExpression | Number_ | FunctionCall

type AST = Expression[]

type Environment = {
    functions: {[key: string]: FunctionDefinition}
}

/**
 * Lex Klar source code into tokens.
 */
function lexer(src: string) {
    const tokens: Token[] = []
    let i = 0
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
        if (["(", ")", ":"].includes(c)) {
            tokens.push(consume() as Token)
        } else if (c.match(/\s/)) {
            skip()
        } else if (c.match(/[a-z]/)) {
            let value = ""
            while (peek().match(/[a-zA-Z0-9]/)) {
                value += consume()
            }
            if (["return", "fn", "end"].includes(value)) {
                tokens.push(value as Token)
            } else {
                tokens.push({kind: "identifier", value})
            }
        } else if (c.match(/[0-9]/)) {
            let value = ""
            while (peek().match(/[0-9]/)) {
                value += consume()
            }
            tokens.push({kind: "number", value})
        } else if (c === "-" && peek(1) === "-" && peek(2) === "-") {
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
        } else {
            throw new Error(`Unexpected char ${c}`)
        }
    }
    return tokens
}

/**
 * Parse tokens into an AST.
 */
function parse(tokens: Token[]): AST {
    const env: Environment = {
        functions: {
            // Built-in functions:
            print: {
                kind: "fn",
                name: "print",
                parameters: [{kind: "parameter", name: "value"}],
                body: [],
                transpile: (_: FunctionCall, args: string[]) => `console.log(${args.join(",")});`,
            },
        },
    }
    const ast: AST = []
    let i = 0
    function peek(ahead = 0) {
        return tokens[i + ahead] as Token & {kind?: any; value?: any}
    }
    function consume() {
        return tokens[i++] as Token & {kind?: any; value?: any}
    }
    function expect(token_kind: "identifier" | "(" | ")" | ":" | "end") {
        let actual = peek() as any
        if (actual.kind) {
            actual = actual.kind
        }
        if (actual !== token_kind) {
            throw new Error(`Expected token of kind ${token_kind} but got ${actual}`)
        }
        return consume() as Token & {kind?: any; value?: any}
    }
    while (i < tokens.length) {
        ast.push(parse_expression())
    }
    function parse_expression(): Expression {
        const token = consume()
        if (token === "fn") {
            return parse_function_definition()
        } else if (token === "return") {
            return parse_return_expression()
        } else if (token.kind === "number") {
            return token as Number_
        } else if (token.kind === "identifier") {
            return parse_function_call(token.value)
        }
        throw new Error(`Unexpected token ${JSON.stringify(token)}`)
    }
    function parse_function_definition(): FunctionDefinition {
        const name = (expect("identifier") as Identifier).value
        expect("(")
        expect(")")
        expect(":")
        const body = []
        while (i < tokens.length && peek() !== "end") {
            body.push(parse_expression())
        }
        expect("end")
        return {kind: "fn", name, parameters: [], body}
    }
    function parse_return_expression(): ReturnExpression {
        return {kind: "return", value: parse_expression()}
    }
    function parse_function_call(name: string): FunctionCall {
        const fn = env.functions[name]
        if (fn) {
            expect("(")
            const args = []
            while (i < tokens.length && tokens[i] !== ")") {
                args.push(parse_expression())
            }
            expect(")")
            return {kind: "call", name: fn.name, arguments: args, transpile: fn.transpile}
        } else {
            throw new Error(`Undefined function ${name}`)
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
        if (expression.kind === "fn") {
            res += `function ${expression.name}() {`
            for (const body_expr of expression.body) {
                res += transpile_expression(body_expr)
            }
            res += "}"
        } else {
            throw new Error(`Unexpected expression ${JSON.stringify(expression)}`)
        }
    }
    function transpile_expression(expression: Expression) {
        if (expression.kind === "return") {
            return `return ${transpile_expression(expression.value)};`
        } else if (expression.kind === "number") {
            return expression.value
        } else if (expression.kind === "call") {
            const args = expression.arguments.map(transpile_expression)
            if (expression.transpile) {
                return expression.transpile(expression, args)
            } else {
                return `${expression.name}(${args.join(",")});`
            }
        } else {
            throw new Error(`Unexpected expression ${JSON.stringify(expression)}`)
        }
    }
    return res
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
    const ast = parse(tokens)
    let transpiled = transpile(ast)
    if (prettify) {
        const proc = Bun.spawn(["prettier", "--stdin-filepath", "transpiled.js"], {stdin: "pipe"})
        proc.stdin.write(transpiled)
        proc.stdin.end()
        transpiled = await new Response(proc.stdout).text()
    }
    if (debug_tokens) {
        console.log("\nTOKENS:")
        console.log(JSON.stringify(tokens, null, 2))
    }
    if (debug_ast) {
        console.log("\nAST:")
        console.log(JSON.stringify(ast, null, 2))
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
