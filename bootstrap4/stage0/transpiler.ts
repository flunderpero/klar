declare const Bun: any
// @ts-ignore
const dir = import.meta.dir

type Token = Identifier | Number_ | "(" | ")" | ":" | "fn" | "end" | "return"

type Identifier = {
    type: "identifier"
    value: string
}

type Parameter = {
    type: "parameter"
    name: Identifier
}

type FunctionDefinition = {
    type: "fn"
    name: Identifier
    parameters: Parameter[]
    body: Expression[]
    transpile?: (fn: FunctionCall, args: string[]) => string
}

type FunctionCall = {
    type: "call"
    name: Identifier
    arguments: Expression[]
    transpile?: (fn: FunctionCall, args: string[]) => string
}

type ReturnExpression = {
    type: "return"
    value: Expression
}

type Number_ = {
    type: "number"
    value: string
}

type Expression = FunctionDefinition | ReturnExpression | Number_ | FunctionCall

type AST = Expression[]

type Environment = {
    functions: {[key: string]: FunctionDefinition}
}

function lexer(src: string) {
    const tokens: Token[] = []
    let i = 0
    while (i < src.length) {
        const c = src[i]
        if (["(", ")", ":"].includes(c)) {
            tokens.push(c as any)
            i++
        } else if (c.match(/\s/)) {
            i++
        } else if (c.match(/[a-z]/)) {
            let value = ""
            while (src[i].match(/[a-zA-Z0-9]/)) {
                value += src[i]
                i++
            }
            if (["return", "fn", "end"].includes(value)) {
                tokens.push(value as any)
            } else {
                tokens.push({type: "identifier", value})
            }
        } else if (c.match(/[0-9]/)) {
            let value = ""
            while (src[i].match(/[0-9]/)) {
                value += src[i]
                i++
            }
            tokens.push({type: "number", value})
        } else if (src[i] === "-" && src[i + 1] === "-" && src[i + 2] === "-") {
            i += 3
            while (
                i < src.length - 1 &&
                !(src[i] === "-" && src[i + 1] === "-" && src[i + 2] === "-")
            ) {
                i++
            }
            i += 3
        } else if (src[i] === "-" && src[i + 1] === "-") {
            i += 2
            while (i < src.length - 1 && src[i] !== "\n") {
                i++
            }
        } else {
            throw new Error(`Unexpected char ${c}`)
        }
    }
    return tokens
}

function parse(tokens: any[]): AST {
    const env: Environment = {
        functions: {
            // Built-in functions:
            print: {
                type: "fn",
                name: {type: "identifier", value: "print"},
                parameters: [{type: "parameter", name: {type: "identifier", value: "x"}}],
                body: [],
                transpile: (_: FunctionCall, args: string[]) => `console.log(${args.join(",")});`,
            },
        },
    }
    const ast: AST = []
    let i = 0
    while (i < tokens.length) {
        ast.push(parse_expression())
    }
    function parse_expression() {
        if (tokens[i] === "fn") {
            return parse_function_definition()
        } else if (tokens[i] === "return") {
            return parse_return_expression()
        } else if (tokens[i]?.type === "number") {
            return tokens[i++]
        } else if (tokens[i]?.type === "identifier") {
            return parse_function_call()
        }
        throw new Error(`Unexpected token ${JSON.stringify(tokens[i])}`)
    }
    function parse_function_definition() {
        i++
        const name = tokens[i]
        i += 4 // skip "(", ")", and ":"
        const body = []
        while (i < tokens.length && tokens[i] !== "end") {
            body.push(parse_expression())
        }
        i++ // skip "end"
        return {type: "fn", name, body}
    }
    function parse_return_expression() {
        i++
        return {type: "return", value: parse_expression()}
    }
    function parse_function_call() {
        const fn = env.functions[tokens[i].value]
        if (fn) {
            i += 2 // skip fn name and "("
            const args = []
            while (i < tokens.length && tokens[i] !== ")") {
                args.push(parse_expression())
            }
            i++ // skip ")"
            return {type: "call", name: fn.name, arguments: args, transpile: fn.transpile}
        } else {
            throw new Error(`Undefined function ${tokens[i].value}`)
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
        if (expression.type === "fn") {
            res += `function ${expression.name.value}() {`
            for (const body_expr of expression.body) {
                res += transpile_expression(body_expr)
            }
            res += "}"
        } else {
            throw new Error(`Unexpected expression ${JSON.stringify(expression)}`)
        }
    }
    function transpile_expression(expression: Expression) {
        if (expression.type === "return") {
            return `return ${transpile_expression(expression.value)};`
        } else if (expression.type === "number") {
            return expression.value
        } else if (expression.type === "call") {
            const args = expression.arguments.map(transpile_expression)
            if (expression.transpile) {
                return expression.transpile(expression, args)
            } else {
                return `${expression.name.value}(${args.join(",")});`
            }
        } else {
            throw new Error(`Unexpected expression ${JSON.stringify(expression)}`)
        }
    }
    return res
}

const prelude = `#!/usr/bin/env bun --silent
process.exit(main())

`

async function cli() {
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
