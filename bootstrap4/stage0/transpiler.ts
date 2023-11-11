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
    value: "(" | ")" | ":" | "fn" | "end" | "return"
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

type FunctionDefinition = {
    kind: "fn"
    name: string
    parameters: Parameter[]
    body: Expression[]
    transpile?: (fn: FunctionCall, args: string[]) => string
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

const builtin_types = {
    i32: {
        name: "i32",
        kind: "type",
        transpile: (value: any) => value,
        span: {start: 0, end: 0},
    } as Type,
}

type Variable = {
    kind: "variable"
    name: string
    type: Type
    span: Span
}

type Expression = FunctionDefinition | ReturnExpression | Number_ | FunctionCall | Variable

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
        if (["(", ")", ":", ","].includes(c)) {
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
            if (["return", "fn", "end"].includes(value)) {
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
            throw new Error(`Unexpected char ${c} at ${span()}`)
        }
    }
    return tokens
}

/**
 * Parse tokens into an AST.
 */
function parse(tokens: Token[]): AST {
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
                body: [],
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
        return token.kind === "simple" ? token.value : undefined
    }
    function consume() {
        return tokens[i++] as Token & {kind?: any; value?: any}
    }
    function expect(token_kind: "identifier" | "(" | ")" | ":" | "," | "end" | "return" | "fn") {
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
        const token = peek()
        const simple_token = token.kind === "simple" ? token.value : undefined
        if (simple_token === "fn") {
            return parse_function_definition()
        } else if (simple_token === "return") {
            return parse_return_expression()
        } else if (token.kind === "number") {
            return consume() as Number_
        } else if (token.kind === "identifier") {
            const token = expect("identifier")
            const result = parse_function_call(token) || parse_variable(token)
            if (!result) {
                throw new Error(`Unexpected identifier ${token.value} at ${token.span}`)
            }
            return result
        }
        throw new Error(`Unexpected token ${JSON.stringify(token)} at ${token.span}`)
    }
    function parse_function_definition(): FunctionDefinition {
        const span = expect("fn").span
        const name = expect("identifier").value
        const fn_def: FunctionDefinition = {
            kind: "fn",
            name,
            parameters: [],
            body: [],
            span,
        }
        expect("(")
        while (i < tokens.length && simple_peek() !== ")") {
            if (fn_def.parameters.length > 0) {
                expect(",")
            }
            const name_token = expect("identifier")
            const type_token = expect("identifier")
            fn_def.parameters.push({
                kind: "parameter",
                name: name_token.value,
                type: find_in_env("types", type_token.value),
                span: Span.combine(name_token.span, type_token.span),
            })
        }
        expect(")")
        fn_def.span = Span.combine(span, expect(":").span)
        env.functions[name] = fn_def
        env = {functions: {}, types: {}, variables: {}, outer: env}
        env.functions[name] = fn_def
        for (const parameter of fn_def.parameters) {
            env.variables[parameter.name] = {
                kind: "variable",
                name: parameter.name,
                type: parameter.type,
                span: parameter.span,
            }
        }
        while (i < tokens.length && simple_peek() !== "end") {
            fn_def.body.push(parse_expression())
        }
        expect("end")
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
            res += `function ${expression.name}(${expression.parameters
                .map((x) => x.name)
                .join(",")}) {`
            for (const body_expr of expression.body) {
                res += transpile_expression(body_expr)
            }
            res += "}"
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
        } else {
            throw new Error(
                `Unexpected expression ${JSON.stringify(expression)} at ${expression.span}`,
            )
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
