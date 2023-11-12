/**
 * Transpile Klar to JavaScript.
 */
declare const Bun: any
declare const process: any
interface ObjectConstructor {
    entries<T>(obj: Record<string, T>): [string, T][]
}
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
        | "."
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
        | "struct"
        | "impl"
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

type FunctionDefinition = {
    kind: "fn"
    name: string
    parameters: Parameter[]
    block: Block
    transpile?: (fn: FunctionCall, args: string[]) => string
    span: Span
}

type Impl = {
    kind: "impl"
    functions: FunctionDefinition[]
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
    target: FunctionDefinition | FieldAccess
    arguments: Expression[]
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
    transpile?: (value: any) => string
    span: Span
    members?: Record<string, Type>
}

type TypeDefinition = {
    kind: "type_definition"
    name: string
    span: Span
    members: Record<string, Type>
    impls: Impl[]
}

type FieldAccess = {
    kind: "field_access"
    object: Expression
    field: string
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

type UnitType = {
    kind: "unit_type"
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
    | Assignment
    | Loop
    | Break
    | Continue
    | TypeDefinition
    | FieldAccess
    | UnitType

type AST = Expression[]

type Environment = {
    functions: Record<string, FunctionDefinition>
    variables: Record<string, Variable>
    types: Record<string, Type>
    type_definitions: Record<string, TypeDefinition>
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
        } else if (["(", ")", ":", ".", ",", "=", "+", "-", "*", "/"].includes(c)) {
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
                    "impl",
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
                        mutable: false,
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
        type_definitions: {},
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
        } else if (simple_token === "struct") {
            expression = parse_type_definition()
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
        } else if (simple_token === "impl") {
            expression = parse_impl()
        }
        if (!expression) {
            throw new Error(
                `Unexpected token ${JSON.stringify(token)} at ${token.span} ${simple_token}`,
            )
        }
        expression = parse_field_access(expression)
        if (simple_peek() === "(") {
            expression = parse_function_call(expression) || expression
        }
        return expression
    }
    function parse_assignment(target: Expression): Assignment {
        assert(
            target.kind === "variable" || target.kind === "field_access",
            `Invalid target for field access: ${JSON.stringify(target)} at ${target.span}`,
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
    function parse_field_access(expression: Expression): FieldAccess {
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
        return expression as FieldAccess
    }
    function parse_impl(): UnitType {
        const span = consume().span
        const name = expect("identifier").value
        const target: TypeDefinition = find_in_env("type_definitions", name)
        if (!target) {
            throw new Error(`Unknown type ${target} at ${span}`)
        }
        const target_type: Type = find_in_env("types", target.name)
        expect(":")
        const functions: FunctionDefinition[] = []
        while (i < tokens.length && simple_peek() !== "end") {
            functions.push(parse_function_definition(target_type))
        }
        const end_span = expect("end").span
        target.impls.push({kind: "impl", functions, span: Span.combine(span, end_span)})
        return {kind: "unit_type", span}
    }
    function parse_type_definition(): TypeDefinition {
        const span = consume().span
        const name = expect("identifier").value
        const members: Record<string, Type> = {}
        expect(":")
        while (i < tokens.length && simple_peek() !== "end") {
            const name = expect("identifier").value
            const type: Type = find_in_env("types", expect("identifier").value)
            if (!type) {
                throw new Error(`Unknown type ${name} at ${name.span}`)
            }
            members[name] = type
        }
        expect("end")
        const type_def: TypeDefinition = {kind: "type_definition", name, impls: [], span, members}
        env.types[name] = {
            kind: "type",
            name,
            span,
            members,
        }
        env.type_definitions[name] = type_def
        const constructor: FunctionDefinition = {
            kind: "fn",
            name,
            parameters: Object.entries(members).map(([name, type]) => ({
                kind: "parameter",
                name,
                type,
                span,
                mutable: false,
            })),
            block: {} as any,
            transpile: (_: FunctionCall, args: string[]) => `new ${name}(${args.join(",")})`,
            span,
        }
        env.functions[name] = constructor
        return type_def
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
    function parse_function_definition(impl_type?: Type): FunctionDefinition {
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
        const fn_def: FunctionDefinition = {kind: "fn", name, parameters, block: {} as any, span}
        env = {functions: {}, types: {}, variables: {}, type_definitions: {}, outer: env}
        if (!impl_type) {
            env.outer.functions[name] = fn_def
            env.functions[name] = fn_def
        }
        for (const parameter of fn_def.parameters) {
            env.variables[parameter.name] = {
                ...parameter,
                kind: "variable",
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
    function parse_function_call(token: Identifier | FieldAccess): FunctionCall | undefined {
        if (simple_peek() !== "(") {
            return
        }
        expect("(")
        let target: FunctionDefinition | FieldAccess
        if (token.kind === "identifier") {
            target = find_in_env("functions", token.value)
        } else {
            target = token
        }
        if (target) {
            const args = []
            while (i < tokens.length && simple_peek() !== ")") {
                if (args.length > 0) {
                    expect(",")
                }
                args.push(parse_expression())
            }
            const span = Span.combine(expect(")").span, token.span)
            return {kind: "call", target, arguments: args, span}
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
        kind: "functions" | "variables" | "types" | "type_definitions",
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
        if (visited.includes(obj)) {
            return
        }
        visited.push(obj)
        if (obj.kind) {
            check_mutability(obj)
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
    function transpile_expression(expression: Expression) {
        if (expression.kind === "fn") {
            const parameters = expression.parameters.map((x) => x.name).join(",")
            const block = transpile_expression(expression.block)
            return `function ${expression.name}(${parameters})${block}`
        } else if (expression.kind === "return") {
            return `return ${transpile_expression(expression.value)};`
        } else if (expression.kind === "number") {
            return expression.value
        } else if (expression.kind === "variable") {
            return expression.name
        } else if (expression.kind === "call") {
            const args = expression.arguments.map(transpile_expression)
            if (expression.target.kind === "fn") {
                if (expression.target.transpile) {
                    return expression.target.transpile(expression, args)
                }
                return `${expression.target.name}(${args.join(",")});`
            } else {
                const target = transpile_expression(expression.target)
                return `${target}(${args.join(",")});`
            }
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
            for (const impl of expression.impls) {
                for (const fn of impl.functions) {
                    const parameters = fn.parameters
                        .filter((x) => x.name !== "self")
                        .map((x) => x.name)
                        .join(",")
                    const block = transpile_expression(fn.block).replace("{", "{const self = this;")
                    impls += `${fn.name}(${parameters})${block}`
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
            const operator = expression.operator === "==" ? "===" : expression.operator
            return `(${lhs} ${operator} ${rhs})`
        } else if (expression.kind === "field_access") {
            const object = transpile_expression(expression.object)
            return `${object}.${expression.field}`
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
    throw new Error(`Unexpected object ${JSON.stringify(x)}`)
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
            console.log(JSON.stringify(tokens, null, 2))
        }
        let ast = parse(tokens)
        if (debug_ast) {
            console.log("\nAST:")
            console.log(JSON.stringify(ast, null, 2))
        }
        ast = check(ast)
        transpiled = transpile(ast)
    } catch (error) {
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
    await Bun.spawn(["chmod", "+x", dst])
}

cli()
