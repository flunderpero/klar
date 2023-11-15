/**
 * Transpile Klar to JavaScript.
 */

import {quote, Span} from "./common"
import * as Lexer from "./lexer"
import * as AST from "./parser"

// @ts-ignore
const dir = import.meta.dir
const prettify = Bun.argv.includes("--pretty")
const debug = Bun.argv.includes("--debug")
const debug_tokens = Bun.argv.includes("--debug-tokens") || debug
const debug_ast = Bun.argv.includes("--debug-ast") || debug
const debug_transpiled = Bun.argv.includes("--debug-transpiled") || debug

class SemanticAnalysisError extends Error {
    constructor(
        public error: string,
        public span: Span,
    ) {
        super(`${error} at ${span}`)
    }
}

class CodeGenError extends Error {
    constructor(
        public error: string,
        public span: Span,
    ) {
        super(`${error} at ${span}`)
    }
}

function semantic_analysis({ast}: {ast: AST.AST}) {
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
    function check_mutability(expression: AST.Expression) {
        if (expression instanceof AST.Assignment) {
            const target = expression.target
            if (target instanceof AST.Variable) {
                if (!target.mutable) {
                    throw new SemanticAnalysisError(
                        `Cannot assign to immutable variable ${quote(target.name)}`,
                        expression.span,
                    )
                }
            }
            // fixme: check field access
        }
    }
    function check_all_trait_functions_are_implemented(expression: AST.Expression) {
        if (!(expression instanceof AST.StructDefinition)) {
            return
        }
        for (const impl of expression.impls) {
            if (!impl.trait) {
                continue
            }
            for (const signature of impl.trait.member_function_signatures) {
                const fn = impl.member_functions.find((x) => x.signature.name === signature.name)
                if (!fn) {
                    throw new SemanticAnalysisError(
                        `Missing implementation of function ${quote(
                            signature.to_signature_string(),
                        )} of trait ${quote(impl.trait.to_signature_string())}`,
                        signature.span,
                    )
                }
            }
            for (const signature of impl.trait.static_function_signatures) {
                const fn = impl.static_functions.find((x) => x.signature.name === signature.name)
                if (!fn) {
                    throw new SemanticAnalysisError(
                        `Missing implementation of function ${quote(
                            signature.to_signature_string(),
                        )} of trait ${quote(impl.trait.to_signature_string())}`,
                        signature.span,
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
function code_gen(ast: AST.AST) {
    let res = ""
    for (const expression of ast) {
        res += transpile_expression(expression)
        res += "\n"
    }
    function transpile_expression(e: AST.Expression): string {
        if (e instanceof AST.FunctionDefinition) {
            const parameters = e.signature.parameters.map((x) => x.name).join(",")
            const block = transpile_expression(e.block)
            return `function ${e.signature.name}(${parameters})${block}`
        } else if (e instanceof AST.Return) {
            return `return ${transpile_expression(e.value)};`
        } else if (e instanceof AST.Bool || e instanceof AST.Number_) {
            return `${e.value}`
        } else if (e instanceof AST.Variable) {
            return e.name
        } else if (e instanceof AST.FunctionCall) {
            const args = e.arguments.map(transpile_expression)
            if (e.target instanceof AST.FunctionDefinition) {
                return `${e.target.signature.name}(${args.join(",")})\n`
            } else {
                const target = transpile_expression(e.target)
                return `${target}(${args.join(",")})\n`
            }
        } else if (e instanceof AST.StructInstantiation) {
            const create_instance = `let _ = new ${e.target.name}()`
            const assign_members = Object.entries(e.values).map(
                ([name, value]) => `_.${name} = ${transpile_expression(value)};`,
            )
            return `(() => {${create_instance};${assign_members.join("\n")} return _})()`
        } else if (e instanceof AST.VariableDeclaration) {
            const kind = e.mutable ? "let" : "const"
            const value = transpile_expression(e.value)
            return `${kind} ${e.name} = ${value};`
        } else if (e instanceof AST.Assignment) {
            const target = transpile_expression(e.target)
            const value = transpile_expression(e.value)
            return `${target} = ${value};`
        } else if (e instanceof AST.Block) {
            const body = e.body.map(transpile_expression).join("")
            return `{${body}}`
        } else if (e instanceof AST.If) {
            const condition = transpile_expression(e.condition)
            const then = transpile_expression(e.then_block)
            let s = `if (${condition}) ${then}`
            if (e.else_block) {
                const else_ = transpile_expression(e.else_block)
                s += ` else ${else_}`
            }
            return s
        } else if (e instanceof AST.StructDefinition) {
            const members = Object.keys(e.members).join(";")
            const parameters = Object.keys(e.members)
                .map((x) => `${x}`)
                .join(",")
            const constructor_body = Object.keys(e.members)
                .map((x) => `this.${x} = ${x};`)
                .join("")

            let impls = ""
            function transpile_impl_function(fn: AST.FunctionDefinition, is_static: boolean) {
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
        } else if (e instanceof AST.UnitTypeExpression) {
            return "undefined"
        } else if (e instanceof AST.Loop) {
            const block = transpile_expression(e.block)
            return `while (true) ${block}`
        } else if (e instanceof AST.Break) {
            return "break;"
        } else if (e instanceof AST.Continue) {
            return "continue;"
        } else if (e instanceof AST.BinaryExpression) {
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
        } else if (e instanceof AST.FieldAccess) {
            const object = transpile_expression(e.object)
            return `${object}.${e.field}`
        } else if (e instanceof AST.NamespaceAccess) {
            return e.namespace.name
        } else if (e instanceof AST.Not) {
            const value = transpile_expression(e.value)
            return `!${value}`
        } else {
            throw new CodeGenError(`Unexpected expression ${quote(e.kind)}`, e.span)
        }
    }
    return res
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
    env: AST.Environment
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
        const tokens = Lexer.lexer({file: file.split("/").pop() ?? file, src})
        if (debug_tokens && !disable_debug) {
            console.log(`\n${log_prefix} TOKENS`)
            console.log(to_json(tokens, 2))
        }
        let ast = AST.parse(new AST.TokenStream(tokens), env)
        if (debug_ast && !disable_debug) {
            console.log(`\n${log_prefix} AST:`)
            console.log(to_json(ast, 2))
        }
        ast = semantic_analysis({ast})
        transpiled = code_gen(ast)
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

    function print(value) {
        console.log(value)
    }

`
    const env = new AST.Environment()
    env.functions["print"] = new AST.FunctionDefinition(
        {
            signature: new AST.FunctionSignature(
                {
                    name: "print",
                    parameters: [
                        new AST.Parameter(
                            {name: "value", mutable: false, type: AST.builtin_types.i32},
                            new Span(0, 0, "<builtin>", ""),
                        ),
                    ],
                    return_type: AST.builtin_types.unit_type,
                },
                new Span(0, 0, "<builtin>", ""),
            ),
            block: new AST.Block({body: []}, new Span(0, 0, "<builtin>", "")),
        },
        new Span(0, 0, "<builtin>", ""),
    )
    env.types = {...AST.builtin_types}
    const file = Bun.argv[2]
    const transpiled = await compile({file: file, env})
    const res = prelude + transpiled
    const dst = `${dir}/build/${(file.split("/")?.pop() ?? file).split(".")[0]}`
    await Bun.write(dst, res)
    const proc = Bun.spawn(["chmod", "+x", dst])
    await proc.exited
}

cli()
