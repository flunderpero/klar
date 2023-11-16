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

/**
 * TODO: Implement these checks
 * - all trait functions are implemented with the correct signature
 * - an enum variant cannot be used in a binary expression
 */
function semantic_analysis({ast}: {ast: AST.AST}) {
    const visited: any[] = []
    for (const expression of ast.body) {
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
            // fixme: we first have to resolve all types.
        }
    }
    function check_all_trait_functions_are_implemented(impl: AST.Expression) {
        if (!(impl instanceof AST.ImplDefinition)) {
            return
        }
        if (!impl.trait_name) {
            return
        }
        const trait = impl.resolved_trait!
        for (const signature of trait.member_function_declarations) {
            const fn = impl.member_functions.find((x) => x.declaration.name === signature.name)
            if (!fn) {
                throw new SemanticAnalysisError(
                    `Missing implementation of function ${quote(
                        signature.to_signature_string(),
                    )} of trait ${quote(trait.to_signature_string())}`,
                    signature.span,
                )
            }
        }
        for (const signature of trait.static_function_declarations) {
            const fn = impl.static_functions.find((x) => x.declaration.name === signature.name)
            if (!fn) {
                throw new SemanticAnalysisError(
                    `Missing implementation of function ${quote(
                        signature.to_signature_string(),
                    )} of trait ${quote(trait.to_signature_string())}`,
                    signature.span,
                )
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
    for (const expression of ast.body) {
        res += transpile_expression(expression)
        res += "\n"
    }
    function transpile_expression(e: AST.Expression): string {
        if (e instanceof AST.FunctionDefinition) {
            const parameters = e.declaration.parameters.map((x) => x.name).join(",")
            const block = transpile_expression(e.block)
            return `function ${e.declaration.name}(${parameters})${block}`
        } else if (e instanceof AST.Return) {
            return `return ${transpile_expression(e.value)};`
        } else if (e instanceof AST.Bool || e instanceof AST.Number_) {
            return `${e.value}`
        } else if (e instanceof AST.FunctionCall) {
            const args = e.arguments.map(transpile_expression)
            if (e.target instanceof AST.FunctionDefinition) {
                return `${e.target.declaration.name}(${args.join(",")})\n`
            } else {
                const target = transpile_expression(e.target)
                return `${target}(${args.join(",")})\n`
            }
        } else if (e instanceof AST.StructInstantiation) {
            const create_instance = `let _ = new ${e.target_struct_name}()`
            const assign_members = Object.entries(e.values).map(
                ([name, value]) => `_.${name} = ${transpile_expression(value)};`,
            )
            return `(() => {${create_instance};${assign_members.join("\n")} return _})()`
        } else if (e instanceof AST.VariableDeclaration) {
            const kind = e.mutable ? "let" : "const"
            if (e.value) {
                const value = transpile_expression(e.value)
                return `${kind} ${e.name} = ${value};`
            } else {
                return `${kind} ${e.name};`
            }
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
        } else if (e instanceof AST.StructDeclaration) {
            const members = Object.keys(e.members).join(";")
            const impls = transpile_impl_definitions(e)
            return `class ${e.name} {${members}\n${impls}}`
        } else if (e instanceof AST.EnumDeclaration) {
            let variants = ""
            for (const variant of Object.values(e.variants)) {
                const members = Object.keys(variant.members).join(";")
                const variant_class_name = `${e.name}_${variant.name}`
                variants += `class ${variant_class_name} extends ${e.name} {${members}};`
            }
            const impls = transpile_impl_definitions(e)
            const base_class = `class ${e.name} {${impls}}`
            return `${base_class}${variants}`
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
            if (
                e.target instanceof AST.IdentifierReference &&
                e.target.resolved instanceof AST.EnumDeclaration
            ) {
                return `(new ${e.target.name}_${e.field}())`
            }
            const target = transpile_expression(e.target)
            return `${target}.${e.field}`
        } else if (e instanceof AST.Not) {
            const value = transpile_expression(e.value)
            return `!${value}`
        } else if (e instanceof AST.IdentifierReference) {
            return e.resolved!.name
        } else if (e instanceof AST.ImplDefinition) {
            return ""
        } else if (e instanceof AST.TraitDeclaration) {
            return ""
        } else if (e instanceof AST.ExternBlock) {
            return ""
        } else {
            throw new CodeGenError(`Unexpected expression ${quote(e.kind)}`, e.span)
        }
    }
    function transpile_impl_definitions(e: AST.StructDeclaration | AST.EnumDeclaration) {
        let impls = ""
        function transpile_impl_function(fn: AST.FunctionDefinition, is_static: boolean) {
            const parameters = fn.declaration.parameters
                .filter((x) => x.name !== "self")
                .map((x) => x.name)
                .join(",")
            const block = transpile_expression(fn.block).replace("{", "{const self = this;")
            impls += `${is_static ? "static " : ""}${fn.declaration.name}(${parameters})${block}`
        }
        for (const impl of e.impls) {
            for (const fn of impl.member_functions) {
                transpile_impl_function(fn, false)
            }
            for (const fn of impl.static_functions) {
                transpile_impl_function(fn, true)
            }
        }
        return impls
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
    link` is a big word here, we just add the JS prelude and epilogue. :)
 */
async function link({compiled, prelude}: {compiled: string; prelude: string}) {
    return (
        (await Bun.file(`${dir}/prelude_js.js`).text()) +
        prelude +
        compiled +
        (await Bun.file(`${dir}/epilogue_js.js`).text())
    )
}

async function compile_prelude(): Promise<{env: AST.Environment; prelude: string}> {
    let captured_env: AST.Environment | undefined
    const env = new AST.Environment(undefined, (x) => {
        if (!captured_env) {
            captured_env = x
        }
    })
    const prelude = await compile({file: `${dir}/prelude_js.kl`, env})
    return {env: captured_env!, prelude}
}

/**
 * Main entry point.
 */
async function cli() {
    const {env, prelude} = await compile_prelude()
    const file = Bun.argv[2]
    const compiled = await compile({file: file, env})
    const res = await link({compiled, prelude})
    const dst = `${dir}/build/${(file.split("/")?.pop() ?? file).split(".")[0]}`
    await Bun.write(dst, res)
    const proc = Bun.spawn(["chmod", "+x", dst])
    await proc.exited
}

cli()
