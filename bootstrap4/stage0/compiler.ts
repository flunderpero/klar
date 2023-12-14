/**
 * Transpile Klar to JavaScript.
 */
import assert from "assert"
import {quote, Span, to_json} from "./common"
import * as Lexer from "./lexer"
import * as AST from "./parser"
import {type_check_ast, TypeEnvironment} from "./type_check2"

// @ts-ignore
const dir = import.meta.dir
const prettify = Bun.argv.includes("--pretty")
const debug = Bun.argv.includes("--debug")
const debug_tokens = Bun.argv.includes("--debug-tokens") || debug
const debug_ast = Bun.argv.includes("--debug-ast") || debug
const debug_transpiled = Bun.argv.includes("--debug-transpiled") || debug

class CodeGenError extends Error {
    constructor(
        public error: string,
        public span: Span,
    ) {
        super(`${error} at ${span}`)
    }
}

function traverse_ast(ast: AST.AST, f: (e: AST.ASTNode, parent: AST.ASTNode) => void) {
    const visited: any[] = []
    for (const expression of ast.body) {
        traverse(expression, ast.body)
    }
    function traverse(obj: any, parent: any) {
        if (obj == null) {
            return
        }
        if (visited.includes(obj)) {
            return
        }
        visited.push(obj)
        if (obj instanceof AST.ASTNode) {
            f(obj, parent)
        }
        for (const key of Object.keys(obj)) {
            const value = obj[key]
            if (Array.isArray(value)) {
                for (const child of value) {
                    traverse(child, obj)
                }
            } else {
                traverse(value, obj)
            }
        }
    }
}

function mangle_names(ast: AST.AST) {
    function mangle(name: string) {
        if (name.startsWith("klar_")) {
            return name
        }
        return `klar_${name}`
    }
    function do_mangle(e: any, parent: any) {
        if (
            e instanceof AST.FunctionDefinition &&
            !(parent instanceof AST.ImplDefinition || parent instanceof AST.TraitDeclaration)
        ) {
            e.declaration.name = mangle(e.declaration.name)
        } else if (e instanceof AST.StructDeclaration) {
            e.name = mangle(e.name)
        } else if (e instanceof AST.EnumDeclaration) {
            e.name = mangle(e.name)
        } else if (e instanceof AST.StructInstantiation) {
            e.target_struct_name = mangle(e.target_struct_name)
        } else if (e instanceof AST.VariableDeclaration) {
            if (e.name !== "self") {
                e.name = mangle(e.name)
            }
        } else if (e instanceof AST.Parameter || e instanceof AST.ClosureParameter) {
            if (e.name !== "self") {
                e.name = mangle(e.name)
            }
        } else if (e instanceof AST.CaptureMatchPatternOrType) {
            e.name = mangle(e.name)
        } else if (e instanceof AST.IdentifierReference) {
            e.name = mangle(e.name)
        } else if (e instanceof AST.ImplDefinition) {
            e.target_name = mangle(e.target_name)
        }
        if (e.attributes?.type?.declaration) {
            do_mangle(e.attributes.type.declaration, e)
        }
    }
    traverse_ast(ast, do_mangle)
}

/**
 * Transpile AST to JavaScript.
 */
function code_gen(ast: AST.AST) {
    mangle_names(ast)
    const binary_op_functions = {
        "==": "eq",
        "!=": "ne",
        "<": "lt",
        "<=": "le",
        ">": "gt",
        ">=": "ge",
        "+": "add",
        "-": "sub",
        "*": "mul",
        "/": "div",
    }
    let res = ""
    for (const expression of ast.body) {
        res += transpile_expression(expression, {})
        res += "\n"
    }
    type Context = {func?: AST.FunctionDeclaration | AST.ClosureDefinition}
    function transpile_expression(e: AST.Expression, ctx: Context): string {
        if (e instanceof AST.UnitOperator) {
            return transpile_expression(e.expression, ctx)
        } else if (e instanceof AST.FunctionDefinition) {
            const parameters = e.declaration.parameters.map((x) => x.name).join(",")
            const block = transpile_block(
                e.block,
                {...ctx, func: e.declaration},
                {return_last_expression: true},
            )
            return `function ${e.declaration.name}(${parameters})${block}`
        } else if (e instanceof AST.ClosureDefinition) {
            const parameters = e.parameters.map((x) => x.name).join(",")
            const block = transpile_block(
                e.block,
                {...ctx, func: e},
                {return_last_expression: true},
            )
            return `function (${parameters})${block}`
        } else if (e instanceof AST.Return) {
            if (!e.value) {
                return "return;"
            }
            const value = wrap_return_value(transpile_expression(e.value, ctx), ctx)
            return `return ${value};`
        } else if (e instanceof AST.Number_) {
            return `new i32(${e.value})`
        } else if (e instanceof AST.Bool || e instanceof AST.Number_) {
            return `new bool(${e.value})`
        } else if (e instanceof AST.Str) {
            let value = e.value.replace(/\\/g, "\\\\")
            value = value.replace(/\n/g, "\\n")
            value = value.replace(/\t/g, "\\t")
            value = value.replace(/"/g, "\\`")
            if (e.is_multiline) {
                value = value.replace(/`/g, "\\`")
                return `new str(\`${value}\`)`
            }
            return `new str("${value}")`
        } else if (e instanceof AST.InterpolatedStr) {
            const parts = e.expressions.map(
                (x) => `_.push(${transpile_expression(x, ctx)}.to_str());`,
            )
            return `(function () { const _ = new str(""); ${parts.join("")}; return _; })()`
        } else if (e instanceof AST.FunctionCall) {
            const args = e.args.map((x) => transpile_expression(x, ctx))
            let call
            if (e.target instanceof AST.FunctionDefinition) {
                call = `${e.target.declaration.name}(${args.join(",")})\n`
            } else {
                const target = transpile_expression(e.target, ctx)
                call = `${target}(${args.join(",")})\n`
            }
            if (e.propagate_error) {
                return `(function() {
                    let res = ${call};
                    if (res.constructor.name == "klar_Result_Err") {
                        const error = new Error("return")
                        error.value = res;
                        throw error;
                    }
                    if (res.constructor.name == "klar_Result_Ok") return res[0];
                    return res})()`
            }
            return call
        } else if (e instanceof AST.StructInstantiation) {
            const create_instance = `let _ = new ${e.target_struct_name}()`
            const assign_members = Object.entries(e.fields).map(
                ([name, value]) => `_.${name} = ${transpile_expression(value, ctx)};`,
            )
            return `(() => {${create_instance};${assign_members.join("\n")} return _})()`
        } else if (e instanceof AST.VariableDeclaration) {
            const kind = e.mutable ? "let" : "const"
            if (e.value) {
                const value = transpile_expression(e.value, ctx)
                return `${kind} ${e.name} = ${value};`
            } else {
                return `${kind} ${e.name};`
            }
        } else if (e instanceof AST.Assignment) {
            const value = transpile_expression(e.value, ctx)
            if (e.target instanceof AST.IndexedAccess) {
                return `${transpile_expression(e.target.target, ctx)}.set(${transpile_expression(
                    e.target.index,
                    ctx,
                )}, ${value});`
            }
            const target = transpile_expression(e.target, ctx)
            return `${target} = ${value};`
        } else if (e instanceof AST.ArrayLiteral) {
            const values = e.elements
                .map((x) => `_.push(${transpile_expression(x, ctx)});`)
                .join("")
            return `(function () { const _ = klar_Vector.new(); ${values} return _; })()`
        } else if (e instanceof AST.Block) {
            return transpile_block(e, ctx)
        } else if (e instanceof AST.If) {
            const condition = transpile_expression(e.condition, ctx)
            const then = transpile_block(e.then_block, ctx)
            let s = `if (${condition}.value) ${then}`
            if (e.else_block) {
                const else_ = transpile_block(e.else_block, ctx)
                s += ` else ${else_}`
            }
            return s
        } else if (e instanceof AST.StructDeclaration) {
            const members = Object.keys(e.fields).join(";")
            const impls = transpile_impls(e)
            return `class ${e.name} {${members}\n}${impls}`
        } else if (e instanceof AST.EnumDeclaration) {
            let variants = ""
            for (const variant of e.variants) {
                const variant_class_name = `${e.name}_${variant.name}`
                const constructor =
                    "constructor(...values) {super();values.forEach((x, i) => {this[i] = x;});}"
                variants += `class ${variant_class_name} extends ${e.name} {\n${constructor}};`
            }
            const base_class = `class ${e.name} {}`
            const impls = transpile_impls(e)
            return `${base_class}${variants}${impls}`
        } else if (e instanceof AST.Loop) {
            const block = transpile_block(e.block, ctx)
            // The `try / catch` is a bit of a hack to be able to break out of a match
            // expression.
            return `while (true) try { ${block} } catch (e) {
                if (e.message.startsWith("break")) break;
                if (e.message.startsWith("continue")) continue;
                throw e;
            }`
        } else if (e instanceof AST.Break) {
            return "break;"
        } else if (e instanceof AST.Continue) {
            return "continue;"
        } else if (e instanceof AST.BinaryExpression) {
            const lhs = transpile_expression(e.lhs, ctx)
            const rhs = transpile_expression(e.rhs, ctx)
            if (e.operator === "and") {
                return `new bool(${lhs}.value && ${rhs}.value)`
            } else if (e.operator === "or") {
                return `new bool(${lhs}.value || ${rhs}.value)`
            }
            const function_name = binary_op_functions[e.operator]
            if (!function_name) {
                throw new CodeGenError(`Unexpected operator ${quote(e.operator)}`, e.span)
            }
            return `(${lhs}.${function_name}(${rhs}))`
        } else if (e instanceof AST.FieldAccess) {
            if (
                e.target instanceof AST.IdentifierReference &&
                e.target.attributes.type?.declaration instanceof AST.EnumDeclaration
            ) {
                const declaration = e.target.attributes.type?.declaration
                assert(typeof e.field === "string")
                const variant = declaration.get_variant(e.field)
                if (variant) {
                    if (variant.fields.fields.length === 0) {
                        return `new ${declaration.name}_${e.field}()`
                    }
                    return `new ${declaration.name}_${e.field}`
                }
            }
            const target = transpile_expression(e.target, ctx)
            if (parseInt(e.field).toString() === e.field) {
                return `${target}[${e.field}]`
            }
            return `${target}.${e.field}`
        } else if (e instanceof AST.IndexedAccess) {
            const target = transpile_expression(e.target, ctx)
            const index = transpile_expression(e.index, ctx)
            return `${target}.get(${index})`
        } else if (e instanceof AST.Not) {
            const value = transpile_expression(e.expression, ctx)
            return `!${value}`
        } else if (e instanceof AST.ParenthesizedExpression) {
            const value = transpile_expression(e.expression, ctx)
            return `(${value})`
        } else if (e instanceof AST.TupleInstantiation) {
            const values = e.elements.map((x) => transpile_expression(x, ctx)).join(",")
            return `[${values}]`
        } else if (e instanceof AST.IdentifierReference) {
            return e.name
        } else if (e instanceof AST.ImplDefinition) {
            return ""
        } else if (e instanceof AST.TraitDeclaration) {
            return ""
        } else if (e instanceof AST.ExternBlock) {
            return ""
        } else if (e instanceof AST.Match) {
            return transpile_match(e, ctx)
        } else if (e instanceof AST.UnitLiteral) {
            return "undefined"
        } else {
            throw new CodeGenError(`Unexpected expression ${quote(e.kind)}`, e.span)
        }
    }
    function wrap_return_value(value: string, ctx: Context) {
        if (ctx.func?.throws) {
            // Wrap the return value in a `Result.Ok` or `Result.Err`.
            return `(function(){
                    let res = ${value}; 
                    return res.constructor.name === "klar_Error" ? 
                        new klar_Result_Err(res): new klar_Result_Ok(res)})()`
        }
        return value
    }
    /**
     * When `assign_last_expression_to` is set, the last expression of the block
     * is assigned to the given variable. If the last expression affects the control flow
     * (i.e. return, break, continue), an error with the expression is thrown.
     */
    function transpile_block(
        block: AST.Block,
        ctx: Context,
        opts?: {assign_last_expression_to?: string; return_last_expression?: boolean},
    ) {
        let body = block.body
            .slice(0, -1)
            .map((x) => transpile_expression(x, ctx))
            .join("\n")
        const last_expression_code =
            block.body.length === 0 ? "undefined" : transpile_expression(block.body.at(-1)!, ctx)
        if (opts?.assign_last_expression_to) {
            if (block.body.length === 0) {
                return `{${opts.assign_last_expression_to} = undefined;)`
            }
            if (
                block.body.at(-1) instanceof AST.Return ||
                block.body.at(-1) instanceof AST.Break ||
                block.body.at(-1) instanceof AST.Continue
            ) {
                return `{${body};throw new Error("${last_expression_code}")}`
            }
            return `{${body};${opts.assign_last_expression_to} = ${last_expression_code};}`
        }
        if (
            opts?.return_last_expression &&
            (ctx.func?.return_type !== AST.unit_type || ctx.func?.throws) &&
            !(
                block.body.at(-1) instanceof AST.Return ||
                block.body.at(-1) instanceof AST.Break ||
                block.body.at(-1) instanceof AST.Continue
            )
        ) {
            body = `{${body};return ${wrap_return_value(last_expression_code, ctx)};}`
        } else {
            body = `{${body};${last_expression_code}}`
        }
        return `{try {${body} } catch (e) { if (e.message === "return") {return e.value;} throw e;}}`
    }
    function transpile_impls(e: AST.StructDeclaration | AST.EnumDeclaration) {
        let s = ""
        function transpile_impl_function(impl: AST.ImplDefinition, fn: AST.FunctionDefinition) {
            const is_static = fn.declaration.parameters[0]?.name !== "self"
            const parameters = fn.declaration.parameters
                .filter((x) => x.name !== "self")
                .map((x) => x.name)
                .join(",")
            const block = transpile_block(
                fn.block,
                {func: fn.declaration},
                {return_last_expression: true},
            ).replace("{", "{const klar_self = this;")
            const prefix = is_static ? `${impl.target_name}.` : `${impl.target_name}.prototype.`
            s += `\n${prefix}${fn.declaration.name} = function(${parameters})${block}`
        }
        for (const impl of e.attributes.impls) {
            for (const fn of impl.functions) {
                if (fn instanceof AST.FunctionDefinition) {
                    transpile_impl_function(impl, fn)
                }
            }
            if (impl.trait_name) {
                const trait = impl.attributes.trait_declaration
                if (!trait) {
                    throw new CodeGenError(`Trait ${quote(impl.trait_name)} not found`, e.span)
                }
                for (const fn of trait.functions.filter(
                    (x) => x instanceof AST.FunctionDefinition,
                ) as AST.FunctionDefinition[]) {
                    if (impl.functions.find((x) => x.name === fn.name)) {
                        continue
                    }
                    transpile_impl_function(impl, fn)
                }
            }
        }
        return s
    }
    function transpile_match(e: AST.Match, ctx: Context) {
        let s = "(function() { let __match_result;"
        s += `let __match_expression = ${transpile_expression(e.value, ctx)};`
        for (const arm of e.arms) {
            s += declare_captured_variables(arm.pattern)
        }
        for (const [index, arm] of e.arms.entries()) {
            s += `if (${transpile_match_pattern_to_condition("__match_expression", arm.pattern)}) `
            s += transpile_block(arm.block, ctx, {assign_last_expression_to: "__match_result"})
            if (index < e.arms.length - 1) {
                s += " else "
            }
        }
        s += `return __match_result;})()\n`
        return s
    }
    function declare_captured_variables(pattern: AST.MatchPattern) {
        if (pattern instanceof AST.CaptureMatchPatternOrType) {
            return `let ${pattern.name};`
        } else if (pattern instanceof AST.StructuredMatchPattern) {
            const {fields} = pattern
            let s = ""
            for (const field_pattern of Object.values(fields)) {
                s += declare_captured_variables(field_pattern)
            }
            return s
        } else if (pattern instanceof AST.TupleMatchPattern) {
            const {values} = pattern
            let s = ""
            for (const value of values) {
                s += declare_captured_variables(value)
            }
            return s
        } else {
            return ""
        }
    }
    function transpile_match_pattern_to_condition(
        match_expression: string,
        pattern: AST.MatchPattern,
    ) {
        if (pattern instanceof AST.LiteralMatchPattern) {
            return `(${match_expression}.value === ${pattern.value})`
        } else if (pattern instanceof AST.WildcardMatchPattern) {
            return "true"
        } else if (pattern instanceof AST.CaptureMatchPatternOrType) {
            return `(${pattern.name} = ${match_expression})`
        } else if (
            pattern instanceof AST.StructuredMatchPattern ||
            pattern instanceof AST.TupleMatchPattern
        ) {
            const {type_expression} = pattern
            let target_name
            if (type_expression instanceof AST.IdentifierReference) {
                target_name = type_expression.name
            } else if (
                type_expression instanceof AST.FieldAccess &&
                type_expression.target instanceof AST.IdentifierReference &&
                type_expression.target.attributes.type?.declaration instanceof AST.EnumDeclaration
            ) {
                const declaration = type_expression.target.attributes.type!.declaration
                target_name = `${declaration.name}_${type_expression.field}`
            }
            let s = `(${match_expression}.constructor.name == "${target_name}")`
            if (pattern instanceof AST.TupleMatchPattern) {
                const values = pattern.values.map((x, i) =>
                    transpile_match_pattern_to_condition(`${match_expression}[${i}]`, x),
                )
                s += ` && ${values.join(" && ")}`
                return s
            }
            const {fields} = pattern
            if (Object.keys(fields).length === 0) {
                return s
            } else {
                s += " && "
                let is_first = true
                for (const [field_name, field_pattern] of Object.entries(fields)) {
                    if (is_first) {
                        is_first = false
                    } else {
                        s += " && "
                    }
                    const field_condition = transpile_match_pattern_to_condition(
                        `${match_expression}.${field_name}`,
                        field_pattern,
                    )
                    s += field_condition
                }
                return s
            }
        } else {
            throw new CodeGenError(`Unexpected pattern ${quote(pattern.kind)}`, pattern.span)
        }
    }
    return res
}

export async function compile({
    file,
    src,
    disable_debug,
    env,
}: {
    file: string
    src: string
    env: TypeEnvironment
    disable_debug?: boolean
}) {
    const log_prefix = `[${file.split("/").pop()}]`
    const tokens = Lexer.lexer({file: file.split("/").pop() ?? file, src})
    if (debug_tokens && !disable_debug) {
        console.log(`\n${log_prefix} TOKENS`)
        console.log(to_json(tokens, 2))
    }
    let ast = AST.parse(new AST.TokenStream(tokens))
    type_check_ast(ast, env)
    if (debug_ast && !disable_debug) {
        console.log(`\n${log_prefix} AST:`)
        console.log(to_json(ast, 2))
    }
    let transpiled = code_gen(ast)
    if (prettify) {
        try {
            const proc = Bun.spawn(["prettier", "--stdin-filepath", "transpiled.js"], {
                stdin: "pipe",
            })
            proc.stdin.write(transpiled)
            proc.stdin.end()
            transpiled = await new Response(proc.stdout).text()
        } catch (error: any) {
            console.error(`${log_prefix} Error running prettier: ${error.message}`)
        }
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
export async function link({
    compiled,
    prelude,
    epilogue,
}: {
    compiled: string
    prelude: string
    epilogue: string
}) {
    return (await Bun.file(`${dir}/prelude_js.js`).text()) + prelude + compiled + epilogue
}

export async function compile_prelude(
    file: string,
    env?: TypeEnvironment,
): Promise<{env: TypeEnvironment; prelude: string}> {
    env = env || TypeEnvironment.global()
    const src = await Bun.file(file).text()
    const prelude = await compile({file, src, env, disable_debug: true})
    return {env, prelude}
}

/**
 * Main entry point.
 */
async function cli() {
    try {
        const {env, prelude} = await compile_prelude(`${dir}/prelude_js.kl`)
        const src_file = Bun.argv[2]
        const dst_file = Bun.argv[3]
        let src: string
        try {
            src = await Bun.file(src_file).text()
        } catch (error: any) {
            console.error(`Error reading file: ${error.message}`)
            process.exit(1)
        }
        const compiled = await compile({file: src_file, src, env})
        const res = await link({
            compiled,
            prelude,
            epilogue: await Bun.file(`${dir}/epilogue_js.js`).text(),
        })
        await Bun.write(dst_file, res)
        const proc = Bun.spawn(["chmod", "+x", dst_file])
        await proc.exited
    } catch (error: any) {
        if (debug || debug_ast || debug_tokens || debug_transpiled) {
            throw error
        }
        console.error(`Compile error: ${error.message}`)
        process.exit(1)
    }
}

if (Bun.argv[1].endsWith("compiler.ts")) {
    cli()
}
