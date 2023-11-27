/**
 * Transpile Klar to JavaScript.
 */
import assert from "assert"
import {quote, Span, to_json} from "./common"
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

function traverse_ast(ast: AST.AST, f: (e: AST.ASTNode, parent: AST.ASTNode) => void) {
    const visited: any[] = []
    for (const expression of ast.body) {
        traverse(expression, ast.body)
    }
    function traverse(obj: any, parent: any) {
        if (obj === undefined) {
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

/**
 * TODO: Implement these checks
 * - all trait functions are implemented with the correct signature
 * - an enum variant cannot be used in a binary expression
 * - matches are exhaustive
 * - match arms are exhaustive (i.e. all fields of a struct/enum are matched)
 */
function semantic_analysis(ast: AST.AST) {
    traverse_ast(ast, (e) => {
        check_mutability(e)
        check_field_access(e)
        check_all_trait_functions_are_implemented(e)
    })
    function check_mutability(expression: AST.Expression) {
        if (expression instanceof AST.Assignment) {
            // fixme: we first have to resolve all types.
        }
    }
    function check_field_access(e: AST.ASTNode) {
        if (!(e instanceof AST.FieldAccess)) {
            return
        }
        if (e.target instanceof AST.IdentifierReference) {
            if (e.target.resolved instanceof AST.StructDeclaration) {
                const struct = e.target.resolved
                if (
                    !struct.members[e.field] &&
                    !struct.impls.find(
                        (impl) =>
                            impl.member_functions.some((fn) => fn.declaration.name === e.field) ||
                            impl.static_functions.some((fn) => fn.declaration.name === e.field),
                    )
                ) {
                    throw new SemanticAnalysisError(
                        `Struct ${quote(e.target.name)} has no field or function ${quote(e.field)}`,
                        e.span,
                    )
                }
            }
        }
    }
    function check_all_trait_functions_are_implemented(impl: AST.ASTNode) {
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
                    impl.span,
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
                    impl.span,
                )
            }
        }
    }
    return ast
}

function mangle_names(ast: AST.AST) {
    function mangle(name: string) {
        if (name.startsWith("klar_")) {
            return name
        }
        return `klar_${name}`
    }
    traverse_ast(ast, (e: any, parent: any) => {
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
        } else if (e instanceof AST.Parameter) {
            if (e.name !== "self") {
                e.name = mangle(e.name)
            }
        } else if (e instanceof AST.CaptureMatchPattern) {
            e.name = mangle(e.name)
        }
    })
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
        res += transpile_expression(expression)
        res += "\n"
    }
    function transpile_expression(e: AST.Expression): string {
        if (e instanceof AST.FunctionDefinition) {
            const parameters = e.declaration.parameters.map((x) => x.name).join(",")
            const block = transpile_block(e.block)
            // fixme: Use a try / catch to be able to break out of contained a match
            // expression with a `return` statement.
            return `function ${e.declaration.name}(${parameters})${block}`
        } else if (e instanceof AST.ClosureDefinition) {
            const parameters = e.parameters.map((x) => x.name).join(",")
            const block = transpile_block(e.block)
            // fixme: Use a try / catch to be able to break out of contained a match
            // expression with a `return` statement.
            return `function (${parameters})${block}`
        } else if (e instanceof AST.Return) {
            if (!e.value) {
                return "return;"
            }
            return `return ${transpile_expression(e.value)};`
        } else if (e instanceof AST.Number_) {
            return `new i32(${e.value})`
        } else if (e instanceof AST.Bool || e instanceof AST.Number_) {
            return `new bool(${e.value})`
        } else if (e instanceof AST.String_) {
            let value = e.value.replace(/\\/g, "\\\\")
            value = value.replace(/\n/g, "\\n")
            value = value.replace(/\t/g, "\\t")
            value = value.replace(/"/g, "\\`")
            if (e.is_multiline) {
                value = value.replace(/`/g, "\\`")
                return `new str(\`${value}\`)`
            }
            return `new str("${value}")`
        } else if (e instanceof AST.InterpolatedString) {
            const parts = e.expressions.map((x) => `_.push(${transpile_expression(x)}.to_str());`)
            return `(function () { const _ = new str(""); ${parts.join("")}; return _; })()`
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
            const value = transpile_expression(e.value)
            if (e.target instanceof AST.IndexedAccess) {
                return `${transpile_expression(e.target.target)}.set(${transpile_expression(
                    e.target.index,
                )}, ${value});`
            }
            const target = transpile_expression(e.target)
            return `${target} = ${value};`
        } else if (e instanceof AST.ArrayLiteral) {
            const values = e.values.map((x) => `_.push(${transpile_expression(x)});`).join("")
            return `(function () { const _ = klar_Vector.new(); ${values} return _; })()`
        } else if (e instanceof AST.Block) {
            return transpile_block(e)
        } else if (e instanceof AST.If) {
            const condition = transpile_expression(e.condition)
            const then = transpile_block(e.then_block)
            let s = `if (${condition}.value) ${then}`
            if (e.else_block) {
                const else_ = transpile_block(e.else_block)
                s += ` else ${else_}`
            }
            return s
        } else if (e instanceof AST.StructDeclaration) {
            const members = Object.keys(e.members).join(";")
            const impls = transpile_impl_definitions(e)
            return `class ${e.name} {${members}\n${impls}}`
        } else if (e instanceof AST.EnumDeclaration) {
            let variants = ""
            for (const variant of e.variants) {
                const variant_class_name = `${e.name}_${variant.name}`
                const constructor =
                    "constructor(...values) {super();values.forEach((x, i) => {this[i] = x;});}"
                variants += `class ${variant_class_name} extends ${e.name} {\n${constructor}};`
            }
            const impls = transpile_impl_definitions(e)
            const base_class = `class ${e.name} {${impls}}`
            return `${base_class}${variants}`
        } else if (e instanceof AST.Loop) {
            const block = transpile_block(e.block)
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
            const lhs = transpile_expression(e.lhs)
            const rhs = transpile_expression(e.rhs)
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
                e.target.resolved instanceof AST.EnumDeclaration
            ) {
                assert(typeof e.field === "string")
                const variant = e.target.resolved.get_variant(e.field)
                assert(variant, `Enum ${e.target.resolved.name} has no variant ${e.field}`)
                if (variant.values.members.length === 0) {
                    return `new ${e.target.resolved.name}_${e.field}()`
                }
                return `new ${e.target.resolved.name}_${e.field}`
            }
            const target = transpile_expression(e.target)
            if (parseInt(e.field).toString() === e.field) {
                return `${target}[${e.field}]`
            }
            return `${target}.${e.field}`
        } else if (e instanceof AST.IndexedAccess) {
            const target = transpile_expression(e.target)
            const index = transpile_expression(e.index)
            return `${target}.get(${index})`
        } else if (e instanceof AST.Not) {
            const value = transpile_expression(e.value)
            return `!${value}`
        } else if (e instanceof AST.ParenthesizedExpression) {
            const value = transpile_expression(e.expression)
            return `(${value})`
        } else if (e instanceof AST.TupleInstantiation) {
            const values = e.values.map(transpile_expression).join(",")
            return `[${values}]`
        } else if (e instanceof AST.IdentifierReference) {
            return e.resolved!.name
        } else if (e instanceof AST.ImplDefinition) {
            return ""
        } else if (e instanceof AST.TraitDeclaration) {
            return ""
        } else if (e instanceof AST.ExternBlock) {
            return ""
        } else if (e instanceof AST.Match) {
            return transpile_match(e)
        } else {
            throw new CodeGenError(`Unexpected expression ${quote(e.kind)}`, e.span)
        }
    }
    /**
     * When `assign_last_expression_to` is set, the last expression of the block
     * is assigned to the given variable. If the last expression affects the control flow
     * (i.e. return, break, continue), an error with the expression is thrown.
     */
    function transpile_block(block: AST.Block, assign_last_expression_to?: string) {
        if (assign_last_expression_to) {
            if (block.body.length === 0) {
                return `{${assign_last_expression_to} = undefined;)`
            }
            const last_expression = block.body.at(-1)!
            const body = block.body.slice(0, -1).map(transpile_expression).join("\n")
            const last_expression_code = transpile_expression(last_expression)
            if (
                block.body.at(-1) instanceof AST.Return ||
                block.body.at(-1) instanceof AST.Break ||
                block.body.at(-1) instanceof AST.Continue
            ) {
                return `{${body};throw new Error("${last_expression_code}")}`
            }
            return `{${body};${assign_last_expression_to} = ${last_expression_code};}`
        }
        const body = block.body.map(transpile_expression).join("\n")
        return `{${body}}`
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
            if (impl.resolved_trait) {
                for (const fn of impl.resolved_trait.member_function_default_impls) {
                    if (
                        impl.member_functions.find(
                            (x) => x.declaration.name === fn.declaration.name,
                        )
                    ) {
                        continue
                    }
                    transpile_impl_function(fn, false)
                }
                for (const fn of impl.resolved_trait.static_function_default_impls) {
                    if (
                        impl.static_functions.find(
                            (x) => x.declaration.name === fn.declaration.name,
                        )
                    ) {
                        continue
                    }
                    transpile_impl_function(fn, false)
                }
            }
        }
        return impls
    }
    function transpile_match(e: AST.Match) {
        let s = "(function() { let __match_result;"
        s += `let __match_expression = ${transpile_expression(e.value)};`
        for (const arm of e.arms) {
            s += declare_captured_variables(arm.pattern)
        }
        for (const [index, arm] of e.arms.entries()) {
            s += `if (${transpile_match_pattern_to_condition("__match_expression", arm.pattern)}) `
            s += transpile_block(arm.block, "__match_result")
            if (index < e.arms.length - 1) {
                s += " else "
            }
        }
        if (e.wildcard_block) {
            if (e.arms.length > 0) {
                s += " else "
            }
            s += transpile_block(e.wildcard_block, "__match_result")
        }
        s += `return __match_result;})()\n`
        return s
    }
    function declare_captured_variables(pattern: AST.MatchPattern) {
        if (pattern instanceof AST.CaptureMatchPattern) {
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
        } else if (pattern instanceof AST.CaptureMatchPattern) {
            return `(${pattern.name} = ${match_expression})`
        } else if (
            pattern instanceof AST.StructuredMatchPattern ||
            pattern instanceof AST.TupleMatchPattern
        ) {
            const {type_expression} = pattern
            let target_name
            if (type_expression instanceof AST.IdentifierReference) {
                target_name = type_expression.resolved.name
            } else if (
                type_expression instanceof AST.FieldAccess &&
                type_expression.target instanceof AST.IdentifierReference &&
                type_expression.target.resolved instanceof AST.EnumDeclaration
            ) {
                target_name = `${type_expression.target.resolved.name}_${type_expression.field}`
            }
            let s = `(${match_expression} instanceof ${target_name})`
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
    env: AST.Environment
    disable_debug?: boolean
}) {
    const log_prefix = `[${file.split("/").pop()}]`
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
    ast = semantic_analysis(ast)
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
    env?: AST.Environment,
): Promise<{env: AST.Environment; prelude: string}> {
    env = env || new AST.Environment()
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
        const file = Bun.argv[2]
        let src: string
        try {
            src = await Bun.file(file).text()
        } catch (error: any) {
            console.error(`Error reading file: ${error.message}`)
            process.exit(1)
        }
        const compiled = await compile({file, src, env})
        const res = await link({
            compiled,
            prelude,
            epilogue: await Bun.file(`${dir}/epilogue_js.js`).text(),
        })
        const dst = `${dir}/build/${(file.split("/")?.pop() ?? file).split(".")[0]}`
        await Bun.write(dst, res)
        const proc = Bun.spawn(["chmod", "+x", dst])
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
