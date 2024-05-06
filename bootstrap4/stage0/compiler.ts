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
const debug_errors = Bun.argv.includes("--debug-errors") || debug
const profile = Bun.argv.includes("--profile")

class CodeGenError extends Error {
    constructor(
        public error: string,
        public span: Span,
    ) {
        super(`${error} at ${span}`)
    }
}

/**
 * Transpile AST to JavaScript.
 */
function code_gen(ast: AST.AST, opts?: {encapsulate?: boolean}) {
    function m(name: string) {
        if (name.startsWith("klar_")) {
            return name
        }
        return `klar_${name}`
    }
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
        "%": "mod",
    }
    let res = ""
    if (opts?.encapsulate) {
        res += "{"
    }
    for (const expression of ast.body) {
        res += transpile_expression(expression, {used_in_expression: false})
        res += "\n"
    }
    if (opts?.encapsulate) {
        res += "}"
    }
    type Context = {
        func?: AST.FunctionDeclaration | AST.ClosureDefinition
        used_in_expression: boolean
    }
    function transpile_expression(e: AST.Expression, ctx: Context): string {
        if (e instanceof AST.UnitOperator) {
            return transpile_expression(e.expression, {...ctx, used_in_expression: false})
        } else if (e instanceof AST.FunctionDefinition) {
            const parameters = e.declaration.parameters.map((x) => m(x.name)).join(",")
            const block = transpile_block(
                e.block,
                {...ctx, func: e.declaration, used_in_expression: false},
                {return_last_expression: true},
            )
            return `function ${m(e.declaration.name)}(${parameters}) {
                try {
                    ${block}
                } catch (e) {
                    if (e.message === "return") {
                        return e.value
                    }
                    throw e
                }};exported.${m(e.declaration.name)} = ${m(e.declaration.name)};`
        } else if (e instanceof AST.ClosureDefinition) {
            const parameters = e.parameters.map((x) => m(x.name)).join(",")
            const block = transpile_block(
                e.block,
                {...ctx, func: e, used_in_expression: false},
                {return_last_expression: true},
            )
            return `function (${parameters}) { 
                try {
                    ${block}
                } catch (e) { 
                    if (e.message === "return") {return e.value;} 
                    throw e;
                }}`
        } else if (e instanceof AST.Return) {
            if (!e.value) {
                return "throw new Error('return')"
            }
            const value = wrap_return_value(
                transpile_expression(e.value, {...ctx, used_in_expression: true}),
                ctx,
                e.span,
            )
            return `;(function () { const error = new Error("return"); error.value = ${value}; throw error; })()`
        } else if (e instanceof AST.Number_ || e instanceof AST.Bool) {
            return `${e.value}`
        } else if (e instanceof AST.Str) {
            const value = escape_str(e)
            return `${value}`
        } else if (e instanceof AST.Char) {
            const value = escape_str(e)
            return `${value}`
        } else if (e instanceof AST.InterpolatedStr) {
            const parts = e.expressions.map((x) => {
                const target = transpile_expression(x, {...ctx, used_in_expression: true})
                if (
                    x.attributes.type?.name === "Int" ||
                    x.attributes.type?.name === "Char" ||
                    x.attributes.type?.name === "Bool" ||
                    x.attributes.type?.name === "String"
                ) {
                    return `_.klar_push("" + ${target});`
                }
                return `_.klar_push(${target}.klar_to_str());`
            })
            return `(function () { const _ = klar_StrBuilder.klar_new(); ${parts.join(
                "",
            )}; return _.klar_to_str(); })()`
        } else if (e instanceof AST.FunctionCall) {
            if (e.target instanceof AST.IdentifierReference && e.target.name === "assert") {
                return `
                    call_frames.push(${span_to_frame(e.span)});
                    try {
                        ${transpile_assert(e)};
                    } finally {
                        call_frames.pop();
                    }`
            }
            if (e.target instanceof AST.IdentifierReference && e.target.name === "panic") {
                convert_call_to_panic(e)
            }
            const args = e.args.map((x, i) => {
                let s = transpile_expression(x, {...ctx, used_in_expression: true})
                const p = (e.target.attributes?.type as any)?.parameters_without_self?.[i]
                if (p?.name === "Option" && x.attributes.type!.name !== "Option") {
                    s = `new klar_Option_Some(${s})`
                }
                return s
            })
            let call
            if (e.target instanceof AST.FunctionDefinition) {
                call = `${m(e.target.declaration.name)}(${args.join(",")})\n`
            } else {
                const target = transpile_expression(e.target, {...ctx, used_in_expression: true})
                call = `${target}(${args.join(",")})\n`
            }
            if (e.propagate_error) {
                call = `(function() {
                    call_frames.push(${span_to_frame(e.span)});
                    let res;
                    try {
                        res = ${call};
                    } finally {
                        call_frames.pop();
                    }
                    if (res.constructor.name == "klar_Result_Error") {
                        const error = new Error("return")
                        error.value = res;
                        res.__error_return_trace_frames.push(${span_to_frame(e.span)});
                        throw error;
                    }
                    if (res.constructor.name == "klar_Result_Ok") return res[0];
                    return res})()`
                if (ctx.used_in_expression) {
                    return call
                }
                return `${call};`
            } else {
                call = `(function () {
                        call_frames.push(${span_to_frame(e.span)});
                        try {
                            return ${call};
                        } finally {
                            call_frames.pop();
                        }
                    })()`
                if (ctx.used_in_expression) {
                    return call
                }
                return `${call};`
            }
        } else if (e instanceof AST.StructInstantiation) {
            const create_instance = `let _ = new ${m(e.target_struct_name)}()`
            const assign_members = Object.entries(e.fields).map(
                ([name, value]) =>
                    `_.${m(name)} = ${
                        value
                            ? transpile_expression(value, {
                                  ...ctx,
                                  used_in_expression: true,
                              })
                            : m(name)
                    };`,
            )
            return `(() => {${create_instance};${assign_members.join("\n")} return _})()`
        } else if (e instanceof AST.VariableDeclaration) {
            const kind = e.mutable ? "let" : "const"
            if (e.value) {
                const value = transpile_expression(e.value, {...ctx, used_in_expression: true})
                return `${kind} ${m(e.name)} = ${value};`
            } else {
                return `${kind} ${m(e.name)};`
            }
        } else if (e instanceof AST.Assignment) {
            const value = transpile_expression(e.value, {...ctx, used_in_expression: true})
            if (e.target instanceof AST.IndexedAccess) {
                return `${transpile_expression(e.target.target, {
                    ...ctx,
                    used_in_expression: true,
                })}.klar_set(${transpile_expression(e.target.index, {
                    ...ctx,
                    used_in_expression: true,
                })}, ${value});`
            }
            const target = transpile_expression(e.target, {...ctx, used_in_expression: true})
            return `${target} = ${value};`
        } else if (e instanceof AST.ArrayLiteral) {
            const values = e.elements
                .map(
                    (x) =>
                        `_.klar_push(${transpile_expression(x, {
                            ...ctx,
                            used_in_expression: true,
                        })});`,
                )
                .join("")
            return `(function () { const _ = klar_Vector.klar_new(); ${values} return _; })()`
        } else if (e instanceof AST.Block) {
            return transpile_block(e, ctx)
        } else if (e instanceof AST.If) {
            const condition = transpile_expression(e.condition, {...ctx, used_in_expression: true})
            const then = transpile_block(
                e.then_block,
                {...ctx, used_in_expression: true},
                {assign_last_expression_to: "__if_result"},
            )
            let s = `(${condition}) ? (function() {let __if_result;${then} return __if_result})()`
            if (e.else_block) {
                const else_ = transpile_block(
                    e.else_block,
                    {...ctx, used_in_expression: true},
                    {assign_last_expression_to: "__if_result"},
                )
                s += ` : (function() {let if_result;${else_} return __if_result})()`
            } else {
                s += ": undefined"
            }
            if (ctx.used_in_expression) {
                return s
            } else {
                return `;${s};`
            }
        } else if (e instanceof AST.IfLet) {
            let s = "(function() {let __if_result;"
            const declared_variables = new Set<string>()
            s += declare_captured_variables(e.pattern, declared_variables)
            const condition = transpile_match_pattern_to_condition(
                transpile_expression(e.value, {...ctx, used_in_expression: true}),
                e.pattern,
            )
            const then = transpile_block(
                e.then_block,
                {...ctx, used_in_expression: true},
                {assign_last_expression_to: "__if_result"},
            )
            s += `if (${condition}) ${then}`
            if (e.else_block) {
                const else_ = transpile_block(
                    e.else_block,
                    {...ctx, used_in_expression: true},
                    {assign_last_expression_to: "__if_result"},
                )
                s += `else ${else_}`
            }
            return `${s} return __if_result})();`
        } else if (e instanceof AST.StructDeclaration) {
            const members = Object.keys(e.fields).map(m).join(";")
            return `class ${m(e.name)} {${members}\n};exported.${m(e.name)} = ${m(e.name)};`
        } else if (e instanceof AST.EnumDeclaration) {
            let variants = ""
            let assign = ""
            for (const variant of e.variants) {
                const variant_class_name = `${m(e.name)}_${variant.name}`
                let constructor =
                    "constructor(...values) {super();values.forEach((x, i) => {this[i] = x;});"
                if (e.name === "Result") {
                    constructor += "this.__error_return_trace_frames = [];"
                }
                constructor += "}"
                variants += `class ${variant_class_name} extends ${m(e.name)} {\n${constructor}};`
                if (variant.fields.fields.length === 0) {
                    assign += `${m(e.name)}.${m(variant.name)} = new ${variant_class_name}();`
                } else {
                    assign += `${m(e.name)}.${m(
                        variant.name,
                    )} = (...args) => new ${variant_class_name}(...args);`
                }
            }
            let base_class = `class ${m(e.name)} {}`
            if (e.name === "Result") {
                base_class += `
                    ;klar_Result.prototype.klar_error_return_trace = function () {
                        const frames = klar_Array.klar_new(this.__error_return_trace_frames.length);
                        for (let i = 0; i < this.__error_return_trace_frames.length; i++) {
                            const f = new klar_Frame();
                            for (const [k, v] of Object.entries(this.__error_return_trace_frames[i])) {
                                f[k] = v;
                            }
                            frames.klar_data[i] = f;
                        }
                        const res = new klar_Trace({klar_frames: frames})
                        res.klar_frames = frames;
                        return res;
                    };
                    `
            }
            return `${base_class}${variants}${assign};exported.${m(e.name)} = ${m(e.name)};`
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
            return "throw new Error('break')"
        } else if (e instanceof AST.Continue) {
            return "throw new Error('continue')"
        } else if (e instanceof AST.BinaryExpression) {
            const lhs = transpile_expression(e.lhs, {...ctx, used_in_expression: true})
            const rhs = transpile_expression(e.rhs, {...ctx, used_in_expression: true})
            if (e.operator === "and") {
                return `(${lhs} && ${rhs})`
            } else if (e.operator === "or") {
                return `(${lhs} || ${rhs})`
            }
            if (
                e.lhs.attributes.type?.name === "Int" ||
                e.rhs.attributes.type?.name === "Int" ||
                e.lhs.attributes.type?.name === "Str" ||
                e.rhs.attributes.type?.name === "Str" ||
                e.lhs.attributes.type?.name === "Char" ||
                e.rhs.attributes.type?.name === "Char" ||
                e.lhs.attributes.type?.name === "Bool" ||
                e.rhs.attributes.type?.name === "Bool"
            ) {
                if (e.operator === "/") {
                    return `Math.floor(${lhs} / ${rhs})`
                }
                return `(${lhs} ${e.operator} ${rhs})`
            }
            const function_name = binary_op_functions[e.operator]
            if (!function_name) {
                throw new CodeGenError(`Unexpected operator ${quote(e.operator)}`, e.span)
            }
            return `(${lhs}.${m(function_name)}(${rhs}))`
        } else if (e instanceof AST.FieldAccess) {
            const target = transpile_expression(e.target, {...ctx, used_in_expression: true})
            if (parseInt(e.field).toString() === e.field) {
                return `${target}[${e.field}]`
            }
            return `${target}.${m(e.field)}`
        } else if (e instanceof AST.FQN) {
            return e.parts
                .map((x) => transpile_expression(x, {...ctx, used_in_expression: true}))
                .join(".")
        } else if (e instanceof AST.IndexedAccess) {
            const target = transpile_expression(e.target, {...ctx, used_in_expression: true})
            const index = transpile_expression(e.index, {...ctx, used_in_expression: true})
            return `${target}.klar_get(${index})`
        } else if (e instanceof AST.Not) {
            const value = transpile_expression(e.expression, {...ctx, used_in_expression: true})
            return `(!${value})`
        } else if (e instanceof AST.ParenthesizedExpression) {
            const value = transpile_expression(e.expression, ctx)
            return `(${value})`
        } else if (e instanceof AST.TupleInstantiation) {
            const values = e.elements
                .map((x) => transpile_expression(x, {...ctx, used_in_expression: true}))
                .join(",")
            return `[${values}]`
        } else if (e instanceof AST.IdentifierReference) {
            return m(e.name)
        } else if (e instanceof AST.ImplDefinition) {
            return transpile_impl(e)
        } else if (e instanceof AST.TraitDeclaration) {
            return ""
        } else if (e instanceof AST.ExternBlock) {
            return ""
        } else if (e instanceof AST.Match) {
            return transpile_match(e, ctx)
        } else if (e instanceof AST.UnitLiteral) {
            return "new klar_unit()"
        } else if (e instanceof AST.Use) {
            let s = ""
            if (e.attributes.enum_variants) {
                const prefix = e.path[0] === "." ? "exported." : ""
                for (const variant of e.attributes.enum_variants) {
                    s += `const ${m(variant.name)} = ${prefix}${m(e.path[0])}.${m(variant.name)};`
                }
            } else {
                s += `const ${m(e.path.at(-1)!)} = exported.${m(e.path.at(-1)!)};`
            }
            return s
        } else {
            throw new CodeGenError(`Unexpected expression ${quote(e.kind)}`, e.span)
        }
    }
    function span_to_frame(span: Span) {
        const src_line = span.src_lines.split("\n")[0].trim()
        return `{
            klar_file: \`${escape_str_str(span.file)}\`,
            klar_line: ${span.pos.line},
            klar_col: ${span.pos.col},
            klar_src: \`${escape_str_str(src_line)}\`
        }`
    }
    /**
     * Convert calls `assert()` so that in the case of an error,
     * the lhs and rhs are printed as well as the source location.
     */
    function transpile_assert(call: AST.FunctionCall) {
        const e = call.args[0]
        const location = escape_str_str(e.span.toString())
        if (e instanceof AST.BinaryExpression) {
            const lhs = transpile_expression(e.lhs, {used_in_expression: true})
            const rhs = transpile_expression(e.rhs, {used_in_expression: true})
            let cond
            if (e.operator === "and") {
                cond = `(lhs && rhs)`
            } else if (e.operator === "or") {
                cond = `(lhs || rhs)`
            } else {
                if (
                    e.lhs.attributes.type?.name === "Int" ||
                    e.rhs.attributes.type?.name === "Int" ||
                    e.lhs.attributes.type?.name === "Char" ||
                    e.rhs.attributes.type?.name === "Char" ||
                    e.lhs.attributes.type?.name === "Str" ||
                    e.rhs.attributes.type?.name === "Str" ||
                    e.lhs.attributes.type?.name === "Bool" ||
                    e.rhs.attributes.type?.name === "Bool"
                ) {
                    cond = `(lhs ${e.operator} rhs)`
                } else {
                    const function_name = binary_op_functions[e.operator]
                    cond = `(lhs.${m(function_name)}(rhs))`
                }
            }
            return `(function() {
const lhs = ${lhs};
const rhs = ${rhs};
if (!${cond}) {
let lhs_str = to_debug_str(lhs);
let rhs_str = to_debug_str(rhs);
for (let i = 0; i < lhs_str.length && i < rhs_str.length; i++) {
    if (lhs_str[i] !== rhs_str[i]) {
        lhs_str = lhs_str.slice(0, i) + "\x1b[31m[" + lhs_str[i] + "]\x1b[0m" + lhs_str.slice(i + 1);
        rhs_str = rhs_str.slice(0, i) + "\x1b[31m[" + rhs_str[i] + "]\x1b[0m" + rhs_str.slice(i + 1);
        break;
    }
}
klar_panic(
\`Assertion failed:

expected: ${escape_str_str(e.lhs.span.src_text)} ${e.operator} ${escape_str_str(
                e.rhs.span.src_text,
            )}
got:      \${lhs_str} ${e.operator} \${rhs_str}

\`, "${location}", "");}})();`
        } else if (e instanceof AST.Not) {
            const inner = transpile_expression(e.expression, {used_in_expression: true})
            return `(function() {
let cond = ${inner};
if (!!(cond)) {
const inner = to_debug_str(cond);
klar_panic(
\`Assertion failed:

expected: not ${escape_str_str(e.expression.span.src_text)}
got:      \${cond}
\`, "${location}", "");}})();`
        }
        const cond = transpile_expression(e, {used_in_expression: true})
        return `(function() {
if (!(${cond})) {
    klar_panic(\`Assertion failed:\\n\`, 
    "${location}", "");
}})();`
    }
    /**
     * Add additional parameters to `panic()` so that the source and location
     * can be printed.
     */
    function convert_call_to_panic(e: AST.FunctionCall) {
        e.args.push(new AST.Str({value: e.span.toString(), is_multiline: false}, e.span))
        e.args.push(
            new AST.Str(
                {value: e.span.src_text, is_multiline: e.span.src_text.includes("\n")},
                e.span,
            ),
        )
    }
    function wrap_return_value(value: string, ctx: Context, span: Span) {
        const throws = ctx.func?.throws
        const is_option = ctx.func?.return_type.name === "Option"
        if (throws || is_option) {
            // Wrap the return value in a `Result` or `Option` constructor.
            return `(function(){
                    let res = ${value}; 
                    if (res === undefined) {
                        res = new klar_unit();
                    }
                    if (${is_option}) {
                        if (${!!throws}) {
                            if (res.constructor.name === "klar_Result_Error") {
                                res.__error_return_trace_frames.push(${span_to_frame(span)});
                                return res;
                            }
                        }
                        if (res.constructor.name !== "klar_Option_Some" 
                            && res.constructor.name !== "klar_Option_None") {
                            res = new klar_Option_Some(res);
                        }
                    }
                    if (${!!throws}) {
                        if (!res.constructor.name.startsWith("klar_Result_")) {
                            res = new klar_Result_Ok(res)
                        } else if (res.constructor.name === "klar_Result_Error") {
                            res.__error_return_trace_frames.push(${span_to_frame(span)});
                        }
                    }
                    return res})()`
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
            .map((x) => transpile_expression(x, {...ctx, used_in_expression: false}))
            .join("\n")
        const last_expression_code =
            block.body.length === 0
                ? "new klar_unit()"
                : transpile_expression(block.body.at(-1)!, {...ctx, used_in_expression: true})
        if (opts?.assign_last_expression_to) {
            if (block.body.length === 0) {
                return `{${opts.assign_last_expression_to} = new klar_unit();}`
            }
            if (
                block.body.at(-1) instanceof AST.Loop ||
                block.body.at(-1) instanceof AST.Return ||
                block.body.at(-1) instanceof AST.Break ||
                block.body.at(-1) instanceof AST.Continue
            ) {
                return `{${body};${last_expression_code}}`
            }
            return `{${body};${opts.assign_last_expression_to} = ${last_expression_code};}`
        }
        if (
            opts?.return_last_expression &&
            (ctx.func?.attributes?.type?.return_type.name !== "()" || ctx.func?.throws) &&
            !(
                block.body.at(-1) instanceof AST.Return ||
                block.body.at(-1) instanceof AST.Break ||
                block.body.at(-1) instanceof AST.Continue ||
                block.body.at(-1) instanceof AST.Loop
            )
        ) {
            body = `{${body};return ${wrap_return_value(
                last_expression_code,
                ctx,
                block.body.at(-1)?.span || block.span,
            )};}`
        } else {
            body = `{${body};${last_expression_code}}`
        }
        return `${body}`
    }
    function transpile_impl(impl: AST.ImplDefinition) {
        let s = ""
        function transpile_impl_function(impl: AST.ImplDefinition, fn: AST.FunctionDefinition) {
            const is_static = fn.declaration.parameters[0]?.name !== "self"
            const parameters = fn.declaration.parameters
                .filter((x) => x.name !== "self")
                .map((x) => m(x.name))
                .join(",")
            const block = transpile_block(
                fn.block,
                {func: fn.declaration, used_in_expression: false},
                {return_last_expression: true},
            )
            const prefix = is_static
                ? `${m(impl.target_name)}.`
                : `${m(impl.target_name)}.prototype.`
            s += `\n${prefix}${m(fn.declaration.name)} = function(${parameters}){
                    try {
                        ${block.replace("{", "{const klar_self = this;")}
                    } catch (e) {
                        if (e.message === "return") {
                            return e.value
                        }
                        throw e
                    }}`
        }
        for (const fn of impl.functions) {
            if (fn instanceof AST.FunctionDefinition) {
                transpile_impl_function(impl, fn)
            }
        }
        if (impl.trait_name) {
            const trait = impl.attributes.trait_declaration
            if (!trait) {
                throw new CodeGenError(`Trait ${quote(impl.trait_name)} not found`, impl.span)
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
        return s
    }
    function transpile_match(e: AST.Match, ctx: Context) {
        let s = "(function() { let __match_result;"
        s += `let __match_expression = ${transpile_expression(e.value, {
            ...ctx,
            used_in_expression: true,
        })};`
        const declared_variables = new Set<string>()
        for (const arm of e.arms) {
            s += declare_captured_variables(arm.pattern, declared_variables)
        }
        for (const [index, arm] of e.arms.entries()) {
            s += `if (${transpile_match_pattern_to_condition("__match_expression", arm.pattern)}) `
            s += transpile_block(arm.block, ctx, {assign_last_expression_to: "__match_result"})
            if (index < e.arms.length - 1) {
                s += " else "
            }
        }
        s += `return __match_result;})()\n`
        if (!ctx.used_in_expression) {
            s += ";"
        }
        return s
    }
    function declare_captured_variables(pattern: AST.MatchPattern, declared_variable: Set<string>) {
        if (pattern instanceof AST.CaptureMatchPatternOrType) {
            if (pattern.attributes.type?.constructor.name === "EnumType") {
                return ""
            }
            if (declared_variable.has(pattern.name)) {
                return ""
            }
            declared_variable.add(pattern.name)
            return `let ${m(pattern.name)};`
        } else if (pattern instanceof AST.StructuredMatchPattern) {
            const {fields} = pattern
            let s = ""
            for (const field_pattern of Object.values(fields)) {
                s += declare_captured_variables(field_pattern, declared_variable)
            }
            return s
        } else if (pattern instanceof AST.TupleMatchPattern) {
            const {values} = pattern
            let s = ""
            for (const value of values) {
                s += declare_captured_variables(value, declared_variable)
            }
            return s
        } else {
            return ""
        }
    }
    function escape_str(s: AST.Str | AST.Char) {
        let value = s.value.replace(/\\/g, "\\\\")
        for (const [Char, replacement] of Object.entries(Lexer.escape_sequences)) {
            value = value.replaceAll(Char, `\\${replacement}`)
        }
        value = value.replace(/`/g, "\\`")
        if (s instanceof AST.Str && s.is_multiline) {
            return `\`${value}\``
        }
        value = value.replace(/"/g, '\\"')
        return `"${value}"`
    }
    function escape_str_str(s: string) {
        let value = s.replace(/\\/g, "\\\\")
        for (const [Char, replacement] of Object.entries(Lexer.escape_sequences)) {
            value = value.replaceAll(Char, `\\${replacement}`)
        }
        value = value.replace(/`/g, "\\`")
        return value
    }
    function transpile_match_pattern_to_condition(
        match_expression: string,
        pattern: AST.MatchPattern,
    ) {
        if (pattern instanceof AST.LiteralMatchPattern) {
            let value = pattern.value.value
            if (pattern.value instanceof AST.Str || pattern.value instanceof AST.Char) {
                value = escape_str(pattern.value)
            }
            return `(${match_expression} === ${value})`
        } else if (pattern instanceof AST.WildcardMatchPattern) {
            return "true"
        } else if (pattern instanceof AST.AlternativeMatchPattern) {
            const conditions: string[] = pattern.patterns.map((x) =>
                transpile_match_pattern_to_condition(match_expression, x),
            )
            return `(${conditions.join(" || ")})`
        } else if (pattern instanceof AST.RangeMatchPattern) {
            const {start, end} = pattern
            const start_value = transpile_expression(start, {used_in_expression: true})
            const end_value = transpile_expression(end, {used_in_expression: true})
            const end_op = pattern.is_closed ? "<=" : "<"
            return `(${match_expression} >= ${start_value}
                && ${match_expression} ${end_op} ${end_value})`
        } else if (pattern instanceof AST.CaptureMatchPatternOrType) {
            if (pattern.attributes.type?.constructor.name === "EnumType") {
                const enum_type = pattern.attributes.type as any
                const constructor_name = `${m(enum_type.variant_parent.name)}_${
                    enum_type.variant_name
                }`
                return `(${match_expression}.constructor.name == "${constructor_name}")`
            }
            return `((${m(pattern.name)} = ${match_expression}) || true)`
        } else if (
            pattern instanceof AST.StructuredMatchPattern ||
            pattern instanceof AST.TupleMatchPattern
        ) {
            const {type_expression} = pattern
            let target_name
            if (type_expression instanceof AST.IdentifierReference) {
                if (type_expression.attributes.type?.constructor.name === "EnumType") {
                    const enum_type = type_expression.attributes.type as any
                    assert(enum_type.variant_name)
                    target_name = `${m(enum_type.name)}_${enum_type.variant_name}`
                } else {
                    target_name = m(type_expression.name)
                }
            } else if (type_expression instanceof AST.FQN) {
                if (type_expression.attributes.type?.constructor.name === "EnumType") {
                    assert((type_expression.attributes.type as any).is_variant)
                    target_name = `${m(type_expression.attributes.type.name)}_${
                        type_expression.parts.at(-1)!.name
                    }`
                } else {
                    target_name = transpile_expression(type_expression, {used_in_expression: true})
                }
            }
            let s = `(${match_expression}.constructor.name === "${target_name}")`
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
                        `${match_expression}.${m(field_name)}`,
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
    modules,
    encapsulate = true,
}: {
    file: string
    src: string
    env: TypeEnvironment
    disable_debug?: boolean
    modules: Map<string, TypeEnvironment>
    encapsulate?: boolean
}) {
    const log_prefix = `[${file.split("/").pop()}]`
    const tokens = Lexer.lexer({file: file.split("/").pop() ?? file, src})
    if (debug_tokens && !disable_debug) {
        console.log(`\n${log_prefix} TOKENS`)
        console.log(to_json(tokens, 2))
    }
    const t0 = Date.now()
    let ast = AST.parse(new AST.TokenStream(tokens))
    if (profile) {
        console.log(`[${file}] Parsing took ${Date.now() - t0}ms`)
    }
    let transpiled = ""
    for (const use of ast.body.filter(
        (x) => x instanceof AST.Use && x.path[0] === ".",
    ) as AST.Use[]) {
        const path = use.path.join("/")
        // Find the file to import.
        let cwd = file.split("/").slice(0, -1).join("/") || "."
        let path_to_import = path
        while (!(await Bun.file(`${cwd}/${path_to_import}.kl`).exists())) {
            if (path_to_import === "") {
                throw new Error(`No module found for ${quote(cwd + "/" + path)}`)
            }
            path_to_import = path_to_import.split("/").slice(0, -1).join("/")
        }

        let module_env = modules.get(path_to_import)
        if (!module_env) {
            module_env = new TypeEnvironment(env)
            transpiled += await compile({
                file: `${cwd}/${path_to_import}.kl`,
                src: await Bun.file(`${cwd}/${path_to_import}.kl`).text(),
                env: module_env,
                modules,
            })
            transpiled += "\n"
            modules.set(path_to_import, module_env)
        }
        if (path_to_import === path) {
            throw new Error(`As of now, modules cannot be imported as a whole.`)
        }
        const imported_type_name = path.split("/").pop()!
        env.import(imported_type_name, module_env.get(imported_type_name, use.span), use.span)
    }
    if (debug_ast && !disable_debug) {
        console.log(`\n${log_prefix} AST:`)
        console.log(to_json(ast, 2))
    }
    const t1 = Date.now()
    type_check_ast(ast, env)
    if (profile) {
        console.log(`[${file}] Type checking took ${Date.now() - t1}ms`)
    }
    const t2 = Date.now()
    transpiled += code_gen(ast, {encapsulate})
    if (profile) {
        console.log(`[${file}] Codegen took ${Date.now() - t2}ms`)
    }
    if (prettify) {
        const proc = Bun.spawn(["prettier", "--stdin-filepath", "transpiled.js"], {
            stdin: "pipe",
        })
        proc.stdin.write(transpiled)
        proc.stdin.end()
        transpiled = await new Response(proc.stdout).text()
        if ((await proc.exited) !== 0) {
            throw new Error(`Prettier failed with exit code ${proc.exitCode}`)
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
    const prelude = await compile({
        file,
        src,
        env,
        disable_debug: true,
        encapsulate: false,
        modules: new Map(),
    })
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
        const compiled = await compile({file: src_file, src, env, modules: new Map()})
        const res = await link({
            compiled,
            prelude,
            epilogue: await Bun.file(`${dir}/epilogue_js.js`).text(),
        })
        await Bun.write(dst_file, res)
        const proc = Bun.spawn(["chmod", "+x", dst_file])
        await proc.exited
    } catch (error: any) {
        if (debug || debug_ast || debug_tokens || debug_transpiled || debug_errors) {
            throw error
        }
        console.error(`Compile error: ${error.message}`)
        process.exit(1)
    }
}

if (Bun.argv[1].endsWith("compiler.ts")) {
    cli()
}
