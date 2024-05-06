#!/usr/bin/env bun --silent

const call_frames = []

function klar_stack_trace() {
    let st = call_frames.slice()
    st.pop()
    st.reverse()
    let frames = klar_Array.klar_new(st.length)
    for (let i = 0; i < st.length; i++) {
        const f = new klar_Frame()
        for (const [k, v] of Object.entries(st[i])) {
            f[k] = v
        }
        frames.klar_data[i] = f
    }
    const res = new klar_Trace({klar_frames: frames})
    res.klar_frames = frames
    return res
}

function klar_print(value) {
    if (typeof value === "number" || typeof value === "boolean" || typeof value === "string") {
        console.log(value)
        return
    }
    console.log(value.klar_to_str())
}

// For compiler debugging purposes only.
const klar_jsprint = console.log

let klar_panic = (value, location, src) => {
    throw new Error(
        `Panic: ${to_debug_str(value)} at ${to_debug_str(location)}:\n  ${to_debug_str(src)}
        \nStack trace:\n    ${klar_stack_trace().klar_to_str()}`,
    )
}

function klar_register_panic_handler(f) {
    const old_panic = klar_panic
    klar_panic = f
    return old_panic
}

function klar_exit(code) {
    process.exit(code)
}

class klar_unit {}

Number.prototype.klar_to_str = function () {
    return this.toString()
}

Number.prototype.klar_eq = function (other) {
    return this.valueOf() === other
}

Number.prototype.klar_ne = function (other) {
    return this.valueOf() !== other
}

Number.prototype.klar_lt = function (other) {
    return this.valueOf() < other
}

Number.prototype.klar_le = function (other) {
    return this.valueOf() <= other
}

Number.prototype.klar_gt = function (other) {
    return this.valueOf() > other
}

Number.prototype.klar_ge = function (other) {
    return this.valueOf() >= other
}

const klar_Int = Number

Boolean.prototype.klar_to_str = function () {
    return this.toString()
}

Boolean.prototype.klar_eq = function (other) {
    return this.valueOf() === other
}

Boolean.prototype.klar_ne = function (other) {
    return this.valueOf() !== other
}

Boolean.prototype.klar_lt = function (other) {
    return this.valueOf() < other
}

Boolean.prototype.klar_le = function (other) {
    return this.valueOf() <= other
}

Boolean.prototype.klar_gt = function (other) {
    return this.valueOf() > other
}

Boolean.prototype.klar_ge = function (other) {
    return this.valueOf() >= other
}

String.prototype.klar_to_str = function () {
    return this.valueOf()
}

String.prototype.klar_eq = function (other) {
    return this.valueOf() === other
}

String.prototype.klar_ne = function (other) {
    return this.valueOf() !== other
}

String.prototype.klar_lt = function (other) {
    return this.valueOf() < other
}

String.prototype.klar_le = function (other) {
    return this.valueOf() <= other
}

String.prototype.klar_gt = function (other) {
    return this.valueOf() > other
}

String.prototype.klar_ge = function (other) {
    return this.valueOf() >= other
}

String.prototype.klar_replace = function (from, to) {
    return this.replaceAll(from, to)
}

String.prototype.klar_split = function (sep) {
    const parts = this.split(sep)
    const arr = klar_Vector.klar_new(parts.length)
    for (const s of parts) {
        arr.klar_push(s)
    }
    return arr.klar_iter()
}

String.prototype.klar_starts_with = function (prefix) {
    return this.startsWith(prefix)
}

String.prototype.klar_ends_with = function (suffix) {
    return this.endsWith(suffix)
}

String.prototype.klar_contains = function (sub) {
    return this.includes(sub)
}

String.prototype.klar_contains_char = function (sub) {
    return this.includes(sub)
}

String.prototype.klar_trim_start = function () {
    return this.trimStart()
}

String.prototype.klar_trim_end = function () {
    return this.trimEnd()
}

String.prototype.klar_trim = function () {
    return this.trim()
}

String.prototype.klar_reverse = function () {
    return this.split("").reverse().join("")
}

String.prototype.klar_slice_copy = function (start, end) {
    return this.slice(start, end)
}

String.prototype.klar_len = function () {
    return this.length
}

String.prototype.klar_join = function (iter) {
    let result = ""
    let first = true
    let item = iter.klar_next()
    while (item.klar_is_some()) {
        if (first) {
            first = false
        } else {
            result += this
        }
        result += item.klar_unwrap()
        item = iter.klar_next()
    }
    return result
}

String.prototype.klar_get = function (index) {
    if (index > this.klar_len() || index < 0) {
        klar_panic(`index out of bound in \`Str[${index}]\`, Str has length \`${this.klar_len()}`)
    }
    return this[index]
}

const klar_Str = String

const klar_Char = String

String.klar_from_int = function (value) {
    if (value < 0 || value > 65535) {
        return new klar_Result_Error(`cannot convert ${value} to Char`)
    }
    return new klar_Result_Ok(String.fromCharCode(value))
}

String.prototype.klar_to_int = function () {
    return this.charCodeAt(0)
}

class klar_StrBuilder {
    constructor(value) {
        this.value = value ? "" + value : ""
    }

    static klar_new() {
        return new klar_StrBuilder()
    }

    static klar_from_str(value) {
        return new klar_StrBuilder(value)
    }

    static klar_from_char(value) {
        return new klar_StrBuilder(value)
    }

    klar_push(value) {
        this.value += value
    }

    klar_push_char(value) {
        this.value += value
    }

    klar_len() {
        return this.value.length
    }

    klar_to_str() {
        return this.value
    }

    klar_clear() {
        this.value = ""
    }

    klar_iter() {
        return this.klar_to_str().klar_iter()
    }

    klar_get(index) {
        return this.value[index]
    }
}

class klar_JSArray extends Array {
    static klar_new(size) {
        return new klar_JSArray(size)
    }

    klar_len() {
        return this.length
    }

    klar_get(index) {
        return this[index]
    }

    klar_set(index, value) {
        this[index] = value
    }

    klar_push(value) {
        this.push(value)
    }

    klar_pop() {
        this.pop()
    }
}

class klar_File {
    static klar_at(path) {
        return new klar_File(path)
    }

    constructor(path) {
        this.path = path
    }

    klar_to_str() {
        return this.path
    }

    klar_read_str() {
        const fs = require("fs")
        try {
            const res = fs.readFileSync(this.path, "utf8")
            return new klar_Result_Ok(res)
        } catch (e) {
            return new klar_Result_Error(e.message)
        }
    }

    klar_write_str(value) {
        const fs = require("fs")
        try {
            fs.writeFileSync(this.path, value)
            return new klar_Result_Ok()
        } catch (e) {
            return new klar_Result_Error(e.message)
        }
    }
}

function klar_ext_args() {
    const result = new klar_JSArray(0)
    for (let i = 0; i < Bun.argv.length; i++) {
        result.klar_push(Bun.argv[i])
    }
    return result
}

let klar_int_counter = 0
function klar_int_next() {
    return klar_int_counter++
}

// Used in `compiler.ts` only.
function to_debug_str(s) {
    if (typeof s === "number" || typeof s === "boolean" || typeof s === "string") {
        return s
    }
    if (typeof s !== "string") {
        s = JSON.stringify(s, null, 2)
    } else {
        for (const [Char, replacement] of Object.entries({
            "\r": "\\r",
            "\t": "\\t",
            "\0": "\\0",
            '"': '\\"',
            "`": "\\`",
        })) {
            s = s.replaceAll(Char, replacement)
        }
    }
    return `"${s}"`
}

// All experted functions and classes.
const exported = {}
