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
    if (typeof value === "number") {
        console.log(value)
        return
    }
    console.log(value.klar_to_str().value)
}

// For compiler debugging purposes only.
const klar_jsprint = console.log

let klar_panic = (value, location, src) => {
    throw new Error(
        `Panic: ${to_debug_str(value)} at ${to_debug_str(location)}:\n  ${to_debug_str(src)}
        \nStack trace:\n    ${klar_stack_trace().klar_to_str().value}`,
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
    return new klar_Str(this.toString())
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
    return new klar_Str(this.toString())
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

class klar_Str {
    constructor(value) {
        this.value = value
    }

    klar_to_str() {
        return this
    }

    klar_eq(other) {
        return this.value === other.value
    }

    klar_ne(other) {
        return this.value !== other.value
    }

    klar_push(other) {
        this.value += other.value
        return this
    }

    klar_push_char(other) {
        this.value += other.value
        return this
    }

    klar_replace(from, to) {
        return new klar_Str(this.value.replaceAll(from.value, to.value))
    }

    klar_split(sep) {
        const parts = this.value.split(sep.value)
        const arr = klar_Vector.klar_new(parts.length)
        for (const s of parts) {
            arr.klar_push(new klar_Str(s))
        }
        return arr.klar_iter()
    }

    klar_starts_with(prefix) {
        return this.value.startsWith(prefix.value)
    }

    klar_ends_with(suffix) {
        return this.value.endsWith(suffix.value)
    }

    klar_contains(sub) {
        return this.value.includes(sub.value)
    }

    klar_contains_char(sub) {
        return this.value.includes(sub.value)
    }

    klar_trim_start() {
        return new klar_Str(this.value.trimStart())
    }

    klar_trim_end() {
        return new klar_Str(this.value.trimEnd())
    }

    klar_trim() {
        return new klar_Str(this.value.trim())
    }

    klar_reverse() {
        return new klar_Str(this.value.split("").reverse().join(""))
    }

    klar_slice_copy(start, end) {
        return new klar_Str(this.value.slice(start, end))
    }

    klar_len() {
        return this.value.length
    }

    klar_join(iter) {
        let result = ""
        let first = true
        let item = iter.klar_next()
        while (item.klar_is_some()) {
            if (first) {
                first = false
            } else {
                result += this.value
            }
            result += item.klar_unwrap().value
            item = iter.klar_next()
        }
        return new klar_Str(result)
    }

    klar_get(index) {
        if (index > this.klar_len() || index < 0) {
            klar_panic(
                `index out of bound in \`Str[${index}]\`, Str has length \`${this.klar_len()}`,
            )
        }
        return new klar_Char(this.value[index])
    }
}

class klar_Char {
    constructor(value) {
        this.value = value
    }

    klar_to_str() {
        return new klar_Str(this.value)
    }

    klar_eq(other) {
        return this.value === other.value
    }

    klar_ne(other) {
        return this.value !== other.value
    }

    static klar_from_int(value) {
        if (value < 0 || value > 65535) {
            return new klar_Result_Error(new klar_Str(`cannot convert ${value} to Char`))
        }
        return new klar_Result_Ok(new klar_Char(String.fromCharCode(value)))
    }

    klar_to_int() {
        return this.value.charCodeAt(0)
    }
}

class klar_StrBuilder {
    constructor(value) {
        this.value = value ? "" + value : ""
    }

    static klar_new() {
        return new klar_StrBuilder()
    }

    static klar_from_str(value) {
        return new klar_StrBuilder(value.value)
    }

    static klar_from_char(value) {
        return new klar_StrBuilder(value.value)
    }

    klar_push(value) {
        this.value += value.value
    }

    klar_push_char(value) {
        this.value += value.value
    }

    klar_len() {
        return this.value.length
    }

    klar_to_str() {
        return new klar_Str(this.value)
    }

    klar_clear() {
        this.value = ""
    }

    klar_iter() {
        return this.klar_to_str().klar_iter()
    }

    klar_get(index) {
        return new klar_Char(this.value[index])
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
        return new klar_Str(this.path)
    }

    klar_read_str() {
        const fs = require("fs")
        try {
            const res = new klar_Str(fs.readFileSync(this.path.value, "utf8"))
            return new klar_Result_Ok(res)
        } catch (e) {
            return new klar_Result_Error(new klar_Str(e.message))
        }
    }

    klar_write_str(value) {
        const fs = require("fs")
        try {
            fs.writeFileSync(this.path.value, value.value)
            return new klar_Result_Ok()
        } catch (e) {
            return new klar_Result_Error(new klar_Str(e.message))
        }
    }
}

function klar_ext_args() {
    const result = new klar_JSArray(0)
    for (let i = 0; i < Bun.argv.length; i++) {
        result.klar_push(new klar_Str(Bun.argv[i]))
    }
    return result
}

let klar_int_counter = 0
function klar_int_next() {
    return klar_int_counter++
}

// Used in `compiler.ts` only.
function to_debug_str(s) {
    if (s?.value !== undefined) {
        s = s.value
    }
    if (typeof s === "number" || typeof s === "boolean") {
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
