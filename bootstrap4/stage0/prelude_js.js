#!/usr/bin/env bun --silent

function klar_print(value) {
    console.log(value.to_str().value)
}

// For compiler debugging purposes only.
const klar_jsprint = console.log

function klar_panic(value, location, src) {
    throw new Error(
        `Panic: ${to_debug_str(value)} at ${to_debug_str(location)}:\n  ${to_debug_str(src)}`,
    )
}

function klar_exit(code) {
    process.exit(code.value)
}

class i32 {
    constructor(value) {
        this.value = value
    }

    to_str() {
        return new str(this.value.toString())
    }

    eq(other) {
        return new bool(this.value === other.value)
    }

    ne(other) {
        return new bool(this.value !== other.value)
    }

    lt(other) {
        return new bool(this.value < other.value)
    }

    le(other) {
        return new bool(this.value <= other.value)
    }

    gt(other) {
        return new bool(this.value > other.value)
    }

    ge(other) {
        return new bool(this.value >= other.value)
    }

    add(other) {
        return new i32(this.value + other.value)
    }

    sub(other) {
        return new i32(this.value - other.value)
    }

    mul(other) {
        return new i32(this.value * other.value)
    }

    div(other) {
        return new i32(Math.ceil(this.value / other.value))
    }
}

const usize = i32

class bool {
    constructor(value) {
        this.value = value
    }

    to_str() {
        return new str(this.value.toString())
    }

    eq(other) {
        return new bool(this.value === other.value)
    }

    ne(other) {
        return new bool(this.value !== other.value)
    }

    lt(other) {
        return new bool(this.value < other.value)
    }

    le(other) {
        return new bool(this.value <= other.value)
    }

    gt(other) {
        return new bool(this.value > other.value)
    }

    ge(other) {
        return new bool(this.value >= other.value)
    }
}

class str {
    constructor(value) {
        this.value = value
    }

    to_str() {
        return this
    }

    eq(other) {
        return new bool(this.value === other.value)
    }

    ne(other) {
        return new bool(this.value !== other.value)
    }

    push(other) {
        this.value += other.value
        return this
    }

    push_char(other) {
        this.value += other.value
        return this
    }

    slice_copy(start, end) {
        return new str(this.value.slice(start.value, end.value))
    }

    len() {
        return new usize(this.value.length)
    }

    iter() {
        let idx = 0
        return {
            next: () => {
                if (idx < this.value.length) {
                    return new klar_Option_Some(new char(this.value[idx++]))
                } else {
                    return new klar_Option_None()
                }
            },
        }
    }
}

class char {
    constructor(value) {
        this.value = value
    }

    to_str() {
        return new str(this.value)
    }

    eq(other) {
        return new bool(this.value === other.value)
    }

    ne(other) {
        return new bool(this.value !== other.value)
    }
}

class klar_JSArray extends Array {
    static new(size) {
        return new klar_JSArray(size.value)
    }

    len() {
        return new i32(this.length)
    }

    get(index) {
        return this[index.value]
    }

    set(index, value) {
        this[index.value] = value
    }
}

class klar_File {
    static at(path) {
        return new klar_File(path)
    }

    constructor(path) {
        this.path = path
    }

    to_str() {
        return new str(this.path)
    }

    read_str() {
        const fs = require("fs")
        try {
            const res = new str(fs.readFileSync(this.path.value, "utf8"))
            return new klar_Result_Ok(res)
        } catch (e) {
            return new klar_Result_Err(new str(e.message))
        }
    }

    write_str(value) {
        const fs = require("fs")
        try {
            fs.writeFileSync(this.path.value, value.value)
            return new klar_Result_Ok()
        } catch (e) {
            return new klar_Result_Err(new str(e.message))
        }
    }
}

function klar_ext_args() {
    const result = new klar_JSArray(0)
    for (let i = 0; i < Bun.argv.length; i++) {
        result.push(new str(Bun.argv[i]))
    }
    return result
}

// Used in `compiler.ts` only.
function to_debug_str(s) {
    if (s.value !== undefined) {
        s = s.value
    }
    if (typeof s === "number" || typeof s === "boolean") {
        return s
    }
    if (typeof s !== "string") {
        s = JSON.stringify(s, null, 2)
    } else {
        for (const [char, replacement] of Object.entries({
            "\n": "\\n",
            "\r": "\\r",
            "\t": "\\t",
            "\v": "\\v",
            "\b": "\\b",
            "\f": "\\f",
            "\0": "\\0",
            '"': '\\"',
            "`": "\\`",
        })) {
            s = s.replaceAll(char, replacement)
        }
    }
    return `"${s}"`
}
