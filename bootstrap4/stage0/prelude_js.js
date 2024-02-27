#!/usr/bin/env bun --silent

function klar_print(value) {
    console.log(value.klar_to_str().value)
}

// For compiler debugging purposes only.
const klar_jsprint = console.log

let klar_panic = (value, location, src) => {
    throw new Error(
        `Panic: ${to_debug_str(value)} at ${to_debug_str(location)}:\n  ${to_debug_str(src)}`,
    )
}

function klar_register_panic_handler(f) {
    const old_panic = klar_panic
    klar_panic = f
    return old_panic
}

function klar_exit(code) {
    process.exit(code.value)
}

class klar_unit {}

class klar_Int {
    static klar_from_str(value) {
        let int
        try {
            int = parseInt(value.value)
        } catch (e) {
            return new klar_Result_Error(new klar_Str(e.message))
        }
        if (isNaN(int) || int < -2147483648 || int > 2147483647) {
            return new klar_Result_Error(new klar_Str("integer out of range"))
        }
        return new klar_Result_Ok(new klar_Int(parseInt(value.value)))
    }

    constructor(value) {
        this.value = value
    }

    klar_to_str() {
        return new klar_Str(this.value.toString())
    }

    klar_eq(other) {
        return new klar_Bool(this.value === other.value)
    }

    klar_ne(other) {
        return new klar_Bool(this.value !== other.value)
    }

    klar_lt(other) {
        return new klar_Bool(this.value < other.value)
    }

    klar_le(other) {
        return new klar_Bool(this.value <= other.value)
    }

    klar_gt(other) {
        return new klar_Bool(this.value > other.value)
    }

    klar_ge(other) {
        return new klar_Bool(this.value >= other.value)
    }

    klar_add(other) {
        return new klar_Int(this.value + other.value)
    }

    klar_sub(other) {
        return new klar_Int(this.value - other.value)
    }

    klar_mul(other) {
        return new klar_Int(this.value * other.value)
    }

    klar_div(other) {
        return new klar_Int(Math.ceil(this.value / other.value))
    }
}

class klar_Bool {
    constructor(value) {
        this.value = value
    }

    klar_to_str() {
        return new klar_Str(this.value.toString())
    }

    klar_eq(other) {
        return new klar_Bool(this.value === other.value)
    }

    klar_ne(other) {
        return new klar_Bool(this.value !== other.value)
    }

    klar_lt(other) {
        return new klar_Bool(this.value < other.value)
    }

    klar_le(other) {
        return new klar_Bool(this.value <= other.value)
    }

    klar_gt(other) {
        return new klar_Bool(this.value > other.value)
    }

    klar_ge(other) {
        return new klar_Bool(this.value >= other.value)
    }
}

class klar_Str {
    constructor(value) {
        this.value = value
    }

    klar_to_str() {
        return this
    }

    klar_eq(other) {
        return new klar_Bool(this.value === other.value)
    }

    klar_ne(other) {
        return new klar_Bool(this.value !== other.value)
    }

    klar_push(other) {
        this.value += other.value
        return this
    }

    klar_push_char(other) {
        this.value += other.value
        return this
    }

    klar_slice_copy(start, end) {
        return new klar_Str(this.value.slice(start.value, end.value))
    }

    klar_len() {
        return new klar_Int(this.value.length)
    }

    klar_join(iter) {
        let result = ""
        let first = true
        let item = iter.klar_next()
        while (item.klar_is_some().value) {
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
        if (index.value > this.klar_len().value || index.value < 0) {
            panic(
                `index out of bound in \`Str[${index.value}]\`, Str has length \`${
                    this.klar_len().value
                }`,
            )
        }
        return new klar_Char(this.value[index.value])
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
        return new klar_Bool(this.value === other.value)
    }

    klar_ne(other) {
        return new klar_Bool(this.value !== other.value)
    }

    static klar_from_int(value) {
        if (value.value < 0 || value.value > 65535) {
            return new klar_Result_Error(new klar_Str(`cannot convert ${value.value} to Char`))
        }
        return new klar_Char(String.fromCharCode(value.value))
    }

    klar_to_int() {
        return new klar_Int(this.value.charCodeAt(0))
    }
}

class klar_JSArray extends Array {
    static klar_new(size) {
        return new klar_JSArray(size.value)
    }

    klar_len() {
        return new klar_Int(this.length)
    }

    klar_get(index) {
        return this[index.value]
    }

    klar_set(index, value) {
        this[index.value] = value
    }

    klar_push(value) {
        this.push(value)
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

function klar_int_next() {
    return new klar_Int(klar_Int.counter++)
}
klar_Int.counter = 0

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
        for (const [Char, replacement] of Object.entries({
            "\n": "\\n",
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
