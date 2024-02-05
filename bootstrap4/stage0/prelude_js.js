#!/usr/bin/env bun --silent

function klar_print(value) {
    console.log(value.to_str().value)
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
    static from_str(value) {
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

    to_str() {
        return new klar_Str(this.value.toString())
    }

    eq(other) {
        return new klar_Bool(this.value === other.value)
    }

    ne(other) {
        return new klar_Bool(this.value !== other.value)
    }

    lt(other) {
        return new klar_Bool(this.value < other.value)
    }

    le(other) {
        return new klar_Bool(this.value <= other.value)
    }

    gt(other) {
        return new klar_Bool(this.value > other.value)
    }

    ge(other) {
        return new klar_Bool(this.value >= other.value)
    }

    add(other) {
        return new klar_Int(this.value + other.value)
    }

    sub(other) {
        return new klar_Int(this.value - other.value)
    }

    mul(other) {
        return new klar_Int(this.value * other.value)
    }

    div(other) {
        return new klar_Int(Math.ceil(this.value / other.value))
    }
}

class klar_Bool {
    constructor(value) {
        this.value = value
    }

    to_str() {
        return new klar_Str(this.value.toString())
    }

    eq(other) {
        return new klar_Bool(this.value === other.value)
    }

    ne(other) {
        return new klar_Bool(this.value !== other.value)
    }

    lt(other) {
        return new klar_Bool(this.value < other.value)
    }

    le(other) {
        return new klar_Bool(this.value <= other.value)
    }

    gt(other) {
        return new klar_Bool(this.value > other.value)
    }

    ge(other) {
        return new klar_Bool(this.value >= other.value)
    }
}

class klar_Str {
    constructor(value) {
        this.value = value
    }

    to_str() {
        return this
    }

    eq(other) {
        return new klar_Bool(this.value === other.value)
    }

    ne(other) {
        return new klar_Bool(this.value !== other.value)
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
        return new klar_Str(this.value.slice(start.value, end.value))
    }

    len() {
        return new klar_Int(this.value.length)
    }

    iter() {
        let idx = 0
        return {
            next: () => {
                if (idx < this.value.length) {
                    return new klar_Option_Some(new klar_Char(this.value[idx++]))
                } else {
                    return new klar_Option_None()
                }
            },
        }
    }

    join(iter) {
        let result = ""
        let first = true
        let item = iter.next()
        while (item.is_some().value) {
            if (first) {
                first = false
            } else {
                result += this.value
            }
            result += item.unwrap().value
            item = iter.next()
        }
        return new klar_Str(result)
    }

    get(index) {
        if (index.value > this.len().value || index.value < 0) {
            panic(
                `index out of bound in \`Str[${index.value}]\`, Str has length \`${
                    this.len().value
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

    to_str() {
        return new klar_Str(this.value)
    }

    eq(other) {
        return new klar_Bool(this.value === other.value)
    }

    ne(other) {
        return new klar_Bool(this.value !== other.value)
    }

    static from_int(value) {
        if (value.value < 0 || value.value > 65535) {
            return new klar_Result_Error(new klar_Str(`cannot convert ${value.value} to Char`))
        }
        return new klar_Char(String.fromCharCode(value.value))
    }

    to_int() {
        return new klar_Int(this.value.charCodeAt(0))
    }
}

class klar_JSArray extends Array {
    static new(size) {
        return new klar_JSArray(size.value)
    }

    len() {
        return new klar_Int(this.length)
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
        return new klar_Str(this.path)
    }

    read_str() {
        const fs = require("fs")
        try {
            const res = new klar_Str(fs.readFileSync(this.path.value, "utf8"))
            return new klar_Result_Ok(res)
        } catch (e) {
            return new klar_Result_Error(new klar_Str(e.message))
        }
    }

    write_str(value) {
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
        result.push(new klar_Str(Bun.argv[i]))
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
