#!/usr/bin/env bun --silent

function klar_print(value) {
    console.log(value.to_str().value)
}

// For compiler debugging purposes only.
const klar_jsprint = console.log

function klar_panic(value) {
    throw new Error(value.to_str().value)
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

    push(other) {
        this.value += other.value
        return this
    }

    len() {
        return new usize(this.value.length)
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
