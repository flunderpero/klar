import {HasKindAndSpan, quote, Span} from "./common"

export class Token extends HasKindAndSpan {
    kind = "token"
    constructor(span: Span) {
        super(span)
    }
}

export class LexicalToken extends Token {
    kind = "lexical"
    constructor(
        public value:
            | "("
            | ")"
            | "{"
            | "}"
            | "<"
            | ">"
            | "."
            | ","
            | ":"
            | "_"
            | "=>"
            | "="
            | "=="
            | "!="
            | ">"
            | "<"
            | ">="
            | "<="
            | "and"
            | "or"
            | "not"
            | "fn"
            | "end"
            | "return"
            | "match"
            | "+"
            | "-"
            | "*"
            | "/"
            | "let"
            | "mut"
            | "if"
            | "else"
            | "loop"
            | "break"
            | "continue"
            | "struct"
            | "enum"
            | "trait"
            | "for"
            | "extern"
            | "impl"
            | "true"
            | "false",
        span: Span,
    ) {
        super(span)
    }

    toString() {
        return this.value
    }
}

export class NumberToken extends Token {
    kind = "number"
    constructor(
        public value: string,
        span: Span,
    ) {
        super(span)
    }

    toString() {
        return `${this.value} (number)`
    }
}

export class Identifier extends Token {
    kind = "identifier"
    constructor(
        public value: string,
        span: Span,
    ) {
        super(span)
    }

    toString() {
        return `${this.value} (identifier)`
    }
}

export class LexerError extends Error {
    constructor(
        public error: string,
        public span: Span,
    ) {
        super(`${error} at ${span}`)
    }
}

/**
 * Lex Klar source code into tokens.
 */
export function lexer({file, src}: {file: string; src: string}): Token[] {
    const tokens: Token[] = []
    let i = 0
    function span(relative_to?: Span): Span {
        if (relative_to) {
            return new Span(relative_to.start, i, file, src)
        }
        return new Span(i, i, file, src)
    }
    function peek(ahead = 0) {
        return src[i + ahead]
    }
    function consume() {
        return src[i++]
    }
    function skip(num = 1) {
        i += num
    }
    while (i < src.length) {
        const c = peek()
        if (c === "-" && peek(1) === "-" && peek(2) === "-") {
            skip(3)
            while (i < src.length - 1 && !(peek() === "-" && peek(1) === "-" && peek(2) === "-")) {
                skip()
            }
            skip(3)
        } else if (c === "-" && peek(1) === "-") {
            skip(2)
            while (i < src.length - 1 && peek() !== "\n") {
                skip()
            }
        } else if (c === "=" && peek(1) === "=") {
            tokens.push(new LexicalToken("==", span()))
            skip(2)
        } else if (c === "!" && peek(1) === "=") {
            tokens.push(new LexicalToken("!=", span()))
            skip(2)
        } else if (c === "<" && peek(1) === "=") {
            tokens.push(new LexicalToken("<=", span()))
            skip(2)
        } else if (c === ">" && peek(1) === "=") {
            tokens.push(new LexicalToken(">=", span()))
            skip(2)
        } else if (c === "=" && peek(1) === ">") {
            tokens.push(new LexicalToken("=>", span()))
            skip(2)
        } else if (
            [
                "(",
                ")",
                "{",
                "}",
                "<",
                ">",
                ":",
                ".",
                ",",
                "=",
                "+",
                "-",
                "*",
                "/",
                ">",
                "<",
                "_",
            ].includes(c)
        ) {
            tokens.push(new LexicalToken(c as any, span()))
            skip()
        } else if (c.match(/\s/)) {
            skip()
        } else if (c.match(/[a-zA-Z_]/)) {
            let value = ""
            let start_span = span()
            while (i < src.length && peek().match(/[a-zA-Z0-9_]/)) {
                value += consume()
            }
            if (
                [
                    "return",
                    "fn",
                    "end",
                    "let",
                    "mut",
                    "if",
                    "else",
                    "loop",
                    "break",
                    "continue",
                    "struct",
                    "enum",
                    "match",
                    "trait",
                    "for",
                    "extern",
                    "impl",
                    "true",
                    "false",
                    "and",
                    "or",
                    "not",
                ].includes(value)
            ) {
                tokens.push(new LexicalToken(value as any, span(start_span)))
            } else {
                tokens.push(new Identifier(value, span(start_span)))
            }
        } else if (c.match(/[0-9]/)) {
            let value = ""
            const start_span = span()
            while (peek().match(/[0-9]/)) {
                value += consume()
            }
            tokens.push(new NumberToken(value, span(start_span)))
        } else {
            throw new LexerError(`Unexpected character ${quote(c)}`, span())
        }
    }
    return tokens
}
