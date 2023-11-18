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
            | "["
            | "]"
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

export class StringToken extends Token {
    kind = "string"

    constructor(
        public value: string,
        public is_multiline: boolean,
        span: Span,
    ) {
        super(span)
    }

    toString() {
        return `${this.value.replaceAll("\n", "\\n")} (string)`
    }
}

export class InterpolatedStringToken extends Token {
    kind = "interpolated_string"

    constructor(
        public parts: InterpolatedStringPart[],
        public is_multiline: boolean,
        span: Span,
    ) {
        super(span)
    }

    toString() {
        return `${this.parts.map((p) => `\`${p.toString()}\``).join(" + ")} (interpolated string)`
    }
}

export abstract class InterpolatedStringPart extends HasKindAndSpan {
    kind = "interpolated_part"
}

export class InterpolatedStringPartExpression extends InterpolatedStringPart {
    kind = "interpolated_string_expression"

    constructor(
        public tokens: Token[],
        span: Span,
    ) {
        super(span)
    }

    toString() {
        return `${this.tokens
            .map((t) => t.toString())
            .join(", ")
            .replaceAll("\n", "\\n")} (interpolated string expression)`
    }
}

export class InterpolatedStringPartLiteral extends InterpolatedStringPart {
    kind = "interpolated_string_literal"

    constructor(
        public value: string,
        public is_multiline: boolean,
        span: Span,
    ) {
        super(span)
    }

    toString() {
        return `${this.value} (interpolated string literal)`
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
        const token = parse_token()
        if (token) {
            tokens.push(token)
        }
    }
    function parse_token(): Token | undefined {
        const c = peek()
        if (c === "-" && peek(1) === "-" && peek(2) === "-") {
            skip(3)
            while (i < src.length - 1 && !(peek() === "-" && peek(1) === "-" && peek(2) === "-")) {
                skip()
            }
            skip(3)
            return undefined
        }
        if (c === "-" && peek(1) === "-") {
            skip(2)
            while (i < src.length - 1 && peek() !== "\n") {
                skip()
            }
            return undefined
        }
        if (c === "f" && peek(1) === '"' && peek(2) === '"' && peek(3) === '"') {
            const start_span = span()
            skip(4)
            const interpolations: InterpolatedStringPart[] = []
            let value = ""
            while (i < src.length && !(peek() === '"' && peek(1) === '"' && peek(2) === '"')) {
                const c = consume()
                if (c === "\\") {
                    value += parse_string_escape_sequence()
                } else if (c === "{") {
                    if (value) {
                        interpolations.push(
                            new InterpolatedStringPartLiteral(
                                value,
                                value.includes("\n"),
                                span(start_span),
                            ),
                        )
                        value = ""
                    }
                    interpolations.push(parse_interpolated_string_expression())
                } else {
                    value += c
                }
            }
            skip(3)
            if (i >= src.length) {
                throw new LexerError(`Unterminated string`, span(start_span))
            }
            if (value) {
                interpolations.push(
                    new InterpolatedStringPartLiteral(
                        value,
                        value.includes("\n"),
                        span(start_span),
                    ),
                )
            }
            return new InterpolatedStringToken(interpolations, true, span(start_span))
        }
        if (c === "f" && peek(1) === '"') {
            const start_span = span()
            skip(2)
            const interpolations: InterpolatedStringPart[] = []
            let value = ""
            while (i < src.length && peek() !== '"') {
                const c = consume()
                if (c === "\\") {
                    value += parse_string_escape_sequence()
                } else if (c === "\n") {
                    throw new LexerError(`Unexpected newline in string`, span())
                } else if (c === "{") {
                    if (value) {
                        interpolations.push(
                            new InterpolatedStringPartLiteral(value, false, span(start_span)),
                        )
                        value = ""
                    }
                    interpolations.push(parse_interpolated_string_expression())
                } else {
                    value += c
                }
            }
            skip()
            if (i >= src.length) {
                throw new LexerError(`Unterminated string`, span(start_span))
            }
            if (value) {
                interpolations.push(
                    new InterpolatedStringPartLiteral(value, false, span(start_span)),
                )
            }
            return new InterpolatedStringToken(interpolations, false, span(start_span))
        }
        if (c === '"' && peek(1) === '"' && peek(2) === '"') {
            const start_span = span()
            skip(3)
            let value = ""
            while (i < src.length && !(peek() === '"' && peek(1) === '"' && peek(2) === '"')) {
                const ch = consume()
                if (ch === "\\") {
                    value += parse_string_escape_sequence()
                } else {
                    value += ch
                }
            }
            skip(3)
            if (i >= src.length) {
                throw new LexerError(`Unterminated string`, span(start_span))
            }
            return new StringToken(value, true, span(start_span))
        }
        if (c === '"') {
            const start_span = span()
            skip()
            let value = ""
            while (i < src.length && peek() !== '"') {
                const ch = consume()
                if (ch === "\\") {
                    value += parse_string_escape_sequence()
                } else if (ch === "\n") {
                    throw new LexerError(`Unexpected newline in string`, span())
                } else {
                    value += ch
                }
            }
            skip()
            if (i >= src.length) {
                throw new LexerError(`Unterminated string`, span(start_span))
            }
            return new StringToken(value, false, span(start_span))
        }
        if (c === "=" && peek(1) === "=") {
            const token = new LexicalToken("==", span())
            skip(2)
            return token
        }
        if (c === "!" && peek(1) === "=") {
            const token = new LexicalToken("!=", span())
            skip(2)
            return token
        }
        if (c === "<" && peek(1) === "=") {
            const token = new LexicalToken("<=", span())
            skip(2)
            return token
        }
        if (c === ">" && peek(1) === "=") {
            const token = new LexicalToken(">=", span())
            skip(2)
            return token
        }
        if (c === "=" && peek(1) === ">") {
            const token = new LexicalToken("=>", span())
            skip(2)
            return token
        }
        if (
            [
                "(",
                ")",
                "[",
                "]",
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
            const token = new LexicalToken(c as any, span())
            skip()
            return token
        }
        if (c.match(/\s/)) {
            skip()
            return undefined
        }
        if (c.match(/[a-zA-Z_]/)) {
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
                return new LexicalToken(value as any, span(start_span))
            }
            return new Identifier(value, span(start_span))
        }
        if (c.match(/[0-9]/)) {
            let value = ""
            const start_span = span()
            while (peek().match(/[0-9]/)) {
                value += consume()
            }
            return new NumberToken(value, span(start_span))
        }
        throw new LexerError(`Unexpected character ${quote(c)}`, span())
    }
    function parse_string_escape_sequence() {
        const c = consume()
        if (c === "n") {
            return "\n"
        } else if (c === "t") {
            return "\t"
        } else if (c === "\\") {
            return "\\"
        } else if (c === '"') {
            return '"'
        } else {
            throw new LexerError(`Invalid escape sequence ${quote(c)}`, span())
        }
    }
    function parse_interpolated_string_expression(): InterpolatedStringPartExpression {
        const start_span = span()
        const tokens: Token[] = []
        while (i < src.length && peek() !== "}") {
            const token = parse_token()
            if (token) {
                tokens.push(token)
            }
        }
        skip()
        if (i >= src.length) {
            throw new LexerError(`Unterminated interpolated string expression`, span(start_span))
        }
        if (tokens.length === 0) {
            throw new LexerError(`Empty interpolated string expression`, span(start_span))
        }
        return new InterpolatedStringPartExpression(tokens, span(start_span))
    }
    return tokens
}
