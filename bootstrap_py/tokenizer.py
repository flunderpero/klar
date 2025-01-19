from dataclasses import dataclass
from enum import Enum

from . import error
from .span import Span

Value = str | None


class Kind(Enum):
    comma = ","
    comment = "--"
    curly_left = "{"
    curly_right = "}"
    eof = "eof"
    fn = "fn"
    ident = "ident"
    minus = "-"
    paren_left = "("
    paren_right = ")"
    str_lit = "str_lit"


@dataclass
class Token:
    kind: Kind
    span: Span
    value: Value

    def __str__(self) -> str:
        value = ""
        match self.value:
            case str():
                value = self.value
            case None:
                value = ""
            case _:
                raise AssertionError(f"Unexpected value: {self.value}")
        return f"{self.span}: [{self.kind.name}] {value}"


keywords = {x.name: x for x in [Kind.fn]}


@dataclass
class Input:
    file: str
    src: str
    index: int = 0

    def next(self) -> str:
        if self.index > len(self.src):
            return ""
        c = self.src[self.index]
        self.index += 1
        return c

    def peek(self) -> str:
        return "" if self.index == len(self.src) else self.src[self.index]

    def has_next(self) -> bool:
        return self.index < len(self.src)

    def span(self) -> Span:
        return Span(self.file, self.src, self.index, self.index)


def tokenize(input: Input) -> tuple[list[Token], list[error.Error]]:
    tokens: list[Token] = []
    errors: list[error.Error] = []
    while input.has_next():
        span = input.span()
        c = input.next()
        kind: Kind
        value: Value = None
        match c:
            case "":
                kind = Kind.eof
            case "\n" | "\r" | "\t" | " ":
                continue
            case "(":
                kind = Kind.paren_left
            case ")":
                kind = Kind.paren_right
            case "{":
                kind = Kind.curly_left
            case "}":
                kind = Kind.curly_right
            case ",":
                kind = Kind.comma
            case "-":
                # Comment
                if input.peek() == "-":
                    input.next()
                    kind = Kind.comment
                    value = "--"
                    while (c := input.peek()) not in ("", "\n"):
                        input.next()
                        value += c
                else:
                    kind = Kind.minus
            case '"':
                # String
                value = ""
                while (c := input.peek()) not in ("", '"', "\n"):
                    input.next()
                    value += c
                if (c := input.peek()) in ("\n", ""):
                    errors.append(error.unterminated_str_lit(span.merge(input.span()), eof=c == ""))
                    continue
                input.next()
                kind = Kind.str_lit
            case c if c.islower() and c.isascii():
                # Identifier
                kind = Kind.ident
                value = c
                while (c := input.peek()).isalnum() and c.isascii():
                    input.next()
                    value += c
                keyword = keywords.get(value)
                if keyword:
                    kind = keyword
                    value = ""
            case _:
                errors.append(error.unknown_token(span, c))
                continue
        tokens.append(Token(kind, span.merge(input.span()), value))
    tokens.append(Token(Kind.eof, input.span(), ""))
    return tokens, errors
