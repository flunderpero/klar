""" This is our second attempt at a compiler for our language. 
    This time we first build up a compiler for a larger subset of our language
    and then use this compiler to compile itself. The mistake we made in our
    first attempt wast that we tried to bootstrap our compiler from a very
    small subset of our language. 

    Anyway, this compiler will guide the implementation of the next compiler 
    and shows how we can build up a compiler in a step by step fashion.

    We won't implement any syntax or type checking. Our goal is still to get
    to a self hosting compiler as fast as possible.

    Usage:
        python3 compiler.py [--debug] [--stdout] <input_file> <output_file>
"""
# pylint: disable-all
from dataclasses import dataclass
from enum import Enum, StrEnum, auto
import typing as t
import sys
from pprint import pprint
import subprocess
import os


class TokenKind(Enum):
    whitespace = auto()
    dot = auto()
    colon = auto()
    comma = auto()
    lparen = auto()
    rparen = auto()
    lbracket = auto()
    rbracket = auto()
    lbrace = auto()
    rbrace = auto()
    langle_bracket = auto()
    rangle_bracket = auto()
    equal = auto()
    double_equal = auto()
    plus = auto()
    minus = auto()
    star = auto()
    slash = auto()
    percent = auto()
    ampersand = auto()
    single_quote = auto()
    double_quote = auto()
    keyword = auto()
    identifier = auto()
    string = auto()
    format_string = auto()
    integer = auto()
    float_ = auto()
    bool_ = auto()
    comment = auto()
    exclamation = auto()


class Keyword(StrEnum):
    let = "let"
    mut = "mut"
    if_ = "if"
    else_ = "else"
    end = "end"
    fn = "fn"
    struct = "struct"
    enum = "enum"
    match_ = "match"
    return_ = "return"
    false = "false"
    true = "true"
    while_ = "while"
    break_ = "break"
    continue_ = "continue"


@dataclass
class Span:
    start_line: int
    start_column: int
    end_line: int
    end_column: int

    def __init__(self,
                 start_line: int,
                 start_column: int,
                 end_line: int = 0,
                 end_column: int = 0):
        self.start_line = start_line
        self.start_column = start_column
        self.end_line = end_line or start_line
        self.end_column = end_column or start_column

    def merge(self, other: "Span"):
        return Span(self.start_line, self.start_column, other.end_line,
                    other.end_column)

    def __repr__(self):
        if self.start_line == self.end_line and self.start_column == self.end_column:
            return f"[{self.start_line}:{self.start_column}]"
        return f"[{self.start_line}:{self.start_column} - {self.end_line}:{self.end_column}]"


@dataclass
class Token:
    kind: TokenKind
    span: Span

    def __repr__(self):
        return f"{repr(self.span)} {self.kind.name}"


@dataclass
class ValueToken(Token):
    value: str

    def __repr__(self):
        return f"{repr(self.span)} {self.kind.name}({self.value!r})"


@dataclass
class KeywordToken(Token):
    keyword: Keyword

    def __repr__(self):
        return f"{repr(self.span)} {self.kind.name}({self.keyword.value!r})"


@dataclass
class StringToken(ValueToken):
    quote: str

    def __repr__(self):
        value = repr(self.value)[1:-1]
        return f"{repr(self.span)} {self.kind.name}({self.quote}{value}{self.quote})"


@dataclass
class FormatStringToken(Token):
    parts: t.List[t.Union[StringToken, t.List[Token]]]


class Lexer:

    def __init__(self, src: str):
        # We add a null byte to the end of the source code to make it easier
        # to lex the last token.
        self.src = src + "\0"
        self.state: t.Optional[Token] = None
        self.tokens: t.List[Token] = []
        self.line = 1
        self.column = 1
        self.idx = 0

    def emit(self, advance: bool = False):
        if self.state is None:
            raise ValueError("No state to emit")
        self.state.span.end_line = self.line
        self.state.span.end_column = self.column - 1
        self.tokens.append(self.state)
        self.state = None
        if not advance:
            self.idx -= 1
            self.column -= 1

    def emit_single(self, kind: TokenKind):
        self.tokens.append(Token(kind, Span(self.line, self.column)))
        self.state = None

    def span(self) -> Span:
        return Span(self.line, self.column)

    def parse(self):
        is_escape = False
        while self.idx < len(self.src):
            c = self.src[self.idx]
            self.idx += 1
            self.column += 1
            match self.state:
                case None:
                    match c:
                        case '"' | "'":
                            self.state = StringToken(TokenKind.string,
                                                     self.span(), "", c)
                        case "(":
                            self.emit_single(TokenKind.lparen)
                        case ")":
                            self.emit_single(TokenKind.rparen)
                        case ":":
                            self.emit_single(TokenKind.colon)
                        case "=":
                            c = self.src[self.idx]
                            if c == "=":
                                self.idx += 1
                                self.column += 1
                                self.emit_single(TokenKind.double_equal)
                            else:
                                self.emit_single(TokenKind.equal)
                        case ",":
                            self.emit_single(TokenKind.comma)
                        case ".":
                            self.emit_single(TokenKind.dot)
                        case "+":
                            self.emit_single(TokenKind.plus)
                        case "*":
                            self.emit_single(TokenKind.star)
                        case "/":
                            self.emit_single(TokenKind.slash)
                        case "!":
                            self.emit_single(TokenKind.exclamation)
                        case "&":
                            self.emit_single(TokenKind.ampersand)
                        case "<":
                            self.emit_single(TokenKind.langle_bracket)
                        case ">":
                            self.emit_single(TokenKind.rangle_bracket)
                        case "-":
                            self.state = ValueToken(TokenKind.comment,
                                                    self.span(), c)
                        case c if c.isspace():
                            if c == "\n":
                                self.line += 1
                                self.column = 1
                            self.state = ValueToken(TokenKind.whitespace,
                                                    self.span(), c)
                        case c if c.isnumeric():
                            self.state = ValueToken(TokenKind.integer,
                                                    self.span(), c)
                        case c if c.isalpha():
                            self.state = ValueToken(TokenKind.identifier,
                                                    self.span(), c)
                        case "\0":
                            continue
                        case _:
                            raise ValueError(
                                f"Unexpected character: {c} at {self.span()}")
                case ValueToken(TokenKind.comment, _):
                    if self.state.value == "-":
                        if c != "-":
                            self.emit_single(TokenKind.minus)
                            self.idx -= 1
                        else:
                            self.state.value += c
                        continue
                    if self.state.value.startswith("---"):
                        # Multiline comment.
                        if self.state.value.endswith("---") and len(
                                self.state.value) >= 6:
                            self.emit()
                        else:
                            self.state.value += c
                    else:
                        if c == "\n":
                            self.emit()
                        else:
                            self.state.value += c
                case StringToken(_, _):
                    if is_escape:
                        match c:
                            case "n":
                                self.state.value += "\n"
                            case "t":
                                self.state.value += "\t"
                            case "r":
                                self.state.value += "\r"
                            case "\\":
                                self.state.value += "\\"
                            case "'":
                                self.state.value += "'"
                            case '"':
                                self.state.value += '"'
                            case _:
                                raise ValueError(
                                    f"Invalid escape sequence: \\{c} at {self.span()}")
                        is_escape = False
                        continue
                    if c == "\\":
                        is_escape = True
                        continue
                    if c == self.state.quote:
                        if self.state.kind == TokenKind.format_string:
                            self.parse_format_string()
                        self.emit(advance=True)
                    else:
                        self.state.value += c
                case ValueToken(TokenKind.whitespace, _):
                    if c.isspace():
                        if c == "\n":
                            self.line += 1
                            self.column = 1
                        self.state.value += c
                    else:
                        self.emit()
                case ValueToken(TokenKind.integer, _):
                    if c.isnumeric():
                        self.state.value += c
                    elif c == ".":
                        self.state.kind = TokenKind.float_
                        self.state.value += c
                    else:
                        self.emit()
                case ValueToken(TokenKind.float_, _):
                    if c.isnumeric():
                        self.state.value += c
                    else:
                        self.emit()
                case ValueToken(TokenKind.identifier, _):
                    if c.isalnum() or c == "_":
                        self.state.value += c
                    elif c == '"' and self.state.value == "f":
                        self.state = StringToken(TokenKind.format_string,
                                                 self.span(), "", c)
                    else:
                        if self.state.value in Keyword.__members__.values():
                            self.state = KeywordToken(
                                TokenKind.keyword, self.state.span,
                                Keyword(self.state.value))
                        self.emit()
                case _:
                    raise ValueError(f"Unexpected state: {self.state}")

    def parse_format_string(self):

        @dataclass
        class Part:
            value: str
            is_expression: bool

        if not isinstance(self.state, StringToken):
            raise ValueError(f"Expected StringToken at {self.span()}")

        part: Part = Part("", False)
        parts: t.List[Part] = []
        next_iter = iter(self.state.value)
        next(next_iter, None)
        this_iter = iter(self.state.value)
        for c in this_iter:
            next_c = next(next_iter, None)
            if part.is_expression:
                if c == "}":
                    parts.append(part)
                    part = Part("", False)
                else:
                    part.value += c
            else:
                if c == "{" and next_c == "{":
                    next(this_iter, None)
                    part.value += c
                elif c == "{":
                    if part.value:
                        parts.append(part)
                    part = Part("", True)
                else:
                    part.value += c
        parts.append(part)
        res = FormatStringToken(TokenKind.format_string, self.span(), [])
        for part in parts:
            if part.is_expression:
                lexer = Lexer(part.value)
                lexer.parse()
                if not lexer.tokens:
                    raise ValueError(
                        f"Empty format string expression at {self.span()}")
                res.parts.append(lexer.tokens)
            else:
                res.parts.append(
                    StringToken(TokenKind.string,
                                self.span(),
                                part.value,
                                quote=self.state.quote))
        self.state = res


@dataclass
class Type():
    klar_name: str
    llvm_name: str
    size: int
    enum_variant: t.Optional[str] = None

    def __repr__(self):
        return self.llvm_name


@dataclass
class BuiltinTypes:
    i32 = Type("i32", "i32", 4)
    f32 = Type("f32", "float", 4)
    str_ = Type("str", "i8*", 8)
    bool_ = Type("bool", "i1", 1)
    void = Type("void", "void", 0)
    # This is not a real type, but it's used to represent a function.
    fn = Type("fn", "fn", 0)


@dataclass
class Node:
    span: Span


@dataclass
class Expression(Node):
    pass


@dataclass
class BinaryExpression(Expression):

    class Operator(Enum):
        add = "+"
        sub = "-"
        mul = "*"
        div = "/"
        mod = "%"
        eq = "=="
        ne = "!="
        lt = "<"
        gt = ">"
        le = "<="
        ge = ">="

    left: Expression
    right: Expression
    operator: Operator


@dataclass
class Literal(Expression):
    value: t.Union[int, float, bool, str]


@dataclass
class FormatString(Expression):
    parts: t.List[Expression]


@dataclass
class Void(Expression):
    pass


@dataclass
class Block(Expression):
    expressions: t.List[Expression]


@dataclass
class Parameter(Expression):
    name: str
    type: Type


@dataclass
class VariableRead(Expression):
    name: str


@dataclass
class VariableWrite(Expression):
    name: str
    value: Expression


@dataclass
class FunctionDefinition(Expression):
    name: str
    parameters: t.List[Parameter]
    return_type: Type
    body: Block


@dataclass
class VariableDefinition(Expression):
    name: str
    mutable: bool
    type: t.Optional[Type]
    value: Expression


@dataclass
class FunctionCall(Expression):
    name: str
    arguments: t.List[Expression]


@dataclass
class ReturnExpression(Expression):
    expression: Expression


@dataclass
class IfExpression(Expression):
    condition: Expression
    then: Block
    else_: t.Optional[Block]


@dataclass
class Field(Expression):
    name: str
    type: Type


@dataclass
class StructDefinition(Expression):
    name: str
    fields: t.List[Field]

    def size(self):
        return sum(f.type.size for f in self.fields
                   if not f.name.startswith("__@"))

    def type(self):
        return Type(self.name, f"%{self.name}*", self.size())


@dataclass
class FieldRead(Expression):
    name: str
    field_path: t.List[str]
    struct_name: t.Optional[str] = None
    enum_variant: t.Optional[str] = None


@dataclass
class FieldWrite(Expression):
    name: str
    field_path: t.List[str]
    value: Expression


@dataclass
class MatchPattern(Node):
    literal: t.Optional[Literal] = None
    identifier: t.Optional[str] = None
    namespace: t.Optional[str] = None
    field_patterns: t.Optional[t.List["MatchPattern"]] = None


@dataclass
class MatchArm(Node):
    pattern: MatchPattern
    block: Block


@dataclass
class MatchExpression(Expression):
    expression: Expression
    arms: t.List[MatchArm]
    default_arm: t.Optional[Block]


@dataclass
class EnumDefinition(Expression):
    name: str
    variants: t.List[StructDefinition]

    def size(self):
        return max(x.size() for x in self.variants)

    def type(self):
        return Type(self.name, f"%{self.name}*", self.size())


@dataclass
class While(Expression):
    condition: Expression
    block: Block


@dataclass
class Break(Expression):
    pass


@dataclass
class Continue(Expression):
    pass


class Parser:

    def __init__(self, tokens: t.List[Token]):
        self.tokens = tokens
        self.structs: t.List[StructDefinition] = []
        self.enums: t.List[EnumDefinition] = []
        self.idx = 0

    def token(self):
        token = None
        while self.idx < len(self.tokens):
            token = self.tokens[self.idx]
            match token:
                case ValueToken(TokenKind.comment, _):
                    self.idx += 1
                case ValueToken(TokenKind.whitespace, _):
                    self.idx += 1
                case _:
                    break
        self.idx += 1
        return token

    def parse(self):
        return self.parse_program()

    def parse_program(self) -> Block:
        expressions = []
        span_start = Span(-1, -1)
        span_end = Span(-1, -1)
        while True:
            token = self.token()
            if not token:
                break
            if span_start.start_line == -1:
                span_start = token.span
            span_end = token.span
            expressions.append(self.parse_expression(token))
        return Block(span_start.merge(span_end), expressions)

    def parse_expression(self, token: t.Optional[Token]) -> Expression:
        if not token:
            raise ValueError("Unexpected end of tokens")
        expression: Expression
        match token:
            case KeywordToken(TokenKind.keyword, _, Keyword.fn):
                expression = self.parse_function_definition()
            case ValueToken(TokenKind.identifier, _):
                next_token = self.token()
                match next_token:
                    case Token(TokenKind.lparen):
                        expression = self.parse_function_call(token.value)
                    case Token(TokenKind.equal):
                        value = self.parse_expression(self.token())
                        expression = VariableWrite(
                            token.span.merge(value.span), token.value, value)
                    case Token(TokenKind.dot):
                        name = token.value
                        fields = []
                        end_of_fields_span = token.span
                        while True:
                            next_token = self.token()
                            match next_token:
                                case ValueToken(TokenKind.identifier, _, _):
                                    fields.append(next_token.value)
                                    end_of_fields_span = next_token.span
                                    next_token = self.token()
                                    match next_token:
                                        case Token(TokenKind.dot):
                                            continue
                                        case Token(TokenKind.lparen):
                                            return self.parse_function_call(
                                                f"{name}.{'.'.join(fields)}")
                                        case _:
                                            self.idx -= 1
                                            break
                                case _:
                                    raise ValueError(
                                        f"Expected identifier, got {next_token}"
                                    )
                        if not fields:
                            raise ValueError(
                                f"Expected at least one field at {token.span}")
                        next_token = self.token()
                        match next_token:
                            case Token(TokenKind.equal):
                                value = self.parse_expression(self.token())
                                expression = FieldWrite(
                                    token.span.merge(value.span), name, fields,
                                    value)
                            case _:
                                self.idx -= 1
                                expression = FieldRead(
                                    token.span.merge(end_of_fields_span), name,
                                    fields)
                    case _:
                        self.idx -= 1
                        expression = VariableRead(token.span, token.value)
            case ValueToken(TokenKind.integer, _, value):
                expression = Literal(token.span, int(value))
            case ValueToken(TokenKind.float_, _, value):
                expression = Literal(token.span, float(value))
            case ValueToken(TokenKind.string, _, value):
                expression = Literal(token.span, value)
            case Token(TokenKind.ampersand):
                # In this implementation, everything is a reference.
                expression = self.parse_expression(self.token())
            case FormatStringToken(TokenKind.format_string, _, value):
                expression = self.parse_format_string(token)
            case KeywordToken(TokenKind.keyword, _, Keyword.true):
                expression = Literal(token.span, True)
            case KeywordToken(TokenKind.keyword, _, Keyword.false):
                expression = Literal(token.span, False)
            case KeywordToken(TokenKind.keyword, _, Keyword.let):
                expression = self.parse_variable_definition(mutable=False)
            case KeywordToken(TokenKind.keyword, _, Keyword.struct):
                struct = self.parse_struct_definition()
                self.structs.append(struct)
                expression = struct
            case KeywordToken(TokenKind.keyword, _, Keyword.enum):
                enum = self.parse_enum_definition()
                self.enums.append(enum)
                expression = enum
            case KeywordToken(TokenKind.keyword, _, Keyword.match_):
                expression = self.parse_match_expression()
            case KeywordToken(TokenKind.keyword, _, Keyword.if_):
                expression = self.parse_if_expression()
            case KeywordToken(TokenKind.keyword, _, Keyword.mut):
                expression = self.parse_variable_definition(mutable=True)
            case KeywordToken(TokenKind.keyword, _, Keyword.return_):
                expression = self.parse_return_expression(token)
            case KeywordToken(TokenKind.keyword, _, Keyword.while_):
                expression = self.parse_while(token)
            case KeywordToken(TokenKind.keyword, _, Keyword.break_):
                expression = Break(token.span)
            case KeywordToken(TokenKind.keyword, _, Keyword.continue_):
                expression = Continue(token.span)
            case _:
                raise ValueError(f"Unexpected token: {token}")
        binary_expression_op = self.probe_binary_expression_op()
        if not binary_expression_op:
            return expression
        rhs = self.parse_expression(self.token())
        return BinaryExpression(expression.span.merge(rhs.span), expression,
                                rhs, binary_expression_op)

    def probe_binary_expression_op(
            self) -> t.Optional[BinaryExpression.Operator]:
        next_token = self.token()
        if not next_token:
            return None
        match next_token:
            case Token(TokenKind.plus, _):
                return BinaryExpression.Operator.add
            case Token(TokenKind.minus, _):
                return BinaryExpression.Operator.sub
            case Token(TokenKind.star, _):
                return BinaryExpression.Operator.mul
            case Token(TokenKind.slash, _):
                return BinaryExpression.Operator.div
            case Token(TokenKind.double_equal, _):
                return BinaryExpression.Operator.eq
            case Token(TokenKind.langle_bracket, _):
                next_token = self.token()
                match next_token:
                    case Token(TokenKind.equal, _):
                        return BinaryExpression.Operator.le
                    case _:
                        self.idx -= 1
                return BinaryExpression.Operator.lt
            case Token(TokenKind.rangle_bracket, _):
                next_token = self.token()
                match next_token:
                    case Token(TokenKind.equal, _):
                        return BinaryExpression.Operator.ge
                    case _:
                        self.idx -= 1
                return BinaryExpression.Operator.gt
            case Token(TokenKind.exclamation, _):
                next_token = self.token()
                match next_token:
                    case Token(TokenKind.equal, _):
                        return BinaryExpression.Operator.ne
                    case _:
                        self.idx -= 1
        self.idx -= 1
        return None

    def parse_if_expression(self) -> IfExpression:
        condition = self.parse_expression(self.token())
        then, then_end_keyword = self.parse_block()
        if then_end_keyword == Keyword.else_:
            else_, _ = self.parse_block()
            span = condition.span.merge(else_.span)
            return IfExpression(span, condition, then, else_)
        elif then_end_keyword is None:
            token = self.token()
            match token:
                case KeywordToken(TokenKind.keyword, _, Keyword.else_):
                    else_, _ = self.parse_block()
                    span = condition.span.merge(else_.span)
                    return IfExpression(span, condition, then, else_)
                case _:
                    self.idx -= 1
        span = condition.span.merge(then.span)
        return IfExpression(span, condition, then, None)

    def parse_match_expression(self) -> MatchExpression:
        value = self.parse_expression(self.token())
        match self.token():
            case Token(TokenKind.colon):
                pass
            case _:
                raise ValueError(f"Expected :, got {self.token()}")
        arms = []
        default_arm: t.Optional[Block] = None
        while True:
            token = self.token()
            match token:
                case KeywordToken(TokenKind.keyword, _, Keyword.end):
                    break
                case None:
                    raise ValueError("Unexpected EOF")
                case _:
                    match token:
                        case KeywordToken(TokenKind.keyword, _, Keyword.else_):
                            default_arm, _ = self.parse_block()
                        case _:
                            pattern = self.parse_match_pattern(token)
                            body, _ = self.parse_block()
                            arms.append(
                                MatchArm(token.span.merge(body.span), pattern,
                                         body))
        return MatchExpression(value.span.merge(token.span), value, arms,
                               default_arm)

    def parse_match_pattern(self, token: Token) -> MatchPattern:
        match token:
            case ValueToken(TokenKind.string, _, value) | ValueToken(
                    TokenKind.float_, _, value) | ValueToken(
                        TokenKind.integer, _, value) | ValueToken(
                            TokenKind.bool_, _, value):
                literal = self.parse_expression(token)
                if not isinstance(literal, Literal):
                    raise ValueError(f"Expected literal, got {literal}")
                return MatchPattern(span=token.span, literal=literal)
            case ValueToken(TokenKind.identifier, _, identifier):
                namespace = None
                next_token = self.token()
                match next_token:
                    case Token(TokenKind.dot):
                        namespace = identifier
                        next_token = self.token()
                        match next_token:
                            case ValueToken(TokenKind.identifier, _,
                                            identifier_):
                                identifier = identifier_
                            case _:
                                raise ValueError(
                                    f"Expected identifier, got {next_token}")
                        next_token = self.token()
                field_patterns = []
                match next_token:
                    case Token(TokenKind.lparen):
                        while True:
                            next_token = self.token()
                            match next_token:
                                case Token(TokenKind.rparen):
                                    break
                                case Token(TokenKind.comma):
                                    continue
                                case _:
                                    if not next_token:
                                        raise ValueError("Unexpected EOF")
                                    field_patterns.append(
                                        self.parse_match_pattern(next_token))
                    case _:
                        self.idx -= 1
                return MatchPattern(span=token.span,
                                    identifier=identifier,
                                    namespace=namespace,
                                    field_patterns=field_patterns)
            case _:
                raise ValueError(f"Unexpected token: {token}")

    def parse_format_string(self, token: FormatStringToken) -> FormatString:
        res = FormatString(token.span, [])
        for part in token.parts:
            match part:
                case StringToken(_, span, string):
                    res.parts.append(Literal(span, string))
                case _:
                    parser = Parser(part)
                    res.parts.append(parser.parse())
        return res

    def parse_type(self, type_token: Token) -> Type:
        match type_token:
            case ValueToken(TokenKind.identifier, _, type_):
                type_str = type_
            case Token(TokenKind.ampersand):
                # In this implementation, we don't distinguish between references
                # and the actual object - everything is a reference.
                next_token = self.token()
                match next_token:
                    case ValueToken(TokenKind.identifier, _, type_):
                        type_str = type_
                    case _:
                        raise ValueError(f"Expected identifier, got {next_token}")
            case _:
                raise ValueError(f"Expected type, got {type_token}")
        builtin_type = next((t for t in BuiltinTypes.__dict__.values()
                             if isinstance(t, Type) and t.klar_name == type_str),
                            None)
        if builtin_type:
            return builtin_type
        # This must be a user-defined type.
        struct = next((x for x in self.structs if x.name == type_str), None)
        if struct:
            return struct.type()
        enum = next((x for x in self.enums if x.name == type_str), None)
        if not enum:
            raise ValueError(f"Unknown type: {type_str}")
        return enum.type()

    def parse_struct_definition(self) -> StructDefinition:
        token = self.token()
        match token:
            case ValueToken(TokenKind.identifier, _, _):
                name = token.value
            case _:
                raise ValueError(f"Expected identifier: {token}")
        token = self.token()
        match token:
            case Token(TokenKind.colon):
                pass
            case _:
                raise ValueError(f"Expected ':': {token}")
        fields = self.parse_struct_fields()
        span = token.span
        if fields:
            span = span.merge(fields[-1].span)
        return StructDefinition(span, name, fields)

    def parse_struct_fields(self) -> t.List[Field]:
        fields = []
        while True:
            token = self.token()
            if not token:
                raise ValueError("Unexpected EOF")
            match token:
                case ValueToken(TokenKind.identifier, _, name):
                    type_token = self.token()
                    if not type_token:
                        raise ValueError("Unexpected EOF")
                    type_ = self.parse_type(type_token)
                    fields.append(Field(token.span.merge(type_token.span), name, type_))
                case KeywordToken(TokenKind.keyword, _, Keyword.end):
                    break
                case _:
                    raise ValueError(f"Unexpected token: {token}")
        return fields

    def parse_enum_definition(self) -> EnumDefinition:
        token = self.token()
        match token:
            case ValueToken(TokenKind.identifier, _, _):
                name = token.value
            case _:
                raise ValueError(f"Expected identifier: {token}")
        token = self.token()
        match token:
            case Token(TokenKind.colon):
                pass
            case _:
                raise ValueError(f"Expected ':': {token}")
        variants = self.parse_enum_variants(name)
        span = token.span
        if variants:
            span = span.merge(variants[-1].span)
        return EnumDefinition(span, name, variants)

    def parse_enum_variants(self, enum_name: str) -> t.List[StructDefinition]:
        variants = []
        while True:
            token = self.token()
            if not token:
                raise ValueError("Unexpected EOF")
            match token:
                case ValueToken(TokenKind.identifier, _, name):
                    fields = []
                    next_token = self.token()
                    match next_token:
                        case Token(TokenKind.lparen):
                            fields = self.parse_enum_fields()
                        case _:
                            self.idx -= 1
                    span = token.span
                    if fields:
                        span = span.merge(fields[-1].span)
                    variants.append(StructDefinition(span, name, fields))
                case KeywordToken(TokenKind.keyword, _, Keyword.end):
                    break
                case _:
                    raise ValueError(f"Unexpected token: {token}")
        return variants

    def parse_enum_fields(self) -> t.List[Field]:
        fields = []
        while True:
            token = self.token()
            if not token:
                raise ValueError("Expected ')'")
            match token:
                case Token(TokenKind.rparen):
                    break
                case Token(TokenKind.comma):
                    continue
                case ValueToken(TokenKind.identifier, _, name):
                    type_token = self.token()
                    match type_token:
                        case ValueToken(TokenKind.identifier, _, _):
                            fields.append(
                                Field(token.span.merge(type_token.span), name,
                                      self.parse_type(type_token)))
                        case _:
                            raise ValueError(f"Expected type: {type_token}")
                case _:
                    raise ValueError(f"Unexpected token: {token}")
        return fields

    def parse_variable_definition(self, mutable: bool) -> VariableDefinition:
        token = self.token()
        match token:
            case ValueToken(TokenKind.identifier, _, name):
                token = self.token()
                match token:
                    case Token(TokenKind.equal):
                        pass
                    case _:
                        raise ValueError(f"Expected '=': {token}")
                token = self.token()
                if token is None:
                    raise ValueError("Unexpected EOF")
                expression = self.parse_expression(token)
                return VariableDefinition(token.span.merge(expression.span),
                                          name, mutable, None, expression)
            case _:
                raise ValueError(f"Expected identifier: {token}")

    def parse_while(self, token: KeywordToken) -> While:
        return While(token.span, self.parse_expression(self.token()),
                     self.parse_block()[0])

    def parse_function_definition(self) -> FunctionDefinition:
        token = self.token()
        match token:
            case ValueToken(TokenKind.identifier, _, name):
                token = self.token()
                match token:
                    case Token(TokenKind.lparen):
                        pass
                    case _:
                        raise ValueError(f"Expected '(': {token}")
                parameters = self.parse_parameters()
                type_token = self.token()
                if type_token is None:
                    raise ValueError("Unexpected EOF")
                match type_token:
                    case ValueToken(TokenKind.identifier, _):
                        return_type = self.parse_type(type_token)
                    case _:
                        return_type = BuiltinTypes.void
                        self.idx -= 1
                body, body_block_end = self.parse_block()
                if body_block_end and body_block_end != Keyword.end:
                    raise ValueError(f"Expected 'end': {body_block_end}")
                if return_type == BuiltinTypes.void:
                    # `void` functions must end with `return`.
                    span = Span(body.span.end_line, body.span.end_column)
                    # `main` functions must return `int`.
                    if name == "main":
                        body.expressions.append(
                            ReturnExpression(span, Literal(span, 0)))
                        return_type = BuiltinTypes.i32
                    else:
                        body.expressions.append(
                            ReturnExpression(span, Void(span)))
                return FunctionDefinition(token.span.merge(body.span), name,
                                          parameters, return_type, body)
            case _:
                raise ValueError(f"Expected identifier: {token}")

    def parse_parameters(self) -> t.List[Parameter]:
        parameters = []
        while True:
            token = self.token()
            if not token:
                raise ValueError("Expected ')'")
            match token:
                case Token(TokenKind.rparen):
                    break
                case Token(TokenKind.comma):
                    continue
                case ValueToken(TokenKind.identifier, _, name):
                    type_token = self.token()
                    match type_token:
                        case ValueToken(TokenKind.identifier, _, _):
                            parameters.append(
                                Parameter(token.span.merge(type_token.span),
                                          name,
                                          self.parse_type(type_token)))
                        case _:
                            raise ValueError(f"Expected type: {type_token}")
                case _:
                    raise ValueError(f"Unexpected token: {token}")
        return parameters

    def parse_fields(self) -> t.List[Parameter]:
        parameters = []
        while True:
            token = self.token()
            if not token:
                raise ValueError("Expected ')'")
            match token:
                case Token(TokenKind.rparen):
                    break
                case Token(TokenKind.comma):
                    continue
                case ValueToken(TokenKind.identifier, _, name):
                    type_token = self.token()
                    match type_token:
                        case ValueToken(TokenKind.identifier, _, _):
                            parameters.append(
                                Parameter(token.span.merge(type_token.span),
                                          name,
                                          self.parse_type(type_token)))
                        case _:
                            raise ValueError(f"Expected type: {type_token}")
                case _:
                    raise ValueError(f"Unexpected token: {token}")
        return parameters

    def parse_block(self) -> t.Tuple[Block, t.Optional[Keyword]]:
        expressions: t.List[Expression] = []
        span_start = Span(-1, -1)
        span_end = Span(-1, -1)
        token = self.token()
        is_single_line = False
        match token:
            case Token(TokenKind.colon, _):
                is_single_line = False
            case Token(TokenKind.equal, _):
                next_token = self.token()
                match next_token:
                    case Token(TokenKind.rangle_bracket, _):
                        is_single_line = True
                    case _:
                        raise ValueError(f"Expected '>': {next_token}")
            case _:
                raise ValueError(f"Expected ':' or '=>': {token}")
        if is_single_line:
            expression = self.parse_expression(self.token())
            return Block(token.span.merge(expression.span), [expression]), None
        while True:
            token = self.token()
            if not token:
                break
            if span_start.start_line == -1:
                span_start = token.span
            span_end = token.span
            match token:
                case KeywordToken(TokenKind.keyword, _,
                                  Keyword.end) | KeywordToken(
                                      TokenKind.keyword, _, Keyword.else_):
                    return Block(span_start.merge(span_end),
                                 expressions), token.keyword
                case _:
                    expressions.append(self.parse_expression(token))
        raise ValueError("Expected end")

    def parse_return_expression(self, return_token: Token) -> ReturnExpression:
        token = self.token()
        match token:
            case None:
                return ReturnExpression(return_token.span,
                                        Void(return_token.span))
            case KeywordToken(TokenKind.keyword, _, Keyword.end):
                self.idx -= 1
                return ReturnExpression(return_token.span,
                                        Void(return_token.span))
            case _:
                expression = self.parse_expression(token)
                return ReturnExpression(
                    return_token.span.merge(expression.span), expression)

    def parse_function_call(self, name) -> FunctionCall:
        arguments: t.List[Expression] = []
        span_start = Span(-1, -1)
        span_end = Span(-1, -1)
        while True:
            token = self.token()
            if not token:
                raise ValueError("Unexpected EOF")
            if span_start.start_line == -1:
                span_start = token.span
            span_end = token.span
            match token:
                case Token(TokenKind.rparen):
                    return FunctionCall(span_start.merge(span_end), name,
                                        arguments)
                case Token(TokenKind.comma):
                    continue
                case _:
                    arguments.append(self.parse_expression(token))


class BlockCodegen:

    @dataclass
    class Value:
        name: str
        type: Type

        def __repr__(self):
            return f"{self.type} {self.name}"

    @dataclass
    class Struct:
        name: str
        definition: StructDefinition

        def __repr__(self):
            return f"{self.definition.name} {self.name}"

    @dataclass
    class Enum:
        name: str
        definition: EnumDefinition

        def __repr__(self):
            return f"{self.definition.name} {self.name}"

    @dataclass
    class Variable:
        span: Span
        name: str
        llvm_reg: str
        type: Type
        mutable: bool
        is_parameter: bool = False

    @dataclass
    class Namespace:
        name: str
        constants: t.Dict[str, "BlockCodegen.Value"]
        functions: t.Dict[str, FunctionDefinition]

    def __init__(self,
                 codegen: "Codegen",
                 block: Block,
                 break_label: t.Optional[str] = None,
                 continue_label: t.Optional[str] = None,
                 parent: t.Optional["BlockCodegen"] = None):
        self.codegen = codegen
        self.block = block
        self.break_label = break_label
        self.continue_label = continue_label
        self.parent = parent
        self.code: t.List[str] = []
        self.indent: int = 1 if parent else 0
        self.literals: t.Dict[t.Union[str, int, bool, float], int] = {}
        self.functions: t.Dict[str, FunctionDefinition] = {}
        self.variables: t.Dict[str, BlockCodegen.Variable] = {}
        self.structs: t.Dict[str, BlockCodegen.Struct] = {}
        self.namespaces: t.Dict[str, BlockCodegen.Namespace] = {}
        self.enums: t.Dict[str, BlockCodegen.Enum] = {}

    def emit(self, code):
        self.code.append(f"{'  ' * (self.indent)}{code}")

    def generate(self) -> Value:
        res = self.Value("void", BuiltinTypes.void)
        for node in self.block.expressions:
            res = self.generate_expression(node)
        return res

    def generate_expression(self, expression) -> Value:
        match expression:
            case Literal():
                return self.generate_literal(expression)
            case FormatString():
                return self.generate_format_string(expression)
            case Void():
                return self.generate_void(expression)
            case Block():
                return self.generate_block(expression)
            case FunctionDefinition():
                return self.generate_function_definition(expression)
            case StructDefinition():
                return self.generate_struct_definition(expression)
            case EnumDefinition():
                return self.generate_enum_definition(expression)
            case VariableDefinition():
                return self.generate_variable_definition(expression)
            case FieldRead():
                return self.generate_field_read(expression)
            case FieldWrite():
                return self.generate_field_write(expression)
            case VariableRead():
                return self.generate_variable_read(expression)
            case VariableWrite():
                return self.generate_variable_write(expression)
            case FunctionCall():
                return self.generate_function_call(expression)
            case ReturnExpression():
                return self.generate_return_expression(expression)
            case BinaryExpression():
                return self.generate_binary_expression(expression)
            case IfExpression():
                return self.generate_if_expression(expression)
            case MatchExpression():
                return self.generate_match_expression(expression)
            case While():
                return self.generate_while(expression)
            case Break():
                return self.generate_break(expression)
            case Continue():
                return self.generate_continue(expression)
            case _:
                raise ValueError(f"Unknown expression: {expression}")

    def generate_literal(self, literal: Literal) -> Value:
        match literal.value:
            case bool():
                return self.Value(str(1 if literal.value else 0),
                                  BuiltinTypes.bool_)
            case int():
                return self.Value(str(literal.value), BuiltinTypes.i32)
            case float():
                return self.Value(str(literal.value), BuiltinTypes.f32)
            case str():
                literal_const = self.codegen.literal_const(literal)
                return self.Value(
                    f"getelementptr ([{len(literal.value) + 1} x i8], ptr {literal_const}, i32 0, i32 0)",
                    BuiltinTypes.str_,
                )

    def generate_while(self, while_: While) -> Value:
        id = self.codegen.next_id()
        continue_label = f".{id}_while_cond"
        break_label = f".{id}_while_end"
        self.emit(f"br label %{continue_label}")
        self.emit(f"{continue_label}:")
        cond = self.generate_expression(while_.condition)
        self.emit(f"br {cond}, label %.{id}_while_body, label %{break_label}")
        self.emit(f".{id}_while_body:")
        res = self.generate_block(while_.block, break_label, continue_label)
        self.emit(f"br label %{continue_label}")
        self.emit(f"{break_label}:")
        return res

    def generate_break(self, break_: Break) -> Value:
        block_codegen: t.Optional[BlockCodegen] = self
        while block_codegen and block_codegen.break_label is None:
            block_codegen = block_codegen.parent
        if block_codegen is None:
            raise ValueError(f"No break label found at {break_.span}")
        self.emit(f"br label %{block_codegen.break_label}")
        return self.Value("void", BuiltinTypes.void)

    def generate_continue(self, continue_: Continue) -> Value:
        block_codegen: t.Optional[BlockCodegen] = self
        while block_codegen and block_codegen.continue_label is None:
            block_codegen = block_codegen.parent
        if block_codegen is None:
            raise ValueError(f"No continue label found at {continue_.span}")
        self.emit(f"br label %{block_codegen.continue_label}")
        return self.Value("void", BuiltinTypes.void)

    def generate_format_string(self, format_string: FormatString) -> Value:
        id = self.codegen.next_id()
        self.emit("; format-string")
        res = self.Value(f"%.{id}_fstr", BuiltinTypes.str_)
        res_len_reg = f"%.{id}_fstr_len"
        self.emit(f"{res_len_reg} = alloca i32")
        self.emit(f"store i32 1, i32* {res_len_reg}")
        code = []

        def handle_number(value: BlockCodegen.Value):
            self.emit(f"%.{id}_str = alloca i8, i32 32")
            self.emit(
                f"%.{id}_clear = call i8* @memset(i8* %.{id}_str, i8 0, i32 32)"
            )
            self.emit(
                f"%.{id}_printf = call i32 (i8*, i8*, ...) @sprintf(i8* %.{id}_str, i8* getelementptr inbounds ([32 x i8], [32 x i8]* @i32_format, i32 0, i32 0), {value})"
            )
            self.emit(f"%.{id}_str_len = call i32 @strlen(i8* %.{id}_str)")
            code.append(
                f"%.{id} = call i8* @strcat(i8* {res.name}, i8* %.{id}_str)")

        for part in format_string.parts:
            id = self.codegen.next_id()
            match part:
                case Literal(_, str()):
                    if not part.value:
                        continue
                    literal = self.generate_literal(part)
                    self.emit(f"%.{id}_str_len = call i32 @strlen({literal})")
                    code.append(
                        f"%.{id} = call i8* @strcat(i8* {res.name}, {literal})"
                    )
                case Block():
                    block_val = self.generate_block(part)
                    match block_val.type:
                        case BuiltinTypes.str_:
                            self.emit(
                                f"%.{id}_str_len = call i32 @strlen({block_val})"
                            )
                            code.append(
                                f"%.{id} = call i8* @strcat(i8* {res.name}, {block_val})"
                            )
                        case BuiltinTypes.i32 | BuiltinTypes.f32:
                            handle_number(block_val)
                        case BuiltinTypes.bool_:
                            self.emit(
                                f"%.{id}_str = select {block_val}, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @true_str, i32 0, i32 0), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @false_str, i32 0, i32 0)"
                            )
                            self.emit(
                                f"%.{id}_str_len = call i32 @strlen(i8* %.{id}_str)"
                            )
                            code.append(
                                f"%.{id} = call i8* @strcat(i8* {res.name}, i8* %.{id}_str)"
                            )
                        case _:
                            try:
                                enum = self.find_enum(block_val.type.klar_name,
                                                      format_string.span)
                                enum_val = self.Value(f"%.{id}_enum_variant",
                                                      BuiltinTypes.i32)
                                self.emit(
                                    f"%.{id}_enum_variant = load i32, i32* {block_val.name}"
                                )
                                handle_number(enum_val)
                            except ValueError:
                                raise ValueError(
                                    f"Unknown format string block type: {block_val.type} at {format_string.span}"
                                )
                case _:
                    raise ValueError(f"Unknown format string part: {part}")

            self.emit(f"%.{id}_res_len = load i32, i32* {res_len_reg}")
            self.emit(
                f"%.{id}_add_len = add i32 %.{id}_res_len, %.{id}_str_len")
            self.emit(f"store i32 %.{id}_add_len, i32* {res_len_reg}")

        self.emit(f"{res_len_reg}_val_32 = load i32, i32* {res_len_reg}")
        self.emit(f"{res_len_reg}_val = sext i32 {res_len_reg}_val_32 to i64")
        self.emit(
            f"{res.name} = call i8* @calloc(i64 1, i64 {res_len_reg}_val)")
        for line in code:
            self.emit(line)
        self.emit("; /format-string")
        return res

    def generate_void(self, void: Void) -> Value:
        return self.Value("", BuiltinTypes.void)

    def generate_block(self,
                       block: Block,
                       break_label: t.Optional[str] = None,
                       continue_label: t.Optional[str] = None) -> Value:
        codegen = BlockCodegen(self.codegen, block, break_label,
                               continue_label, self)
        reg = codegen.generate()
        for line in codegen.code:
            self.emit(line)
        return reg

    def generate_function_definition(self, func: FunctionDefinition) -> Value:
        parameters = ", ".join(
            [f"{x.type} %{x.name}" for x in func.parameters])
        self.emit(f"define {func.return_type} @{func.name}({parameters}) {{")
        for param in func.parameters:
            self.variables[param.name] = self.Variable(span=param.span,
                                                       name=param.name,
                                                       llvm_reg=f"%{param.name}",
                                                       is_parameter=True,
                                                       mutable=False,
                                                       type=param.type)
        self.generate_block(func.body)
        self.emit("}")
        self.functions[func.name] = func
        return self.Value("void", BuiltinTypes.void)

    def generate_struct_definition(self, struct: StructDefinition) -> Value:
        self.structs[struct.name] = self.Struct(struct.name, struct)
        self.emit(f"%{struct.name} = type {{")
        self.indent += 1
        for i, field in enumerate(struct.fields):
            self.emit(
                f"{field.type}{',' if i < len(struct.fields) - 1 else ''}")
        self.indent -= 1
        self.emit("}")
        res = self.Value("void", BuiltinTypes.void)
        # Create the constructor.
        func = FunctionDefinition(
            span=struct.span,
            name=struct.name,
            return_type=struct.type(),
            parameters=[
                Parameter(name=x.name, type=x.type, span=x.span)
                for x in struct.fields
            ],
            body=Block(struct.span, []),
        )
        self.functions[func.name] = func
        parameters = ", ".join(
            [f"{x.type} %{x.name}" for x in func.parameters])
        self.emit(f"define {func.return_type} @{func.name}({parameters}) {{")
        self.indent += 1
        self.emit(
            f"%sizeptr = getelementptr inbounds {func.return_type}, {func.return_type}* null, i32 1"
        )
        self.emit(f"%size = ptrtoint {func.return_type}* %sizeptr to i64")
        self.emit(f"%alloc = call i8* @calloc(i64 1, i64 %size)")
        # Copy the fields.
        for i, field in enumerate(struct.fields):
            self.emit(
                f"%field{i} = getelementptr inbounds %{struct.name}, ptr %alloc, i32 0, i32 {i}"
            )
            self.emit(
                f"store {field.type} %{field.name}, {field.type}* %field{i}")
        self.emit(f"ret ptr %alloc")
        self.indent -= 1
        self.emit("}")
        return res

    def generate_enum_definition(self, enum: EnumDefinition) -> Value:
        self.enums[enum.name] = self.Enum(enum.name, enum)
        self.emit(f"%{enum.name} = type {{")
        self.indent += 1
        self.emit(f"i32, [{enum.size()} x i8]")
        self.indent -= 1
        self.emit("}")
        namespace = BlockCodegen.Namespace(enum.name, {}, {})
        for variant in enum.variants:
            self.generate_enum_variant(enum, variant, namespace)
        self.namespaces[enum.name] = namespace
        return self.Value("void", BuiltinTypes.void)

    def generate_enum_variant(self, enum: EnumDefinition,
                              variant: StructDefinition,
                              namespace: Namespace) -> Value:
        struct_name = f"{enum.name}.{variant.name}"
        variant_index = enum.variants.index(variant)
        self.structs[struct_name] = self.Struct(struct_name, variant)
        self.emit(f"%{struct_name} = type {{")
        self.indent += 1
        self.emit(f"i32{',' if variant.fields else ''}")
        for i, field in enumerate(variant.fields):
            self.emit(
                f"{field.type}{',' if i < len(variant.fields) - 1 else ''}")
        self.indent -= 1
        self.emit("}")
        res = self.Value("void", BuiltinTypes.void)
        if not variant.fields:
            self.emit(
                f'@{enum.name}.{variant.name}_const = constant %{enum.name} {{i32 {variant_index}, [{enum.size()} x i8] c"{" " * (enum.size())}"}}'
            )
            namespace.constants[variant.name] = self.Value(
                f"@{enum.name}.{variant.name}_const", enum.type())
            return res
        # Create the constructor.
        func = FunctionDefinition(
            span=variant.span,
            name=variant.name,
            return_type=Type(enum.name,
                             f"%{enum.name}*",
                             enum.size(),
                             enum_variant=variant.name),
            parameters=[
                Parameter(name=x.name, type=x.type, span=x.span)
                for x in variant.fields
            ],
            body=Block(variant.span, []),
        )
        namespace.functions[func.name] = func
        parameters = ", ".join(
            [f"{x.type} %{x.name}" for x in func.parameters])
        self.emit(f"define {func.return_type} @{func.name}({parameters}) {{")
        self.indent += 1
        self.emit(
            f"%sizeptr = getelementptr inbounds {func.return_type}, {func.return_type}* null, i32 1"
        )
        self.emit(f"%size = ptrtoint {func.return_type}* %sizeptr to i64")
        self.emit(f"%alloc = call i8* @calloc(i64 1, i64 %size)")
        variant.fields.insert(
            0, Field(variant.span, "__@tag__", BuiltinTypes.i32))
        # Copy the fields.
        for i, field in enumerate(variant.fields):
            self.emit(
                f"%field{i} = getelementptr inbounds %{struct_name}, ptr %alloc, i32 0, i32 {i}"
            )
            if i == 0:
                self.emit(f"store i32 {variant_index}, i32* %field{i}")
            else:
                self.emit(
                    f"store {field.type} %{field.name}, {field.type}* %field{i}"
                )
        self.emit(f"ret ptr %alloc")
        self.indent -= 1
        self.emit("}")
        return res

    def generate_field_read(self, field_read: FieldRead) -> Value:
        try:
            return self.generate_namespace_read(field_read.name,
                                                field_read.field_path[0],
                                                field_read.span)
        except ValueError as e:
            pass
        if field_read.struct_name and field_read.name.startswith("."):
            struct_name = field_read.struct_name
            struct = self.find_struct(struct_name, field_read.span)
            var_type = struct.definition.type()
            if field_read.enum_variant:
                var_type = Type(**struct.definition.type().__dict__)
                var_type.llvm_name = f"%{struct_name}*"
            var = self.Value(f"%{field_read.name}", var_type)
        else:
            var = self.generate_variable_read(
                VariableRead(field_read.span, field_read.name))
            if var.type == BuiltinTypes.fn:
                return var
            struct_name = var.type.klar_name
            if var.type.enum_variant:
                struct_name = f"{struct_name}.{var.type.enum_variant}"
            elif field_read.enum_variant:
                struct_name = f"{struct_name}.{field_read.enum_variant}"
            struct = self.find_struct(struct_name, field_read.span)
        for field_name in field_read.field_path[:-1]:
            field = next(
                (x for x in struct.definition.fields if x.name == field_name),
                None)
            if field is None:
                raise ValueError(
                    f"Struct {struct.name} does not have a field named {field_name}",
                    field_read.span)
            field_index = struct.definition.fields.index(field)
            new_var = self.Value(f"%.{self.codegen.next_id()}", field.type)
            self.emit(
                f"{new_var.name}_ptr = getelementptr %{struct.name}, {var}, i32 0, i32 {field_index}"
            )
            self.emit(
                f"{new_var.name} = load {field.type}, {field.type}* {new_var.name}_ptr"
            )
            var = new_var
            struct = self.find_struct(field.type.klar_name, field.span)
        field_name = field_read.field_path[-1]
        field = next(
            (x for x in struct.definition.fields if x.name == field_name),
            None)
        if field is None:
            raise ValueError(
                f"Struct {struct.name} does not have a field named {field_name}",
                field_read.span)
        field_index = struct.definition.fields.index(field)
        res = self.Value(f"%.{self.codegen.next_id()}", field.type)
        self.emit(
            f"{res.name}_field = getelementptr %{struct.name}, {var}, i32 0, i32 {field_index}"
        )
        self.emit(
            f"{res.name} = load {field.type}, {field.type}* {res.name}_field")
        return res

    def generate_field_write(self, field_write: FieldWrite) -> Value:
        var = self.generate_variable_read(
            VariableRead(field_write.span, field_write.name))
        struct_name = var.type.klar_name
        struct = self.find_struct(struct_name, field_write.span)
        for field_name in field_write.field_path[:-1]:
            field = next(x for x in struct.definition.fields
                         if x.name == field_name)
            field_index = struct.definition.fields.index(field)
            new_var = self.Value(f"%.{self.codegen.next_id()}", field.type)
            self.emit(
                f"{new_var.name}_ptr = getelementptr %{struct.name}, {var}, i32 0, i32 {field_index}"
            )
            self.emit(
                f"{new_var.name} = load {field.type}, {field.type}* {new_var.name}_ptr"
            )
            var = new_var
            struct = self.find_struct(field.type.klar_name, field.span)
        field_name = field_write.field_path[-1]
        field = next(x for x in struct.definition.fields
                     if x.name == field_name)
        field_index = struct.definition.fields.index(field)
        value = self.generate_expression(field_write.value)
        id = self.codegen.next_id()
        self.emit(
            f"%.{id} = getelementptr %{struct.name}, {var}, i32 0, i32 {field_index}"
        )
        self.emit(f"store {value}, {field.type}* %.{id}")
        return value

    def generate_return_expression(self, return_: ReturnExpression) -> Value:
        reg = self.generate_expression(return_.expression)
        self.emit(f"ret {reg}")
        return reg

    def generate_function_call(self, call: FunctionCall) -> Value:
        argument_values = [self.generate_expression(x) for x in call.arguments]
        func = self.find_function(call.name, call.span)
        res = self.Value("void", BuiltinTypes.void)
        code = ""
        if func.return_type.klar_name != "void":
            res = self.Value(f"%.{self.codegen.next_id()}", func.return_type)
            code += f"{res.name} = "
        code += f"call {func.return_type} @{func.name}("
        code += ", ".join([f"{x}" for x in argument_values])
        code += ")"
        self.emit(code)
        return res

    def generate_if_expression(self, if_: IfExpression) -> Value:
        condition = self.generate_expression(if_.condition)
        id = self.codegen.next_id()
        self.emit(f"br {condition}, label %if_true_{id}, label %if_false_{id}")
        self.emit(f"if_true_{id}:")
        self.generate_expression(if_.then)
        self.emit(f"br label %if_end_{id}")
        self.emit(f"if_false_{id}:")
        if if_.else_:
            self.generate_expression(if_.else_)
        self.emit(f"br label %if_end_{id}")
        self.emit(f"if_end_{id}:")
        return self.Value("void", BuiltinTypes.void)

    def generate_variable_definition(self,
                                     variable: VariableDefinition) -> Value:
        res = self.generate_expression(variable.value)
        var_reg = f"%.{variable.name}_{self.codegen.next_id()}"
        variable.type = res.type
        self.emit(f"{var_reg} = alloca {res.type}")
        self.emit(f"store {res}, {res.type}* {var_reg}")
        self.variables[variable.name] = self.Variable(span=variable.span,
                                                      name=variable.name,
                                                      llvm_reg=var_reg,
                                                      mutable=True,
                                                      type=res.type)
        return res

    def generate_variable_read(self, variable_read: VariableRead) -> Value:
        variable = self.find_variable(variable_read.name, variable_read.span)
        if variable.type is None:
            raise ValueError(f"Unknown variable type: {variable_read.name}")
        if variable.is_parameter:
            return self.Value(variable.llvm_reg, variable.type)
        res = self.Value(f"%.{self.codegen.next_id()}", variable.type)
        self.emit(
            f"{res.name} = load {variable.type}, {variable.type}* {variable.llvm_reg}"
        )
        return res

    def generate_variable_write(self, variable_write: VariableWrite) -> Value:
        variable = self.find_variable(variable_write.name, variable_write.span)
        if variable.type is None:
            raise ValueError(f"Unknown variable type: {variable_write.name}")
        if not variable.mutable:
            raise ValueError(f"Variable is not mutable: {variable_write.name}")
        res = self.generate_expression(variable_write.value)
        self.emit(f"store {res}, {variable.type}* {variable.llvm_reg}")
        return res

    def generate_namespace_read(self, namespace_name: str, field: str,
                                span: Span) -> Value:
        namespace = self.find_namespace(namespace_name, span)
        if namespace is None:
            raise ValueError(f"Unknown namespace: {namespace_name}")
        constant = next((x for x in namespace.constants if x == field), None)
        if constant:
            return namespace.constants[constant]
        function = next((x for x in namespace.functions if x == field), None)
        if not function:
            raise ValueError(f"Unknown field: {namespace_name}.{field}")
        return self.Value(f"@{function}", BuiltinTypes.fn)

    def generate_binary_expression(self, binary: BinaryExpression) -> Value:
        left = self.generate_expression(binary.left)
        right = self.generate_expression(binary.right)
        res = self.Value(f"%.{self.codegen.next_id()}", left.type)
        op_name = binary.operator.name
        if op_name == "div":
            op_name = "sdiv"
        if op_name == "eq":
            res.type = BuiltinTypes.bool_
            if right.type.klar_name == "str":
                self.emit(
                    f"{res.name}_str = call i32 @strcmp({left}, {right})")
                self.emit(f"{res.name} = icmp eq i32 {res.name}_str, 0")
                return res
            op_name = "icmp eq"
        if op_name == "ne":
            res.type = BuiltinTypes.bool_
            if right.type.klar_name == "str":
                self.emit(
                    f"{res.name}_str = call i32 @strcmp({left}, {right})")
                self.emit(f"{res.name} = icmp ne i32 {res.name}_str, 0")
                return res
            op_name = "icmp ne"
        if op_name == "lt":
            op_name = "icmp slt"
            res.type = BuiltinTypes.bool_
        if op_name == "gt":
            op_name = "icmp sgt"
            res.type = BuiltinTypes.bool_
        if op_name == "le":
            op_name = "icmp sle"
            res.type = BuiltinTypes.bool_
        if op_name == "ge":
            op_name = "icmp sge"
            res.type = BuiltinTypes.bool_
        self.emit(f"{res.name} = {op_name} {left}, {right.name}")
        return res

    def generate_match_expression(self, match: MatchExpression) -> Value:
        value = self.generate_expression(match.expression)
        id = self.codegen.next_id()
        end_label = f"match_{id}_match_end"

        def string_match(value: BlockCodegen.Value,
                         arm_value: BlockCodegen.Value, id: int):
            self.emit(
                f"%match_{id}_strcmp = call i32 @strcmp({value}, {arm_value})")
            self.emit(f"%match_{id} = icmp eq i32 %match_{id}_strcmp, 0")

        def float_match(value: BlockCodegen.Value,
                        arm_value: BlockCodegen.Value, id: int):
            self.emit(f"%match_{id} = fcmp oeq {value}, {arm_value.name}")

        def int_type_match(value: BlockCodegen.Value,
                           arm_value: BlockCodegen.Value, id: int):
            self.emit(f"%match_{id} = icmp eq {value}, {arm_value.name}")

        def struct_match(value: BlockCodegen.Value, pattern: MatchPattern,
                         id: int) -> t.List[Expression]:
            if pattern.identifier is None or pattern.field_patterns is None:
                raise ValueError(f"Invalid struct pattern: {pattern}")
            if pattern.namespace:
                enum = self.find_enum(pattern.namespace, pattern.span)
                variant = next((x for x in enum.definition.variants
                                if x.name == pattern.identifier), None)
                if not variant:
                    raise ValueError(
                        f"Unknown enum variant: {pattern.identifier}")
                variant_index = enum.definition.variants.index(variant)
                # First test the variant type.
                var_id = self.codegen.next_id(
                ) if pattern.field_patterns else id
                self.emit(
                    f"%match_{var_id}_load_enum_ptr = getelementptr inbounds %{enum.name}, {value}, i32 0, i32 0"
                )
                self.emit(
                    f"%match_{var_id}_load_enum_value = load i32, i32* %match_{var_id}_load_enum_ptr"
                )
                self.emit(
                    f"%match_{var_id} = icmp eq i32 %match_{var_id}_load_enum_value, {variant_index}"
                )
                if not pattern.field_patterns:
                    return []
                self.emit(
                    f"br i1 %match_{var_id}, label %match_{id}_field_matches, label %match_{id}_else"
                )
                self.emit(f"match_{id}_field_matches:")
                _struct = self.find_struct(f"{enum.name}.{variant.name}",
                                           pattern.span)
                enum_variant = variant.name
                fields = _struct.definition.fields.copy()[1:]
                struct_name = _struct.name
            else:
                _struct = self.find_struct(pattern.identifier, pattern.span)
                enum_variant = None
                fields = _struct.definition.fields
                struct_name = _struct.name
            block_prelude: t.List[Expression] = []
            for pattern, field in zip(pattern.field_patterns, fields):
                id2 = self.codegen.next_id()
                if pattern.identifier and not pattern.namespace:
                    variable_definition = VariableDefinition(
                        span=pattern.span,
                        name=pattern.identifier,
                        mutable=False,
                        type=field.type,
                        value=FieldRead(span=pattern.span,
                                        name=value.name.replace("%", ""),
                                        struct_name=struct_name,
                                        enum_variant=enum_variant,
                                        field_path=[pattern.identifier]))
                    block_prelude.append(variable_definition)
                    continue
                field_value = self.generate_field_read(
                    FieldRead(pattern.span,
                              name=value.name.replace("%", ""),
                              struct_name=struct_name,
                              enum_variant=enum_variant,
                              field_path=[field.name]))
                if pattern.identifier and pattern.namespace:
                    match_pattern(field_value, pattern, id2)
                else:
                    match_pattern(field_value, pattern, id2)
                self.emit(
                    f"br i1 %match_{id2}, label %match_{id2}_matches, label %match_{id}_else"
                )
                self.emit(f"match_{id2}_matches:")
            self.emit(f"%match_{id} = add i1 1, 0")
            return block_prelude

        def match_pattern(value: BlockCodegen.Value, pattern: MatchPattern,
                          id: int) -> t.List[Expression]:
            if pattern.literal is not None:
                arm_value = self.generate_literal(pattern.literal)
                match arm_value.type:
                    case BuiltinTypes.str_:
                        string_match(value, arm_value, id)
                    case BuiltinTypes.f32:
                        float_match(value, arm_value, id)
                    case BuiltinTypes.i32 | BuiltinTypes.bool_:
                        int_type_match(value, arm_value, id)
                return []
            return struct_match(value, pattern, id)

        for arm in match.arms:
            scope_prelude = match_pattern(value, arm.pattern, id)
            self.emit(
                f"br i1 %match_{id}, label %match_{id}_matches, label %match_{id}_else"
            )
            self.emit(f"match_{id}_matches:")
            arm.block.expressions = scope_prelude + arm.block.expressions
            self.generate_block(arm.block)
            self.indent += 1
            self.emit(f"br label %{end_label}")
            self.indent -= 1
            self.emit(f"match_{id}_else:")
            id = self.codegen.next_id()
        if match.default_arm:
            self.generate_block(match.default_arm)
            self.indent += 1
            self.emit(f"br label %{end_label}")
            self.indent -= 1
        else:
            self.emit(f"br label %{end_label}")
        self.emit(f"{end_label}:")
        # TODO: If the match expression is used as a value, we need to return
        #       the value of the block that was executed.
        return BlockCodegen.Value("void", BuiltinTypes.void)

    def find_function(self, name: str, span: Span) -> FunctionDefinition:
        if "." in name:
            namespace_name, function_name = name.split(".")
            namespace = self.find_namespace(namespace_name, span)
            if namespace is None:
                raise ValueError(f"Unknown namespace: {namespace_name}")
            function = namespace.functions.get(function_name)
            if not function:
                raise ValueError(
                    f"Unknown function: {namespace_name}.{function_name}")
            return function
        func = self.functions.get(name)
        if func:
            return func
        if not self.parent:
            raise ValueError(f"Unknown function: {name} at {span}")
        return self.parent.find_function(name, span)

    def find_variable(self, name: str, span: Span) -> Variable:
        variable = self.variables.get(name)
        if variable:
            return variable
        if not self.parent:
            raise ValueError(f"Unknown variable: {name} at {span}")
        return self.parent.find_variable(name, span)

    def find_struct(self, name: str, span: Span) -> Struct:
        struct = self.structs.get(name)
        if struct:
            return struct
        if not self.parent:
            raise ValueError(f"Unknown struct: {name} at {span}")
        return self.parent.find_struct(name, span)

    def find_enum(self, name: str, span: Span) -> Enum:
        enum = self.enums.get(name)
        if enum:
            return enum
        if not self.parent:
            raise ValueError(f"Unknown enum: {name} at {span}")
        return self.parent.find_enum(name, span)

    def find_namespace(self, name: str, span: Span) -> Namespace:
        namespace = self.namespaces.get(name)
        if namespace:
            return namespace
        if not self.parent:
            raise ValueError(f"Unknown namespace: {name} at {span}")
        return self.parent.find_namespace(name, span)


class Codegen:

    llvm_ir_prelude = """
declare i32 @puts(i8*) 
declare i32 @sprintf(i8*, i8*, ...)
declare i32 @printf(i8*, ...)
declare i32 @strcmp(i8*, i8*)
declare i8* @strcat(i8*, i8*)
declare i32 @strlen(i8*)
declare i8* @calloc(i64, i64)
declare i32 @memset(i8*, i32, i32)
declare i32 @getchar()

@i32_format = internal constant [3 x i8] c"%d\00"
@f32_format = internal constant [3 x i8] c"%f\00"
@true_str = internal constant [5 x i8] c"true\00"
@false_str = internal constant [6 x i8] c"false\00"

@debug_format_int = constant [8 x i8] c"deb:%d\n\00"

define void @print(i8* %str) {
  %1 = call i32 @puts(i8* %str)
  ret void
}

define i8* @chr(i32 %i) {
    %1 = call i8* @calloc(i64 2, i64 1)
    %2 = trunc i32 %i to i8
    store i8 %2, i8* %1
    ret i8* %1
}
    """

    def __init__(self, program: Block):
        self.program = program
        self.literals: t.Dict[t.Union[str, int, bool, float], int] = {}
        self.id_counter = 0

    def next_id(self) -> int:
        self.id_counter += 1
        return self.id_counter

    def generate(self) -> str:
        code = ""
        program_codegen = BlockCodegen(self, self.program)
        self.register_builtin_functions(program_codegen)
        program_codegen.generate()
        target_triple = subprocess.check_output("llvm-config --host-target",
                                                shell=True).decode().strip()
        code += f'target triple = "{target_triple}"'
        code += "\n\n; Prelude\n"
        code += self.llvm_ir_prelude.strip()
        code += "\n\n; Program\n"
        code += "\n".join(program_codegen.code)
        code += "\n\n; Literals\n"
        code += "\n".join(self.emit_literal_constants())
        return code

    def register_builtin_functions(self, block: BlockCodegen):
        block.functions["print"] = FunctionDefinition(
            name="print",
            span=Span(-1, -1),
            return_type=BuiltinTypes.i32,
            parameters=[Parameter(Span(-1, -1), "str", BuiltinTypes.str_)],
            body=Block(Span(-1, -1), []))
        block.functions["getchar"] = FunctionDefinition(
            name="getchar",
            span=Span(-1, -1),
            return_type=BuiltinTypes.i32,
            parameters=[],
            body=Block(Span(-1, -1), []))
        block.functions["chr"] = FunctionDefinition(
            name="chr",
            span=Span(-1, -1),
            return_type=BuiltinTypes.str_,
            parameters=[Parameter(Span(-1, -1), "i", BuiltinTypes.i32)],
            body=Block(Span(-1, -1), []))

    def literal_const(self, literal: Literal) -> str:
        literal_const = self.literals.get(
            literal.value) or (len(self.literals) + 1)
        self.literals[literal.value] = literal_const
        return f"@const_{literal_const}"

    def emit_literal_constants(self):
        literals = []
        for value, name in self.literals.items():
            match value:
                case str():
                    value_len = len(value) + 1
                    value = value.replace('"', '\\22')
                    literals.append(
                        f"@const_{name} = internal constant [{value_len} x i8] c\"{value}\\00\""
                    )
        return literals


def main():
    debug = any(x == "--debug" for x in sys.argv)
    print_to_stdout = any(x == "--stdout" for x in sys.argv)
    src_file = sys.argv[-2]
    bin_file = sys.argv[-1]
    ll_file = os.path.join(
        "build",
        os.path.splitext(os.path.basename(src_file))[0] + ".ll")
    obj_file = ll_file.replace(".ll", ".o")
    src = open(src_file).read()
    lexer = Lexer(src)
    lexer.parse()
    if debug:
        print("Tokens:")
        pprint(lexer.tokens)
    parser = Parser(lexer.tokens)
    ast = parser.parse()
    if debug:
        print("AST:")
        pprint(ast)
    codegen = Codegen(ast)
    code = codegen.generate()
    if not os.path.exists("build"):
        os.mkdir("build")
    if print_to_stdout:
        print(code)
    with open(ll_file, "w") as f:
        f.write(code)
    subprocess.check_call(
        f"llc -O=0 -opaque-pointers -filetype=obj {ll_file} -o {obj_file}",
        shell=True)
    subprocess.check_call(f"gcc -O0 {obj_file} -o {bin_file}", shell=True)


if __name__ == "__main__":
    main()
