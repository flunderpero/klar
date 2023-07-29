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
    single_quote = auto()
    double_quote = auto()
    keyword = auto()
    identifier = auto()
    string = auto()
    integer = auto()
    float_ = auto()
    bool_ = auto()
    comment = auto()


class Keyword(StrEnum):
    let = "let"
    mut = "mut"
    if_ = "if"
    else_ = "else"
    end = "end"
    fn = "fn"
    struct = "struct"
    enum = "enum"
    return_ = "return"
    false = "false"
    true = "true"


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


class Lexer:

    def __init__(self, src: str):
        self.src = src
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
                case StringToken(TokenKind.string, _):
                    if c == self.state.quote:
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
                    else:
                        if self.state.value in Keyword.__members__.values():
                            self.state = KeywordToken(
                                TokenKind.keyword, self.state.span,
                                Keyword(self.state.value))
                        self.emit()
                case _:
                    raise ValueError(f"Unexpected state: {self.state}")


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
    f32 = Type("f32", "f32", 4)
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


@dataclass
class FieldWrite(Expression):
    name: str
    field_path: t.List[str]
    value: Expression


@dataclass
class EnumDefinition(Expression):
    name: str
    variants: t.List[StructDefinition]

    def size(self):
        return max(x.size() for x in self.variants)

    def type(self):
        return Type(self.name, f"%{self.name}*", self.size())


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
            case KeywordToken(TokenKind.keyword, _, Keyword.if_):
                expression = self.parse_if_expression()
            case KeywordToken(TokenKind.keyword, _, Keyword.mut):
                expression = self.parse_variable_definition(mutable=True)
            case KeywordToken(TokenKind.keyword, _, Keyword.return_):
                expression = self.parse_return_expression(token)
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
        self.idx -= 1
        return None

    def parse_if_expression(self) -> IfExpression:
        condition = self.parse_expression(self.token())
        then, then_end_keyword = self.parse_block()
        if then_end_keyword == Keyword.else_:
            else_, _ = self.parse_block()
            span = condition.span.merge(else_.span)
        else:
            span = condition.span.merge(then.span)
            else_ = None
        return IfExpression(span, condition, then, else_)

    def parse_type(self, type: str) -> Type:
        builtin_type = next((t for t in BuiltinTypes.__dict__.values()
                             if isinstance(t, Type) and t.klar_name == type),
                            None)
        if builtin_type:
            return builtin_type
        # This must be a user-defined type.
        struct = next((x for x in self.structs if x.name == type), None)
        if struct:
            return struct.type()
        enum = next((x for x in self.enums if x.name == type), None)
        if not enum:
            raise ValueError(f"Unknown type: {type}")
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
                    match type_token:
                        case ValueToken(TokenKind.identifier, _, _):
                            fields.append(
                                Field(token.span.merge(type_token.span), name,
                                      self.parse_type(type_token.value)))
                        case _:
                            raise ValueError(f"Expected type: {type_token}")
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
                                      self.parse_type(type_token.value)))
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
                        return_type = self.parse_type(type_token.value)
                    case _:
                        return_type = BuiltinTypes.void
                        self.idx -= 1
                body, body_block_end = self.parse_block()
                if body_block_end != Keyword.end:
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
                                          self.parse_type(type_token.value)))
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
                                          self.parse_type(type_token.value)))
                        case _:
                            raise ValueError(f"Expected type: {type_token}")
                case _:
                    raise ValueError(f"Unexpected token: {token}")
        return parameters

    def parse_block(self) -> t.Tuple[Block, Keyword]:
        expressions: t.List[Expression] = []
        span_start = Span(-1, -1)
        span_end = Span(-1, -1)
        token = self.token()
        if token is None:
            raise ValueError("Unexpected EOF")
        if token.kind != TokenKind.colon:
            raise ValueError(f"Expected ':': {token}")
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
                 parent: t.Optional["BlockCodegen"] = None):
        self.codegen = codegen
        self.block = block
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
        for node in self.block.expressions:
            self.generate_expression(node)
        return self.Value("void", BuiltinTypes.void)

    def generate_expression(self, expression) -> Value:
        match expression:
            case Literal():
                return self.generate_literal(expression)
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

    def generate_void(self, void: Void) -> Value:
        return self.Value("", BuiltinTypes.void)

    def generate_block(self, block: Block) -> Value:
        codegen = BlockCodegen(self.codegen, block, self)
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
        self.emit(f"%alloc = call i8* @malloc(i64 %size)")
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
        self.emit(f"%alloc = call i8* @malloc(i64 %size)")
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
        var = self.generate_variable_read(
            VariableRead(field_read.span, field_read.name))
        if var.type == BuiltinTypes.fn:
            return var
        struct_name = var.type.klar_name
        if var.type.enum_variant:
            struct_name = f"{struct_name}.{var.type.enum_variant}"
        struct = self.find_struct(struct_name, field_read.span)
        for field_name in field_read.field_path[:-1]:
            field = next(
                (x for x in struct.definition.fields if x.name == field_name),
                None)
            if field is None:
                raise ValueError(
                    f"Struct {struct_name} does not have a field named {field_name}",
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
                f"Struct {struct_name} does not have a field named {field_name}",
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
        variable.type = res.type
        self.emit(f"%{variable.name} = alloca {res.type}")
        self.emit(f"store {res}, {res.type}* %{variable.name}")
        self.variables[variable.name] = self.Variable(span=variable.span,
                                                      name=variable.name,
                                                      mutable=True,
                                                      type=res.type)
        return res

    def generate_variable_read(self, variable_read: VariableRead) -> Value:
        variable = self.find_variable(variable_read.name, variable_read.span)
        if variable.type is None:
            raise ValueError(f"Unknown variable type: {variable_read.name}")
        if variable.is_parameter:
            return self.Value(f"%{variable.name}", variable.type)
        res = self.Value(f"%.{self.codegen.next_id()}", variable.type)
        self.emit(
            f"{res.name} = load {variable.type}, {variable.type}* %{variable.name}"
        )
        return res

    def generate_variable_write(self, variable_write: VariableWrite) -> Value:
        variable = self.find_variable(variable_write.name, variable_write.span)
        if variable.type is None:
            raise ValueError(f"Unknown variable type: {variable_write.name}")
        if not variable.mutable:
            raise ValueError(f"Variable is not mutable: {variable_write.name}")
        res = self.generate_expression(variable_write.value)
        self.emit(f"store {res}, {variable.type}* %{variable.name}")
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
            op_name = "icmp eq"
            res.type = BuiltinTypes.bool_
        self.emit(f"{res.name} = {op_name} {left}, {right.name}")
        return res

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
declare i8* @malloc(i64)
declare i32 @memset(i8*, i32, i32)

define void @print(i8* %str) {
  %1 = call i32 @puts(i8* %str)
  ret void
}

define i8* @format(i8* %fmt, i32 %i) {
  %1 = call i8* @malloc(i64 64)
  %2 = call i8* @memset(i8* %1, i32 0, i32 64)
  %3 = call i32 (i8*, i8*, ...) @sprintf(i8* %1, i8* %fmt, i32 %i)
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
        block.functions["format"] = FunctionDefinition(
            name="format",
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
                    literals.append(
                        f"@const_{name} = internal constant [{len(value) + 1} x i8] c\"{value}\\00\""
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
    subprocess.check_call(f"clang -O0 {obj_file} -o {bin_file}", shell=True)


if __name__ == "__main__":
    main()
