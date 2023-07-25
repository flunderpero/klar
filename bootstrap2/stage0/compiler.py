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
    newline = auto()
    dot = auto()
    colon = auto()
    lparen = auto()
    rparen = auto()
    lbracket = auto()
    rbracket = auto()
    lbrace = auto()
    rbrace = auto()
    langle_bracket = auto()
    rangle_bracket = auto()
    equal = auto()
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
    comment = auto()


class Keyword(StrEnum):
    let = "let"
    mut = "mut"
    if_ = "if"
    else_ = "else"
    end = "end"
    fn = "fn"
    return_ = "return"


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

    def emit_newline(self):
        self.emit_single(TokenKind.newline)
        self.line += 1
        self.column = 0

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
                            self.emit_single(TokenKind.equal)
                        case "\n":
                            self.emit_newline()
                        case c if c.isspace():
                            self.state = ValueToken(TokenKind.whitespace,
                                                    self.span(), c)
                        case c if c.isnumeric():
                            self.state = ValueToken(TokenKind.integer,
                                                    self.span(), c)
                        case c if c.isalpha():
                            self.state = ValueToken(TokenKind.identifier,
                                                    self.span(), c)
                        case _:
                            raise ValueError(f"Unexpected character: {c}")
                case StringToken(TokenKind.string, _):
                    if c == self.state.quote:
                        self.emit(advance=True)
                    else:
                        self.state.value += c
                case ValueToken(TokenKind.whitespace, _):
                    if c == "\n":
                        self.emit()
                        self.emit_newline()
                    elif c.isspace():
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
                        if self.state.value in Keyword.__members__:
                            self.state = KeywordToken(
                                TokenKind.keyword, self.state.span,
                                Keyword(self.state.value))
                        self.emit()
                case _:
                    raise ValueError(f"Unexpected state: {self.state}")


@dataclass
class Type():
    name: str

    def __repr__(self):
        return self.name


class BuiltinTypes:
    i32 = Type("i32")
    f32 = Type("f32")
    i8_ptr = Type("i8*")
    void = Type("void")


@dataclass
class Node:
    pass


@dataclass
class Expression(Node):
    pass


@dataclass
class Literal(Expression):
    value: t.Union[int, float, str]


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
    call_name: str
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


class Parser:

    def __init__(self, tokens: t.List[Token]):
        self.tokens = tokens
        self.idx = 0

    def token(self):
        token = None
        while self.idx < len(self.tokens):
            token = self.tokens[self.idx]
            if token.kind == TokenKind.whitespace:
                self.idx += 1
            else:
                break
        self.idx += 1
        return token

    def parse(self):
        return self.parse_program()

    def parse_program(self):
        expressions = []
        while True:
            token = self.token()
            if not token:
                break
            if token.kind == TokenKind.newline:
                continue
            expressions.append(self.parse_expression(token))
        return Block(expressions)

    def parse_expression(self, token: t.Optional[Token]):
        if not token:
            raise ValueError("Unexpected end of tokens")
        match token:
            case KeywordToken(TokenKind.keyword, _, Keyword.fn):
                return self.parse_function_definition()
            case ValueToken(TokenKind.identifier, _):
                next_token = self.token()
                match next_token:
                    case Token(TokenKind.lparen):
                        return self.parse_function_call(token.value)
                    case Token(TokenKind.equal):
                        value = self.parse_expression(self.token())
                        return VariableWrite(token.value, value)
                    case _:
                        self.idx -= 1
                        return VariableRead(token.value)
            case ValueToken(TokenKind.integer, _, value):
                return Literal(int(value))
            case ValueToken(TokenKind.float_, _, value):
                return Literal(float(value))
            case ValueToken(TokenKind.string, _, value):
                return Literal(value)
            case KeywordToken(TokenKind.keyword, _, Keyword.let):
                return self.parse_variable_definition(mutable=False)
            case KeywordToken(TokenKind.keyword, _, Keyword.mut):
                return self.parse_variable_definition(mutable=True)
            case _:
                raise ValueError(f"Unexpected token: {token}")

    def parse_variable_definition(self, mutable: bool):
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
                return VariableDefinition(name, mutable, None, expression)

    def parse_function_definition(self):
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
                token = self.token()
                type_ = BuiltinTypes.void
                match token:
                    case Token(TokenKind.colon, _):
                        pass
                    case ValueToken(TokenKind.identifier, _, type_):
                        body = self.parse_block()
                        type_ = Type(type_)
                    case _:
                        raise ValueError(f"Unexpected token: {token}")
                body = self.parse_block()
                if name == "main":
                    # `main` functions must return `int`.
                    body.expressions.append(ReturnExpression(Literal(0)))
                return FunctionDefinition(name, name, parameters, type_, body)
            case _:
                raise ValueError(f"Expected identifier: {token}")

    def parse_parameters(self):
        parameters = []
        while True:
            token = self.token()
            if not token:
                raise ValueError("Expected ')'")
            match token:
                case Token(TokenKind.rparen):
                    break
                case ValueToken(TokenKind.identifier, _, name):
                    type_ = self.token()
                    match type_:
                        case ValueToken(TokenKind.identifier, type_):
                            parameters.append(
                                Parameter(name, Type(type_.value)))
                        case _:
                            raise ValueError(f"Expected type: {type_}")
                case _:
                    raise ValueError(f"Unexpected token: {token}")
        return parameters

    def parse_block(self):
        expressions = []
        while True:
            token = self.token()
            if not token:
                break
            match token:
                case KeywordToken(TokenKind.keyword, _, Keyword.end):
                    return Block(expressions)
                case Token(TokenKind.newline):
                    continue
                case _:
                    expressions.append(self.parse_expression(token))
        raise ValueError("Expected end")

    def parse_function_call(self, name):
        arguments = []
        while True:
            token = self.token()
            if not token:
                break
            match token:
                case Token(TokenKind.rparen):
                    return FunctionCall(name, arguments)
                case _:
                    arguments.append(self.parse_expression(token))


class BlockCodegen:

    @dataclass
    class Value:
        name: str
        type: Type

        def __repr__(self):
            return f"{self.type} {self.name}"

    def __init__(self,
                 codegen: "Codegen",
                 block: Block,
                 parent: t.Optional["BlockCodegen"] = None):
        self.codegen = codegen
        self.block = block
        self.parent = parent
        self.code: t.List[str] = []
        self.indent: int = 1 if parent else 0
        self.literals: t.Dict[t.Union[str, int, float], int] = {}
        self.functions: t.Dict[str, FunctionDefinition] = {}
        self.variables: t.Dict[str, VariableDefinition] = {}

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
            case Block():
                return self.generate_block(expression)
            case FunctionDefinition():
                return self.generate_function_definition(expression)
            case VariableDefinition():
                return self.generate_variable_definition(expression)
            case VariableRead():
                return self.generate_variable_read(expression)
            case VariableWrite():
                return self.generate_variable_write(expression)
            case FunctionCall():
                return self.generate_function_call(expression)
            case ReturnExpression():
                return self.generate_return_expression(expression)
            case _:
                raise ValueError(f"Unknown expression: {expression}")

    def generate_literal(self, literal: Literal) -> Value:
        match literal.value:
            case int():
                return self.Value(str(literal.value), BuiltinTypes.i32)
            case float():
                return self.Value(str(literal.value), BuiltinTypes.f32)
            case str():
                literal_const = self.codegen.literal_const(literal)
                return self.Value(
                    f"getelementptr ([{len(literal.value) + 1} x i8], ptr {literal_const}, i32 0, i32 0)",
                    BuiltinTypes.i8_ptr,
                )

    def generate_block(self, block: Block) -> Value:
        codegen = BlockCodegen(self.codegen, block, self)
        reg = codegen.generate()
        for line in codegen.code:
            self.emit(line)
        return reg

    def generate_function_definition(self, func: FunctionDefinition) -> Value:
        if func.name == "main":
            self.emit("define i32 @main() {")
        self.generate_block(func.body)
        self.emit("}")
        self.functions[func.name] = func
        return self.Value("void", BuiltinTypes.void)

    def generate_return_expression(self, return_: ReturnExpression) -> Value:
        reg = self.generate_expression(return_.expression)
        self.emit(f"ret {reg}")
        return reg

    def generate_function_call(self, call: FunctionCall) -> Value:
        arument_values = [self.generate_expression(x) for x in call.arguments]
        func = self.find_function(call.name)
        res = self.Value("void", BuiltinTypes.void)
        code = ""
        if func.return_type.name != "void":
            res = self.Value(f"%{self.codegen.next_id()}", func.return_type)
            code += f"{res.name} = "
        code += f"call {func.return_type} @{func.call_name}("
        code += ", ".join([f"{x}" for x in arument_values])
        code += ")"
        self.emit(code)
        return self.Value("void", BuiltinTypes.void)

    def generate_variable_definition(self,
                                     variable: VariableDefinition) -> Value:
        res = self.generate_expression(variable.value)
        variable.type = res.type
        self.emit(f"%{variable.name} = alloca {res.type}")
        self.emit(f"store {res}, {res.type}* %{variable.name}")
        self.variables[variable.name] = variable
        return res

    def generate_variable_read(self, variable_read: VariableRead) -> Value:
        variable = self.find_variable(variable_read.name)
        if variable.type is None:
            raise ValueError(f"Unknown variable type: {variable_read.name}")
        res = self.Value(f"%{self.codegen.next_id()}", variable.type)
        self.emit(
            f"{res.name} = load {variable.type}, {variable.type}* %{variable.name}"
        )
        return res

    def generate_variable_write(self, variable_write: VariableWrite) -> Value:
        variable = self.find_variable(variable_write.name)
        if variable.type is None:
            raise ValueError(f"Unknown variable type: {variable_write.name}")
        if not variable.mutable:
            raise ValueError(f"Variable is not mutable: {variable_write.name}")
        res = self.generate_expression(variable_write.value)
        self.emit(f"store {res}, {variable.type}* %{variable.name}")
        return res

    def find_function(self, name: str) -> FunctionDefinition:
        func = self.functions.get(name)
        if func:
            return func
        if not self.parent:
            raise ValueError(f"Unknown function: {name}")
        return self.parent.find_function(name)

    def find_variable(self, name: str) -> VariableDefinition:
        variable = self.variables.get(name)
        if variable:
            return variable
        if not self.parent:
            raise ValueError(f"Unknown variable: {name}")
        return self.parent.find_variable(name)


class Codegen:

    llvm_ir_prelude = """
declare i32 @puts(i8*) 
    """

    def __init__(self, program: Block):
        self.program = program
        self.literals: t.Dict[t.Union[str, int, float], int] = {}
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
            call_name="puts",
            return_type=Type("i32"),
            parameters=[Parameter("str", Type("i8*"))],
            body=Block([]))

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
