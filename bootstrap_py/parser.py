from __future__ import annotations

from dataclasses import dataclass
from typing import Callable

from . import ast, error
from . import tokenizer as token
from .span import FQN, Span


@dataclass
class Input:
    tokens: list[token.Token]
    next_id: Callable[[], int]
    index: int = 0

    def next(self) -> token.Token:
        """Return the next token while skipping comments."""
        t = self.peek()
        if t.kind != token.Kind.eof:
            self.index += 1
        return t

    def peek(self) -> token.Token:
        """Return the next token without consuming it. Skips comments."""
        while self.tokens[self.index].kind == token.Kind.comment:
            self.index += 1
        return self.tokens[self.index]

    def span(self) -> Span:
        return self.peek().span

    def span_merge(self, span: Span) -> Span:
        return span.merge(self.span())


class Parser:
    input: Input
    errors: list[error.Error]

    def __init__(self, input: Input) -> None:
        self.input = input
        self.errors = []

    def error(self, err: error.Error) -> None:
        """Record the error and find the next best place to resume parsing."""
        self.errors.append(err)
        while (t := self.input.next()).kind != token.Kind.eof:
            match t.kind:
                case token.Kind.paren_right | token.Kind.curly_right:
                    break
                case token.Kind.fn:
                    break

    def expect_ident(self) -> str | None:
        t = self.input.next()
        if t.kind == token.Kind.ident:
            return t.value_str()
        self.error(error.unexpected_token(t.span, t.kind.value, token.Kind.ident.value))
        return None

    def expect(self, *kind: token.Kind) -> token.Token | None:
        t = self.input.next()
        if t.kind in kind:
            return t
        self.error(error.unexpected_token(t.span, t.kind.value, *(x.value for x in kind)))
        return None

    def id(self) -> ast.NodeId:
        return self.input.next_id()

    def parse_type(self) -> ast.Type | None:
        t = self.input.next()
        if t.kind == token.Kind.type_ident:
            return ast.Type(self.id(), t.value_str(), t.span)
        self.error(error.unexpected_token(t.span, t.kind.value, token.Kind.type_ident.value))
        return None

    def parse_type_params(self) -> ast.TypeParams | None:
        if self.input.peek().kind != token.Kind.lt:
            return []
        self.input.next()
        if self.input.peek().kind == token.Kind.gt:
            self.error(error.unexpected_token(self.input.span(), token.Kind.gt.value, token.Kind.type_ident.value))
            return None
        res: ast.TypeParams = []
        while True:
            t = self.input.peek()
            if t.kind == token.Kind.type_ident:
                self.input.next()
                res.append(ast.TypeParam(t.value_str(), t.span))
                match self.input.peek().kind:
                    case token.Kind.comma:
                        self.input.next()
                    case token.Kind.gt:
                        self.input.next()
                        break
                    case _:
                        self.error(
                            error.unexpected_token(t.span, t.kind.value, token.Kind.comma.value, token.Kind.gt.value)
                        )
                        return None
            else:
                self.error(error.unexpected_token(t.span, t.kind.value, token.Kind.type_ident.value))
                return None
        return res

    def parse_type_args(self) -> ast.TypeArgs | None:
        if self.input.peek().kind != token.Kind.lt:
            return []
        self.input.next()
        if self.input.peek().kind == token.Kind.gt:
            self.error(error.unexpected_token(self.input.span(), token.Kind.gt.value, token.Kind.type_ident.value))
            return None
        res: ast.TypeArgs = []
        while True:
            typ = self.parse_type()
            if not typ:
                return None
            res.append(typ)
            t = self.input.peek()
            match t.kind:
                case token.Kind.comma:
                    self.input.next()
                case token.Kind.gt:
                    self.input.next()
                    break
                case _:
                    self.error(
                        error.unexpected_token(t.span, t.kind.value, token.Kind.comma.value, token.Kind.gt.value)
                    )
                    return None
        return res

    def parse_fn_decl(self) -> ast.FnDecl | None:
        span = self.input.span()
        if not self.expect(token.Kind.fn):
            return None
        name = self.expect_ident()
        if not name:
            return None
        type_params = self.parse_type_params()
        if type_params is None:
            return None
        if not self.expect(token.Kind.paren_left):
            return None
        params = []
        while True:
            t = self.input.peek()
            match t.kind:
                case token.Kind.ident:
                    self.input.next()
                    param_name = t.value_str()
                    existing = next((x for x in params if x.name == param_name), None)
                    if existing:
                        self.error(error.duplicate_param_name(param_name, t.span, existing.span))
                        return None
                    param_type = self.parse_type()
                    if not param_type:
                        return None
                    params.append(ast.FieldOrParam(param_name, param_type, t.span.merge(param_type.span)))
                    match self.input.peek().kind:
                        case token.Kind.comma:
                            self.input.next()
                        case token.Kind.paren_right:
                            break
                        case _:
                            self.error(
                                error.unexpected_token(
                                    t.span, t.kind.value, token.Kind.comma.value, token.Kind.paren_right.value
                                )
                            )
                            return None
                case token.Kind.paren_right:
                    break
                case _:
                    self.error(
                        error.unexpected_token(
                            t.span, t.kind.value, token.Kind.ident.value, token.Kind.paren_right.value
                        )
                    )
                    return None

        if not self.expect(token.Kind.paren_right):
            return None
        result: ast.Type | None = None
        if self.input.peek().kind == token.Kind.type_ident:
            result = self.parse_type()

        return ast.FnDecl(self.id(), name, params, result, type_params, self.input.span_merge(span))

    def parse_fn_def(self) -> ast.FnDef | None:
        span = self.input.span()
        fn_decl = self.parse_fn_decl()
        if not fn_decl:
            return None
        body = self.parse_block()
        if not body:
            return None
        return ast.FnDef(self.id(), fn_decl, body, self.input.span_merge(span))

    def parse_ident(self) -> ast.Ident | None:
        span = self.input.span()
        ident = self.expect(token.Kind.ident, token.Kind.type_ident)
        if not ident:
            return None
        type_args = self.parse_type_args()
        if type_args is None:
            return None
        return ast.Ident(self.id(), ident.value_str(), type_args, self.input.span_merge(span))

    def parse_call(self, callee: ast.Expr) -> ast.Call | None:
        if not self.expect(token.Kind.paren_left):
            return None
        args: list[ast.Expr] = []
        while self.input.peek().kind != token.Kind.paren_right:
            expr = self.parse_expr()
            if not expr:
                return None
            args.append(expr)
            t = self.input.peek()
            match t.kind:
                case token.Kind.comma:
                    self.input.next()
                case token.Kind.paren_right:
                    break
                case _:
                    self.error(
                        error.unexpected_token(
                            t.span, t.kind.value, token.Kind.comma.value, token.Kind.paren_right.value
                        )
                    )
                    return None
        self.input.next()
        return ast.Call(self.id(), callee, args, self.input.span_merge(callee.span))

    def parse_member(self, target: ast.Expr) -> ast.Member | None:
        if not self.expect(token.Kind.dot):
            return None
        name = self.expect_ident()
        if not name:
            return None
        type_args = self.parse_type_args()
        if type_args is None:
            return None
        return ast.Member(self.id(), target, name, type_args, self.input.span_merge(target.span))

    def parse_if(self) -> ast.If | None:
        span = self.input.span()
        if not self.expect(token.Kind.if_):
            return None
        cond = self.parse_expr()
        if not cond:
            return None
        then_block = self.parse_block()
        if not then_block:
            return None
        else_block: ast.Block | None = None
        if self.input.peek().kind == token.Kind.else_:
            self.input.next()
            else_block = self.parse_block()
            if not else_block:
                return None
        return ast.If(self.id(), cond, then_block, else_block, self.input.span_merge(span))

    def parse_loop(self) -> ast.Loop | None:
        span = self.input.span()
        if not self.expect(token.Kind.loop):
            return None
        block = self.parse_block()
        if not block:
            return None
        return ast.Loop(self.id(), block, self.input.span_merge(span))

    def parse_expr(self, min_precedence: int = 0) -> ast.Expr | None:
        lhs = self.parse_primary_expr()
        if not lhs:
            return None
        while True:
            t = self.input.peek()
            op_by_token = {
                token.Kind.plus: ast.BinaryOp.add,
                token.Kind.minus: ast.BinaryOp.sub,
                token.Kind.eqeq: ast.BinaryOp.eq,
                token.Kind.neq: ast.BinaryOp.ne,
            }
            precendence_by_op = {ast.BinaryOp.eq: 1, ast.BinaryOp.ne: 1, ast.BinaryOp.add: 2, ast.BinaryOp.sub: 2}
            op = op_by_token.get(t.kind)
            if not op:
                return lhs
            precedence = precendence_by_op[op]
            if precedence < min_precedence:
                return lhs
            self.input.next()
            # `+ 1` because all of our binary operators are left associative, i.e. `1 + 3 - 4`
            # becomes `(1 + 3) - 4`.
            rhs = self.parse_expr(precedence + 1)
            if not rhs:
                return None
            lhs = ast.BinaryExpr(self.id(), op, lhs, rhs, self.input.span_merge(lhs.span))

    def parse_primary_expr(self) -> ast.Expr | None:
        t = self.input.peek()
        expr: ast.Expr | None
        match t.kind:
            case token.Kind.curly_left | token.Kind.fat_arrow:
                return self.parse_block()
            case token.Kind.ident | token.Kind.type_ident:
                expr = self.parse_ident()
            case token.Kind.str_lit:
                self.input.next()
                return ast.StrLit(self.id(), t.value_str(), t.span)
            case token.Kind.int_lit:
                self.input.next()
                return ast.IntLit(self.id(), bits=64, signed=True, value=int(t.value_str()), span=t.span)
            case token.Kind.true | token.Kind.false:
                self.input.next()
                return ast.BoolLit(self.id(), value=t.kind == token.Kind.true, span=t.span)
            case token.Kind.if_:
                return self.parse_if()
            case _:
                self.error(error.unexpected_token(t.span, t.kind.value))
                return None
        if not expr:
            return None
        match self.input.peek().kind:
            case token.Kind.paren_left:
                expr = self.parse_call(expr)
            case token.Kind.dot:
                expr = self.parse_member(expr)
        return expr

    def parse_let_or_mut(self) -> ast.Let | None:
        span = self.input.span()
        keyword = self.expect(token.Kind.let, token.Kind.mut)
        if not keyword:
            return None
        name = self.expect_ident()
        if not name:
            return None
        if not self.expect(token.Kind.eq):
            return None
        value = self.parse_expr()
        if not value:
            return None
        return ast.Let(
            self.id(), name, None, value, self.input.span_merge(span), mutable=keyword.kind == token.Kind.mut
        )

    def parse_struct(self) -> ast.Struct | None:
        span = self.input.span()
        if not self.expect(token.Kind.struct):
            return None
        name = self.expect(token.Kind.type_ident)
        if not name:
            return None
        type_params = self.parse_type_params()
        if type_params is None:
            return None
        if not self.expect(token.Kind.curly_left):
            return None
        fields: list[ast.FieldOrParam] = []
        while self.input.peek().kind != token.Kind.curly_right:
            param_span = self.input.span()
            param_name = self.expect_ident()
            if not param_name:
                return None
            param_type = self.parse_type()
            if not param_type:
                return None
            fields.append(ast.FieldOrParam(param_name, param_type, self.input.span_merge(param_span)))
            match self.input.peek():
                case token.Kind.comma:
                    self.input.next()
                case token.Kind.curly_right:
                    break
        if not self.expect(token.Kind.curly_right):
            return None
        return ast.Struct(self.id(), name.value_str(), fields, type_params, self.input.span_merge(span))

    def parse_block(self) -> ast.Block | None:
        span = self.input.span()
        entry_token = self.expect(token.Kind.curly_left, token.Kind.fat_arrow)
        if not entry_token:
            return None
        nodes: list[ast.Node] = []
        if entry_token.kind == token.Kind.fat_arrow:
            node = self.parse_block_node()
            if not node:
                return None
            nodes.append(node)
        else:
            while (t := self.input.peek()).kind != token.Kind.eof:
                if t.kind == token.Kind.curly_right:
                    self.input.next()
                    break
                node = self.parse_block_node()
                if node:
                    nodes.append(node)
        return ast.Block(self.id(), nodes, self.input.span_merge(span))

    def parse_block_node(self) -> ast.Node | None:
        t = self.input.peek()
        match t.kind:
            case token.Kind.fn:
                return self.parse_fn_def()
            case token.Kind.let | token.Kind.mut:
                return self.parse_let_or_mut()
            case token.Kind.loop:
                return self.parse_loop()
            case token.Kind.break_:
                span = self.input.next().span
                return ast.Break(self.id(), span)
            case token.Kind.continue_:
                span = self.input.next().span
                return ast.Continue(self.id(), span)
            case token.Kind.struct:
                return self.parse_struct()
            case _:
                expr = self.parse_expr()
                if not expr:
                    return None
                if self.input.peek().kind == token.Kind.eq:
                    self.input.next()
                    if not isinstance(expr, ast.Ident):
                        self.error(error.expected_ident(str(expr), t.span))
                        return None
                    value = self.parse_expr()
                    if not value:
                        return None
                    return ast.Assign(self.id(), expr, value, self.input.span_merge(expr.span))
                return expr

    def parse_module(self) -> ast.Module:
        span = self.input.span()
        nodes: list[ast.Node] = []
        while t := self.input.peek():
            if t.kind == token.Kind.eof:
                break
            node = self.parse_block_node()
            if node:
                nodes.append(node)
        module_name = span.file.split("/")[-1].split(".")[0]
        return ast.Module(self.id(), FQN([module_name]), nodes, self.input.span_merge(span))


def parse(tokens: Input) -> tuple[ast.Module, list[error.Error]]:
    parser = Parser(tokens)
    return (parser.parse_module(), parser.errors)
