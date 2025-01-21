from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Callable

from . import ast, error
from . import tokenizer as token

if TYPE_CHECKING:
    from .span import Span


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
        self.error(error.unexpected_token(t.span, t.kind.name, token.Kind.ident.name))
        return None

    def expect(self, kind: token.Kind) -> bool:
        t = self.input.next()
        if t.kind == kind:
            return True
        self.error(error.unexpected_token(t.span, t.kind.name, kind.name))
        return False

    def id(self) -> ast.NodeId:
        return self.input.next_id()

    def parse_type(self) -> ast.Type | None:
        t = self.input.next()
        if t.kind == token.Kind.type_ident:
            return ast.Type(self.id(), t.value_str(), t.span)
        self.error(error.unexpected_token(t.span, t.kind.name, token.Kind.type_ident.name))
        return None

    def parse_fn_decl(self) -> ast.FnDecl | None:
        span = self.input.span()
        if not self.expect(token.Kind.fn):
            return None
        name = self.expect_ident()
        if not name:
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
                    params.append(ast.Param(param_name, param_type, t.span.merge(param_type.span)))
                    match self.input.peek().kind:
                        case token.Kind.comma:
                            self.input.next()
                        case token.Kind.paren_right:
                            break
                        case _:
                            self.error(
                                error.unexpected_token(
                                    t.span, t.kind.name, token.Kind.comma.name, token.Kind.paren_right.name
                                )
                            )
                            return None
                case token.Kind.paren_right:
                    break
                case _:
                    self.error(
                        error.unexpected_token(t.span, t.kind.name, token.Kind.ident.name, token.Kind.paren_right.name)
                    )
                    return None

        if not self.expect(token.Kind.paren_right):
            return None
        result: ast.Type | None = None
        if self.input.peek().kind == token.Kind.type_ident:
            result = self.parse_type()

        return ast.FnDecl(self.id(), name, params, result, self.input.span_merge(span))

    def parse_fn_def(self) -> ast.FnDef | None:
        span = self.input.span()
        fn_decl = self.parse_fn_decl()
        if not fn_decl:
            return None
        body = self.parse_block()
        if not body:
            return None
        return ast.FnDef(self.id(), fn_decl, body, self.input.span_merge(span))

    def parse_ident_expr(self) -> ast.Ident | None:
        span = self.input.span()
        ident = self.expect_ident()
        if not ident:
            return None
        return ast.Ident(self.id(), ident, self.input.span_merge(span))

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
                        error.unexpected_token(t.span, t.kind.name, token.Kind.comma.name, token.Kind.paren_right.name)
                    )
                    return None
        self.input.next()
        return ast.Call(self.id(), callee, args, self.input.span_merge(callee.span))

    def parse_expr(self) -> ast.Expr | None:
        lhs = self.parse_primary_expr()
        if not lhs:
            return None
        while True:
            t = self.input.peek()
            if t.kind == token.Kind.plus:
                self.input.next()
                rhs = self.parse_expr()
                if not rhs:
                    return None
                lhs = ast.BinaryExpr(self.id(), ast.BinaryOp.add, lhs, rhs, self.input.span_merge(lhs.span))
            else:
                return lhs

    def parse_primary_expr(self) -> ast.Expr | None:
        t = self.input.peek()
        expr: ast.Expr | None
        expr_callable = False
        match t.kind:
            case token.Kind.curly_left:
                expr = self.parse_block()
            case token.Kind.ident:
                expr = self.parse_ident_expr()
                expr_callable = True
            case token.Kind.str_lit:
                self.input.next()
                expr = ast.StrLit(self.id(), t.value_str(), t.span)
            case token.Kind.int_lit:
                self.input.next()
                expr = ast.IntLit(self.id(), bits=64, signed=True, value=int(t.value_str()), span=t.span)
            case token.Kind.true | token.Kind.false:
                self.input.next()
                expr = ast.BoolLit(self.id(), value=t.kind == token.Kind.true, span=t.span)
            case _:
                self.error(
                    error.unexpected_token(t.span, t.kind.name, token.Kind.curly_left.name, token.Kind.ident.name)
                )
                return None
        if not expr:
            return None
        if not expr_callable:
            return expr
        match self.input.peek().kind:
            case token.Kind.paren_left:
                expr = self.parse_call(expr)
        return expr

    def parse_block(self) -> ast.Block | None:
        span = self.input.span()
        if not self.expect(token.Kind.curly_left):
            return None
        nodes: list[ast.Node] = []
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
            case _:
                return self.parse_expr()

    def parse_module(self) -> ast.Module:
        span = self.input.span()
        nodes: list[ast.Node] = []
        while t := self.input.peek():
            if t.kind == token.Kind.eof:
                break
            node = self.parse_block_node()
            if node:
                nodes.append(node)
        return ast.Module(self.id(), nodes, self.input.span_merge(span))


def parse(tokens: Input) -> tuple[ast.Module, list[error.Error]]:
    parser = Parser(tokens)
    return (parser.parse_module(), parser.errors)
