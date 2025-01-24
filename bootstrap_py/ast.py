from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING, Callable

if TYPE_CHECKING:
    from .span import Span

NodeId = int


def nid(id: NodeId) -> str:
    return f"{{{id}}}"


@dataclass
class StrLit:
    id: NodeId
    value: str
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f'"{self.value}"'


@dataclass
class IntLit:
    id: NodeId
    bits: int
    signed: bool
    value: int
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"I{self.bits}" if self.signed else f"U{self.bits}"


@dataclass
class BoolLit:
    id: NodeId
    value: bool
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + str(self.value).lower()


@dataclass
class Ident:
    id: NodeId
    name: str
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + self.name


@dataclass
class Type:
    id: NodeId
    name: str
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + self.name


@dataclass
class Let:
    id: NodeId
    name: str
    typ: Type | None
    value: Expr
    span: Span
    mutable: bool

    def __str__(self) -> str:
        keyword = "mut" if self.mutable else "let"
        typ = f"{self.typ} " if self.typ else ""
        return nid(self.id) + f"{keyword} {self.name} {typ}= {self.value}"


@dataclass
class Call:
    id: NodeId
    callee: Expr
    args: list[Expr]
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"{self.callee}({', '.join(str(x) for x in self.args)})"


@dataclass
class If:
    id: NodeId
    cond: Expr
    then_block: Block
    else_block: Block | None
    span: Span

    def __str__(self) -> str:
        if self.else_block:
            return nid(self.id) + f"if {self.cond} {self.then_block} else {self.else_block}"
        return nid(self.id) + f"if {self.cond} {self.then_block}"


class BinaryOp(Enum):
    add = "+"
    sub = "-"


@dataclass
class BinaryExpr:
    id: NodeId
    op: BinaryOp
    lhs: Expr
    rhs: Expr
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"{self.lhs} {self.op} {self.rhs}"


@dataclass
class Block:
    id: NodeId
    nodes: list[Node]
    span: Span

    def __str__(self) -> str:
        values = "\n".join("    " + str(x) for x in self.nodes)
        return nid(self.id) + f"{{\n{values}\n}}"


@dataclass
class Param:
    name: str
    typ: Type
    span: Span

    def __str__(self) -> str:
        return f"{self.name} {self.typ}"


@dataclass
class FnDecl:
    id: NodeId
    name: str
    params: list[Param]
    result: Type | None
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"{self.name}({', '.join(str(x) for x in self.params)}) -> {self.result}"


@dataclass
class FnDef:
    id: NodeId
    decl: FnDecl
    body: Block
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"{self.decl} {self.body}"


@dataclass
class Module:
    id: NodeId
    nodes: list[Node]
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + "\n".join(str(x) for x in self.nodes)


Expr = Block | IntLit | StrLit | BoolLit | Ident | Call | BinaryExpr | If
Node = Expr | FnDecl | FnDef | Module | Type | Let


ASTVisitor = Callable[[Node], None]


def walk(node: Node, visit: ASTVisitor) -> bool:
    """Visit all children of the given node.

    @return True if the node contained children
    """
    match node:
        case Module():
            for n in node.nodes:
                visit(n)
        case Block():
            for n in node.nodes:
                visit(n)
        case FnDef():
            visit(node.decl)
            visit(node.body)
        case Call():
            visit(node.callee)
            for arg in node.args:
                visit(arg)
        case BinaryExpr():
            visit(node.lhs)
            visit(node.rhs)
        case If():
            visit(node.cond)
            visit(node.then_block)
            if node.else_block:
                visit(node.else_block)
        case Let():
            visit(node.value)
        case Ident() | IntLit() | StrLit() | BoolLit() | FnDecl():
            return False
        case _:
            raise AssertionError(f"Don't know how to walk: {node}")
    return True
