from __future__ import annotations

from dataclasses import dataclass
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
class Ident:
    id: NodeId
    name: str
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + self.name


@dataclass
class Call:
    id: NodeId
    callee: Expr
    args: list[Expr]
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"{self.callee}({', '.join(str(x) for x in self.args)})"


@dataclass
class Block:
    id: NodeId
    nodes: list[Node]
    span: Span

    def __str__(self) -> str:
        values = "\n".join("    " + str(x) for x in self.nodes)
        return nid(self.id) + f"{{\n{values}\n}}"


@dataclass
class FnDecl:
    id: NodeId
    name: str
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"fn {self.name}()"


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


Expr = Block | StrLit | Ident | Call
Node = Expr | FnDecl | FnDef | Module


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
        case Ident() | StrLit() | FnDecl():
            return False
        case _:
            raise AssertionError(f"Don't know how to walk: {node}")
    return True
