from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING, Callable

if TYPE_CHECKING:
    from .span import FQN, Span

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
    type_args: TypeArgs
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + self.name + generics_to_str(self.type_args)


@dataclass
class Member:
    id: NodeId
    target: Expr
    name: str
    type_args: TypeArgs
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"{self.target}.{self.name}{generics_to_str(self.type_args)}"


@dataclass
class Type:
    id: NodeId
    name: str
    type_args: TypeArgs
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"{self.name}{generics_to_str(self.type_args)}"


@dataclass
class Struct:
    id: NodeId
    name: str
    fields: list[FieldOrParam]
    type_params: TypeParams
    span: Span

    def __str__(self) -> str:
        fields = ", ".join(str(x) for x in self.fields)
        type_params = generics_to_str(self.type_params)
        return nid(self.id) + f"struct {self.name}{type_params}{{{fields}}}"


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
class Assign:
    id: NodeId
    target: Expr
    value: Expr
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"{self.target} = {self.value}"


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


@dataclass
class Loop:
    id: NodeId
    block: Block
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"loop {self.block}"


@dataclass
class Break:
    id: NodeId
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + "break"


@dataclass
class Continue:
    id: NodeId
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + "continue"


class BinaryOp(Enum):
    add = "+"
    eq = "=="
    ne = "!="
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
class TypeParam:
    name: str
    span: Span

    def __str__(self) -> str:
        return self.name


TypeParams = list[TypeParam]
TypeArgs = list[Type]


def generics_to_str(type_params: TypeParams | TypeArgs) -> str:
    if not type_params:
        return ""
    return f"<{', '.join(str(x) for x in type_params)}>"


@dataclass
class FieldOrParam:
    name: str
    typ: Type
    span: Span

    def __str__(self) -> str:
        return f"{self.name} {self.typ}"


@dataclass
class FnDecl:
    id: NodeId
    name: str
    params: list[FieldOrParam]
    result: Type | None
    type_params: TypeParams
    span: Span

    def __str__(self) -> str:
        type_params = generics_to_str(self.type_params)
        return nid(self.id) + f"{self.name}{type_params}({', '.join(str(x) for x in self.params)}) -> {self.result}"


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
    fqn: FQN
    nodes: list[Node]
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"mod {self.fqn}\n" + "\n".join(str(x) for x in self.nodes)


Expr = Block | IntLit | StrLit | BoolLit | Ident | Member | Call | BinaryExpr | If
Node = Expr | FnDecl | FnDef | Module | Type | Let | Assign | Loop | Break | Continue | Struct


ASTVisitor = Callable[[Node, Node | None], None]


def walk(node: Node, visit: ASTVisitor) -> bool:
    """Visit all children of the given node.

    @return True if the node contained children
    """
    match node:
        case Module():
            for n in node.nodes:
                visit(n, node)
        case Block():
            for n in node.nodes:
                visit(n, node)
        case FnDef():
            visit(node.decl, node)
            visit(node.body, node)
        case Call():
            visit(node.callee, node)
            for arg in node.args:
                visit(arg, node)
        case BinaryExpr():
            visit(node.lhs, node)
            visit(node.rhs, node)
        case If():
            visit(node.cond, node)
            visit(node.then_block, node)
            if node.else_block:
                visit(node.else_block, node)
        case Loop():
            visit(node.block, node)
        case Let():
            visit(node.value, node)
        case Assign():
            visit(node.target, node)
            visit(node.value, node)
        case Member():
            visit(node.target, node)
        case Ident() | IntLit() | StrLit() | BoolLit() | FnDecl() | Break() | Continue() | Struct():
            return False
        case _:
            raise AssertionError(f"Don't know how to walk: {node}")
    return True
