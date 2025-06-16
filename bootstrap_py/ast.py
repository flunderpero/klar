from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING, Callable, cast

if TYPE_CHECKING:
    from .span import FQN, Span

NodeId = int

nid_enabled = True


def to_str_withoud_nid(node: Node) -> str:
    global nid_enabled  # noqa: PLW0603
    ne = nid_enabled
    try:
        nid_enabled = False
        return str(node)
    finally:
        nid_enabled = ne


def nid(id: NodeId) -> str:
    if not nid_enabled:
        return ""
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
class NamedType:
    id: NodeId
    name: str
    type_args: TypeArgs
    span: Span

    def __str__(self) -> str:
        return nid(self.id) + f"{self.name}{generics_to_str(self.type_args)}"


@dataclass
class FnType:
    id: NodeId
    params: list[Type]
    result: Type
    span: Span

    def __str__(self) -> str:
        params = ", ".join(str(x) for x in self.params)
        return nid(self.id) + f"fn({params}) -> {self.result}"


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
class Trait:
    id: NodeId
    name: str
    methods: list[FnDecl]
    type_params: TypeParams
    span: Span

    def __str__(self) -> str:
        methods = "\n".join("    " + str(x) for x in self.methods)
        type_params = generics_to_str(self.type_params)
        return nid(self.id) + f"trait {self.name}{type_params}{{\n{methods}\n}}"


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
    trait_bound: NamedType | None
    span: Span

    def __str__(self) -> str:
        trait_bound = f" {self.trait_bound}" if self.trait_bound else ""
        return self.name + trait_bound


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
    receiver: str | None
    trait_qualifier: NamedType | None
    params: list[FieldOrParam]
    result: Type | None
    type_params: TypeParams
    span: Span

    def fullname(self) -> str:
        if self.receiver is None:
            return self.name
        return f"{self.receiver}.{self.name}"

    def __str__(self) -> str:
        type_params = generics_to_str(self.type_params)
        receiver = (self.receiver + ".") if self.receiver is not None else ""
        trait = (f"({self.trait_qualifier}) ") if self.trait_qualifier is not None else ""
        return (
            nid(self.id)
            + f"fn {trait}{receiver}{self.name}{type_params}({', '.join(str(x) for x in self.params)}) {self.result}"
        )


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


Type = NamedType | FnType
Expr = Block | IntLit | StrLit | BoolLit | Ident | Member | Call | BinaryExpr | If
Node = Expr | FnDecl | FnDef | Module | Type | Let | Assign | Loop | Break | Continue | Struct | Trait
ParameterizedNode = FnDecl | Struct | Trait
ImplementableNode = Struct | Trait

TypeParams = list[TypeParam]
TypeArgs = list[Type]


ASTVisitor = Callable[[Node, Node | None], Node]


def walk(node: Node, visit_: ASTVisitor) -> bool:
    """Visit all children of the given node.

    @return True if the node contained children
    """

    def visit(node: Node, parent: Node | None) -> Node:
        res = visit_(node, parent)
        assert res is not None, f"`visit` must return a node, got {res}"
        return res

    match node:
        case Module():
            for i, n in enumerate(node.nodes):
                node.nodes[i] = visit(n, node)
        case Block():
            for i, n in enumerate(node.nodes):
                node.nodes[i] = visit(n, node)
        case FnDef():
            node.decl = cast(FnDecl, visit(node.decl, node))
            node.body = cast(Block, visit(node.body, node))
        case Call():
            node.callee = cast(Expr, visit(node.callee, node))
            for i, arg in enumerate(node.args):
                node.args[i] = cast(Expr, visit(arg, node))
        case BinaryExpr():
            node.lhs = cast(Expr, visit(node.lhs, node))
            node.rhs = cast(Expr, visit(node.rhs, node))
        case If():
            node.cond = cast(Expr, visit(node.cond, node))
            node.then_block = cast(Block, visit(node.then_block, node))
            if node.else_block:
                node.else_block = cast(Block, visit(node.else_block, node))
        case Loop():
            node.block = cast(Block, visit(node.block, node))
        case Let():
            node.value = cast(Expr, visit(node.value, node))
        case Assign():
            node.target = cast(Expr, visit(node.target, node))
            node.value = cast(Expr, visit(node.value, node))
        case Member():
            node.target = cast(Expr, visit(node.target, node))
        case Trait():
            for i, m in enumerate(node.methods):
                node.methods[i] = cast(FnDecl, visit(m, node))
        case Ident() | IntLit() | StrLit() | BoolLit() | FnDecl() | Break() | Continue() | Struct():
            return False
        case _:
            raise AssertionError(f"Don't know how to walk: {node}")
    return True
