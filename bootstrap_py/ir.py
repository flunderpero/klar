from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING

from . import ast, lower, types
from .span import FQN

if TYPE_CHECKING:
    from collections.abc import Generator


@dataclass
class Int:
    bits: int
    signed: bool

    def __str__(self) -> str:
        return f"I{self.bits}" if self.signed else f"U{self.bits}"


@dataclass
class Struct:
    fqn: FQN
    fields: list[Type]

    def __str__(self) -> str:
        return f"{self.fqn}{{{', '.join(str(f) for f in self.fields)}}}"


@dataclass
class Fn:
    fqn: str
    params: list[Type]
    result: Type
    is_named: bool

    def __str__(self) -> str:
        name = f" {self.fqn}" if self.is_named else ""
        return f"fn{name}({', '.join(str(p) for p in self.params)}) -> {self.result}"


@dataclass
class Ptr:
    typ: Type

    def __str__(self) -> str:
        return f"*{self.typ}"


@dataclass
class NoneTyp:
    def __str__(self) -> str:
        return "none"


Type = Int | Struct | Fn | Ptr | NoneTyp


I1 = Int(bits=1, signed=True)
I8 = Int(bits=8, signed=True)
U8 = Int(bits=8, signed=False)
I16 = Int(bits=16, signed=True)
U16 = Int(bits=16, signed=False)
I32 = Int(bits=32, signed=True)
U32 = Int(bits=32, signed=False)
I64 = Int(bits=64, signed=True)
U64 = Int(bits=64, signed=False)
Str = Struct(fqn=FQN(["Str"]), fields=[I64, Ptr(U8)])

RegId = str


@dataclass
class Reg:
    id: RegId
    typ: Type

    def __str__(self) -> str:
        return self.id


NoneReg = Reg("%none", NoneTyp())


@dataclass
class IntConst:
    reg: Reg
    value: int

    def __str__(self) -> str:
        return f"{self.reg.id} = {self.value}"


@dataclass
class GetPtr:
    reg: Reg
    src: Reg
    field: int = 0

    def __str__(self) -> str:
        return f"{self.reg} = getptr {self.src.typ} {self.src}, {self.reg.typ}, {self.field}"


@dataclass
class GetFnPtr:
    reg: Reg
    src: Fn

    def __str__(self) -> str:
        return f"{self.reg} = getfnptr {self.src.fqn}, {self.reg.typ}"


@dataclass
class Load:
    reg: Reg
    src: Reg

    def __str__(self) -> str:
        return f"{self.reg} = load {self.src.typ} {self.src}"


@dataclass
class Call:
    reg: Reg
    callee: str | Reg
    args: list[Reg]

    def __str__(self) -> str:
        prefix = f"{self.reg} = call {self.reg.typ}" if self.reg != NoneReg else "call none"
        return f"{prefix} {self.callee}, {', '.join(f'{x.typ} {x.id}' for x in self.args)}"


@dataclass
class Alloc:
    reg: Reg
    args: list[Reg]

    def __str__(self) -> str:
        return f"{self.reg.id} = alloc {self.reg.typ}, {', '.join(f'{x.typ} {x.id}' for x in self.args)}"


@dataclass
class IAddO:
    """Signed addition with overflow."""

    reg: Reg
    lhs: Reg
    rhs: Reg

    def __str__(self) -> str:
        return f"{self.reg} = iaddo {self.lhs.typ} {self.lhs}, {self.rhs.typ} {self.rhs}"


@dataclass
class ISubO:
    """Signed subtraction with overflow."""

    reg: Reg
    lhs: Reg
    rhs: Reg

    def __str__(self) -> str:
        return f"{self.reg} = isubo {self.lhs.typ} {self.lhs}, {self.rhs.typ} {self.rhs}"


class ICmpOp(Enum):
    eq = "eq"
    ne = "ne"


@dataclass
class ICmp:
    reg: Reg
    op: ICmpOp
    lhs: Reg
    rhs: Reg

    def __str__(self) -> str:
        return f"{self.reg} = icmp {self.op.value} {self.lhs.typ} {self.lhs}, {self.rhs.typ} {self.rhs}"


@dataclass
class PhiIn:
    reg: Reg
    block: Block

    def __str__(self) -> str:
        return f"[{self.reg}, {self.block.id}]"


@dataclass
class Phi:
    reg: Reg
    incoming: list[PhiIn]

    def __str__(self) -> str:
        return f"{self.reg} = phi {', '.join(str(reg) for reg in self.incoming)}"


Inst = IntConst | GetPtr | GetFnPtr | Load | Call | Alloc | IAddO | ISubO | ICmp | Phi

BlockId = str


@dataclass
class Block:
    id: BlockId
    insts: list[Inst]
    terminator: Terminator | None

    def __str__(self) -> str:
        insts = "\n".join(f"    {inst}" for inst in self.insts)
        if insts:
            insts += "\n"
        term = str(self.terminator) if self.terminator else "<TERMINATOR MISSING>"
        return f"{self.id}:\n{insts}    {term}"


@dataclass
class Branch:
    reg: Reg
    then_block: Block
    else_block: Block

    def __str__(self) -> str:
        return f"br {self.reg.typ} {self.reg.id}, {self.then_block.id}, {self.else_block.id}"


@dataclass
class Jump:
    target: Block

    def __str__(self) -> str:
        return f"b {self.target.id}"


@dataclass
class Return:
    reg: Reg

    def __str__(self) -> str:
        return f"ret {self.reg.typ} {self.reg.id}"


Terminator = Branch | Jump | Return


@dataclass
class StrConst:
    reg: Reg
    value: str

    def __str__(self) -> str:
        return f'{self.reg.id} = "{self.value}"'


@dataclass
class IR:
    fn_irs: list[FnIR]
    constant_pool: dict[str, StrConst]

    def __str__(self) -> str:
        constants = "\n".join(str(const) for const in self.constant_pool.values())
        fns = "\n\n".join(str(fn_ir) for fn_ir in self.fn_irs)
        return f"{constants}\n\n{fns}"


@dataclass
class Param:
    reg: Reg
    typ: Type

    def __str__(self) -> str:
        return f"{self.typ} {self.reg.id}"


@dataclass
class Scope:
    parent: Scope | None
    vars: dict[str, Reg]

    def declare(self, name: str, reg: Reg) -> None:
        assert name not in self.vars, f"Variable {name} already declared in scope"
        self.vars[name] = reg

    def update(self, name: str, reg: Reg) -> None:
        if name in self.vars:
            self.vars[name] = reg
        elif self.parent:
            self.parent.update(name, reg)

    def find(self, name: str) -> Reg | None:
        res = self.vars.get(name)
        if res:
            return res
        if self.parent:
            return self.parent.find(name)
        return None

    def snapshot(self) -> dict[str, Reg]:
        res = self.vars.copy()
        if self.parent:
            res.update(self.parent.snapshot())
        return res

    def deep_copy(self) -> Scope:
        return Scope(self.parent.deep_copy() if self.parent else None, self.vars.copy())


@dataclass
class LoopScope:
    continue_block: Block
    break_block: Block


@dataclass
class FnIR:
    fn_def: ast.FnDef
    fn_name: str
    params: list[Param]
    result: Type
    blocks: list[Block]

    def __str__(self) -> str:
        params = ", ".join(str(param) for param in self.params)
        blocks = "\n".join(str(block) for block in self.blocks)
        return f"declare {self.fn_name}({params}) {self.result}:\n" + blocks


class FnGen:
    type_env: lower.TypeEnv
    block: Block
    node_regs: dict[ast.NodeId, Reg]
    scope: Scope
    loop_scopes: list[LoopScope]
    ir: IR
    fn_ir: FnIR
    next_reg = 0
    next_block = 0

    def __init__(self, spec: lower.FnSpec, ir: IR) -> None:
        self.type_env = spec.type_env
        self.ir = ir
        self.node_regs = {}
        self.loop_scopes = []
        self.scope = Scope(None, {})
        fn_typ = spec.typ
        fn_def = spec.fn_def
        params: list[Param] = []
        if spec.call_args:
            for arg, p in zip(spec.call_args, fn_typ.params, strict=True):
                typ = self.typ(arg)
                reg = self.reg(typ)
                self.scope.declare(p.name, reg)
                params.append(Param(reg, typ))
        else:
            for p in fn_typ.params:
                typ = self.typ(p.typ)
                reg = self.reg(typ)
                self.scope.declare(p.name, reg)
                params.append(Param(reg, typ))
        result = self.typ(fn_typ.result)
        name = self.fn_name(fn_typ) if fn_def.decl.name != "main" else "main"
        self.fn_ir = FnIR(fn_def, name, params, result, [])
        self.block = self.new_block()

    def fn_name(self, fn: types.Fn) -> str:
        name = str(fn.fqn)
        if fn.type_args:
            name += "$" + "$".join(types.full_id(x) for x in fn.type_args)
        return name

    def new_block(self) -> Block:
        self.next_block += 1
        res = Block(id=f"block_{self.next_block}", insts=[], terminator=None)
        self.fn_ir.blocks.append(res)
        return res

    @contextmanager
    def child_scope(self) -> Generator[None]:
        scope = self.scope
        self.scope = Scope(scope, {})
        yield
        self.scope = scope

    def typ(self, typ: types.Type) -> Type:
        typ = self.type_env.resolve(typ)
        match typ:
            case types.Int():
                match (typ.bits, typ.signed):
                    case (8, True):
                        return I8
                    case (8, False):
                        return U8
                    case (16, True):
                        return I16
                    case (16, False):
                        return U16
                    case (32, True):
                        return I32
                    case (32, False):
                        return U32
                    case (64, True):
                        return I64
                    case (64, False):
                        return U64
                    case (_, _):
                        raise AssertionError(f"Unsupported int type: {typ}")
            case types.Bool():
                return I1
            case types.Str():
                return Str
            case types.NoneTyp():
                return NoneTyp()
            case types.Struct():
                return Struct(typ.fqn, [self.typ(x.typ) for x in typ.fields])
            case types.Fn():
                return Fn(
                    self.fn_name(typ),
                    [self.typ(x.typ) for x in typ.params],
                    self.typ(typ.result),
                    is_named=typ.is_named,
                )
            case _:
                raise AssertionError(f"Unsupported type: {typ.debug()} ({typ.__class__})")

    def reg(self, typ: Type, prefix: str = "%") -> Reg:
        self.next_reg += 1
        return Reg(id=f"{prefix}{self.next_reg}", typ=typ)

    def emit(self, inst: Inst, node: ast.Node | None) -> None:
        self.block.insts.append(inst)
        if node:
            assert node.id not in self.node_regs, f"Node {node.id} already has a register"
            self.node_regs[node.id] = inst.reg

    def generate(self, node: ast.Node, parent: ast.Node | None) -> ast.Node:
        match node:
            case ast.FnDef():
                ast.walk(node, self.generate)
                assert self.block.terminator is None
                reg = NoneReg if isinstance(self.fn_ir.result, NoneTyp) else self.node_regs[node.body.id]
                self.block.terminator = Return(reg)
            case ast.Block():
                with self.child_scope():
                    ast.walk(node, self.generate)
                    reg = NoneReg
                    if node.nodes:
                        reg = self.node_regs[node.nodes[-1].id]
                    self.node_regs[node.id] = reg
            case ast.Loop():
                scope_snapshot = self.scope.snapshot()
                loop_block = self.new_block()
                break_block = self.new_block()
                self.loop_scopes.append(LoopScope(loop_block, break_block))
                prev_block = self.block
                self.block.terminator = Jump(loop_block)
                self.block = loop_block
                self.generate(node.block, node)
                body_scope_snapshot = self.scope.snapshot()
                # Insert phi nodes for every variable that has been changed in the loop body.
                for name, prev_reg in scope_snapshot.items():
                    loop_reg = body_scope_snapshot[name]
                    if prev_reg == loop_reg:
                        continue
                    reg = self.reg(prev_reg.typ)
                    self.scope.update(name, reg)
                    self.emit(Phi(reg, [PhiIn(prev_reg, prev_block), PhiIn(loop_reg, loop_block)]), None)
                self.block.terminator = Jump(loop_block)
                self.block = break_block
                self.loop_scopes.pop()
                self.node_regs[node.id] = NoneReg
            case ast.Continue():
                loop_scope = self.loop_scopes[-1]
                self.block.terminator = Jump(loop_scope.continue_block)
                self.node_regs[node.id] = NoneReg
            case ast.Break():
                loop_scope = self.loop_scopes[-1]
                self.block.terminator = Jump(loop_scope.break_block)
                self.node_regs[node.id] = NoneReg
            case ast.If():
                self.generate(node.cond, node)
                prev_block = self.block
                cond_reg = self.node_regs[node.cond.id]
                then_block = self.new_block()
                scope_copy = self.scope.deep_copy()
                scope_snapshot = self.scope.snapshot()
                # Walk the `then_block`.
                self.block = then_block
                self.generate(node.then_block, node)
                then_scope_snapshot = self.scope.snapshot()
                if not node.else_block:
                    # Reset the scope.
                    self.scope = scope_copy
                    merge_block = self.new_block()
                    # End the then-branch by jumping to the merge-block.
                    if not self.block.terminator:
                        self.block.terminator = Jump(merge_block)
                    # There is no `else_block` so the result of the if expression is None.
                    self.node_regs[node.id] = NoneReg
                    prev_block.terminator = Branch(cond_reg, then_block, merge_block)
                    self.block = merge_block
                    # Insert phi nodes for every variable that has been changed in the then-branch.
                    for name, prev_reg in scope_snapshot.items():
                        then_reg = then_scope_snapshot[name]
                        if prev_reg == then_reg:
                            continue
                        reg = self.reg(prev_reg.typ)
                        self.scope.update(name, reg)
                        self.emit(Phi(reg, [PhiIn(prev_reg, prev_block), PhiIn(then_reg, then_block)]), None)
                else:
                    # Reset the scope.
                    self.scope = scope_copy
                    else_block = self.new_block()
                    merge_block = self.new_block()
                    # End the then-branch by jumping to the merge-block.
                    if not self.block.terminator:
                        self.block.terminator = Jump(merge_block)
                    # Walk the `else_block`.
                    prev_block.terminator = Branch(cond_reg, then_block, else_block)
                    self.block = else_block
                    self.generate(node.else_block, node)
                    else_scope_snapshot = self.scope.snapshot()
                    assert not self.block.terminator
                    self.block.terminator = Jump(merge_block)
                    self.block = merge_block
                    # Insert a phi node to signal that the result of the if expression is based
                    # on the branch taken if it is not none.
                    then_reg = self.node_regs[node.then_block.id]
                    else_reg = self.node_regs[node.else_block.id]
                    if NoneReg not in (then_reg, else_reg):
                        reg = self.reg(self.typ(self.type_env.get_node_type(node)))
                        self.emit(Phi(reg, [PhiIn(then_reg, then_block), PhiIn(else_reg, else_block)]), node)
                    else:
                        self.node_regs[node.id] = NoneReg
                    # Reset the scope again.
                    self.scope = scope_copy
                    # Insert phi nodes for every variable that has been changed in either branches.
                    for name, prev_reg in scope_snapshot.items():
                        then_reg = then_scope_snapshot[name]
                        else_reg = else_scope_snapshot[name]
                        if prev_reg == then_reg and prev_reg == else_reg:
                            continue
                        reg = self.reg(prev_reg.typ)
                        if prev_reg not in (then_reg, else_reg):
                            self.emit(Phi(reg, [PhiIn(then_reg, then_block), PhiIn(else_reg, else_block)]), None)
                        elif prev_reg != then_reg:
                            self.emit(Phi(reg, [PhiIn(prev_reg, prev_block), PhiIn(then_reg, then_block)]), None)
                        else:
                            self.emit(Phi(reg, [PhiIn(prev_reg, prev_block), PhiIn(else_reg, else_block)]), None)
            case ast.StrLit():
                const = self.ir.constant_pool.get(node.value)
                if not const:
                    reg = Reg(f"s{len(self.ir.constant_pool)}", Str)
                    const = StrConst(reg, node.value)
                    self.ir.constant_pool[node.value] = const
                self.emit(GetPtr(reg=self.reg(Str), src=const.reg), node)
            case ast.IntLit():
                reg = self.reg(I64)
                self.emit(IntConst(reg, value=node.value), node)
            case ast.BoolLit():
                reg = self.reg(I1)
                self.emit(IntConst(reg, value=int(node.value)), node)
            case ast.Ident():
                reg = self.scope.find(node.name)
                if reg:
                    self.node_regs[node.id] = reg
                    return node
                # If this node is the callee of a call node then we don't want
                # to emit a GetFnPtr for named functions.
                if isinstance(parent, ast.Call) and parent.callee == node:
                    return node
                ir_typ = self.type_env.get_node_type(node)
                if not isinstance(ir_typ, types.Fn) or not ir_typ.is_named:
                    return node
                # Emit a GetFnPtr if the identifier refers to a named function.
                getptr_reg = self.reg(Ptr(self.typ(ir_typ)))
                fn_typ = self.typ(ir_typ)
                assert isinstance(fn_typ, Fn), f"Expected Fn, got {fn_typ}"
                self.emit(GetFnPtr(getptr_reg, fn_typ), None)
                self.node_regs[node.id] = getptr_reg
            case ast.Member():
                ast.walk(node, self.generate)
                src = self.node_regs[node.target.id]
                types_src = self.type_env.get_node_type(node.target)
                assert isinstance(src.typ, Struct)
                assert isinstance(types_src, types.Struct), f"Expected Struct, got {types_src}"
                # Because all method members have already been lowered this can only be a field.
                field_index = types_src.field_index(node.name)
                assert field_index is not None
                getptr_reg = self.reg(Ptr(src.typ.fields[field_index]))
                self.emit(GetPtr(getptr_reg, src, field_index), None)
                reg = self.reg(src.typ.fields[field_index])
                self.emit(Load(reg, getptr_reg), node)
            case ast.Call():
                callee = self.type_env.get_node_type(node.callee)
                assert isinstance(callee, types.CallableType), f"Expected CallableType, got {callee}"
                ast.walk(node, self.generate)
                args = [self.node_regs[x.id] for x in node.args]
                match callee:
                    case types.Fn():
                        reg = NoneReg
                        result_typ = callee.result
                        if not isinstance(result_typ, types.NoneTyp):
                            reg = self.reg(self.typ(result_typ))
                        if callee.is_named:
                            # Direct call by name.
                            self.emit(Call(reg, self.fn_name(callee), args), node)
                        else:
                            # Indirect call by register.
                            src = self.node_regs[node.callee.id]
                            self.emit(Call(reg, src, args), node)
                    case types.Struct():
                        typ = self.typ(callee)
                        reg = self.reg(typ)
                        self.emit(Alloc(reg, args), node)
                    case _:
                        raise AssertionError(f"Unsupported callee type: {callee.typ}")
            case ast.Let():
                ast.walk(node, self.generate)
                self.scope.declare(node.name, self.node_regs[node.value.id])
                self.node_regs[node.id] = NoneReg
            case ast.Assign():
                ast.walk(node, self.generate)
                src = node.target
                assert isinstance(src, ast.Ident)
                self.scope.update(src.name, self.node_regs[node.value.id])
                self.node_regs[node.id] = NoneReg
            case ast.BinaryExpr():
                ast.walk(node, self.generate)
                lhs_reg = self.node_regs[node.lhs.id]
                rhs_reg = self.node_regs[node.rhs.id]
                match node.op:
                    case ast.BinaryOp.add:
                        reg = self.reg(I64)
                        self.emit(IAddO(reg, lhs_reg, rhs_reg), node)
                    case ast.BinaryOp.sub:
                        reg = self.reg(I64)
                        self.emit(ISubO(reg, lhs_reg, rhs_reg), node)
                    case ast.BinaryOp.eq | ast.BinaryOp.ne:
                        match lhs_reg.typ:
                            case Int():
                                op = ICmpOp.eq if node.op == ast.BinaryOp.eq else ICmpOp.ne
                                reg = self.reg(I1)
                                self.emit(ICmp(reg, op, lhs_reg, rhs_reg), node)
                            case _:
                                raise AssertionError(f"Unsupported type for equality comparison: {lhs_reg.typ}")
                    case _:
                        raise AssertionError(f"Unsupported binary op: {node.op}")
            case ast.FnDecl():
                pass
            case _:
                raise AssertionError(f"Unsupported node: {node.__class__}")
        return node


def generate_ir(specs: list[lower.FnSpec]) -> IR:
    ir = IR(fn_irs=[], constant_pool={})
    for spec in specs:
        gen = FnGen(spec, ir)
        gen.generate(spec.fn_def, None)
        ir.fn_irs.append(gen.fn_ir)

    return ir
