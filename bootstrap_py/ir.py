from __future__ import annotations

from dataclasses import dataclass
from typing import cast

from . import ast, typechecker, types


@dataclass
class Int:
    bits: int
    signed: bool

    def __str__(self) -> str:
        return f"I{self.bits}" if self.signed else f"U{self.bits}"


@dataclass
class Struct:
    name: str
    fields: list[Type]

    def __str__(self) -> str:
        return f"{self.name}{{{', '.join(str(f) for f in self.fields)}}}"


@dataclass
class Ptr:
    typ: Type

    def __str__(self) -> str:
        return f"*{self.typ}"


@dataclass
class NoneTyp:
    def __str__(self) -> str:
        return "none"


Type = Int | Struct | Ptr | NoneTyp


I1 = Int(bits=1, signed=True)
I8 = Int(bits=8, signed=True)
U8 = Int(bits=8, signed=False)
I16 = Int(bits=16, signed=True)
U16 = Int(bits=16, signed=False)
I32 = Int(bits=32, signed=True)
U32 = Int(bits=32, signed=False)
I64 = Int(bits=64, signed=True)
U64 = Int(bits=64, signed=False)
Str = Struct(name="Str", fields=[I64, Ptr(U8)])

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
class Call:
    reg: Reg
    callee: str
    args: list[Reg]

    def __str__(self) -> str:
        prefix = f"{self.reg} = call {self.reg.typ}" if self.reg != NoneReg else "call none"
        return f"{prefix} {self.callee}, {', '.join(f'{x.typ} {x.id}' for x in self.args)}"


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


@dataclass
class PhiIncoming:
    reg: Reg
    block: Block

    def __str__(self) -> str:
        return f"[{self.reg}, {self.block.id}]"


@dataclass
class Phi:
    reg: Reg
    incoming: list[PhiIncoming]

    def __str__(self) -> str:
        return f"{self.reg} = phi {', '.join(str(reg) for reg in self.incoming)}"


Inst = IntConst | GetPtr | Call | IAddO | ISubO | Phi

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


@dataclass
class FnIR:
    fn_def: ast.FnDef
    params: list[Param]
    result: Type
    blocks: list[Block]

    def __str__(self) -> str:
        params = ", ".join(str(param) for param in self.params)
        blocks = "\n".join(str(block) for block in self.blocks)
        return f"declare {self.fn_def.decl.name}({params}) {self.result}:\n" + blocks


class FnGen:
    type_env: typechecker.TypeEnv
    block: Block
    node_regs: dict[ast.NodeId, Reg]
    vars: dict[str, Reg]
    ir: IR
    fn_ir: FnIR
    next_reg = 0
    next_const = 0
    next_block = 0

    def __init__(self, fn_def: ast.FnDef, type_env: typechecker.TypeEnv, ir: IR) -> None:
        self.type_env = type_env
        self.ir = ir
        self.node_regs = {}
        self.vars = {}
        types_fn = cast(types.Fn, type_env.get_node_type(fn_def.decl))
        params: list[Param] = []
        for p in types_fn.params:
            typ = self.typ(p.typ)
            reg = self.reg(typ)
            self.vars[p.name] = reg
            params.append(Param(reg, typ))
        result = self.typ(types_fn.result)
        self.fn_ir = FnIR(fn_def, params, result, [])
        self.block = self.new_block()

    def new_block(self) -> Block:
        self.next_block += 1
        res = Block(id=f"block_{self.next_block}", insts=[], terminator=None)
        self.fn_ir.blocks.append(res)
        return res

    def typ(self, typ: types.Type) -> Type:
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
            case _:
                raise AssertionError(f"Unsupported type: {typ}")

    def reg(self, typ: Type, prefix: str = "%") -> Reg:
        self.next_reg += 1
        return Reg(id=f"{prefix}{self.next_reg}", typ=typ)

    def emit(self, inst: Inst, node: ast.Node | None) -> None:
        self.block.insts.append(inst)
        if node:
            assert node.id not in self.node_regs, f"Node {node.id} already has a register"
            self.node_regs[node.id] = inst.reg

    def generate(self, node: ast.Node) -> None:
        match node:
            case ast.FnDef():
                ast.walk(node, self.generate)
                assert self.block.terminator is None
                reg = NoneReg if isinstance(self.fn_ir.result, NoneTyp) else self.node_regs[node.body.id]
                self.block.terminator = Return(reg)
            case ast.Block():
                ast.walk(node, self.generate)
                reg = NoneReg
                if node.nodes:
                    reg = self.node_regs[node.nodes[-1].id]
                self.node_regs[node.id] = reg
            case ast.If():
                self.generate(node.cond)
                prev_block = self.block
                cond_reg = self.node_regs[node.cond.id]
                then_block = self.new_block()
                # Walk the `then_block`.
                self.block = then_block
                self.generate(node.then_block)
                if not node.else_block:
                    merge_block = self.new_block()
                    assert not self.block.terminator
                    self.block.terminator = Jump(merge_block)
                    # There is no `else_block` so the result of the if expression is None.
                    self.node_regs[node.id] = NoneReg
                    prev_block.terminator = Branch(cond_reg, then_block, merge_block)
                else:
                    else_block = self.new_block()
                    merge_block = self.new_block()
                    # We are still at the end of the `then_block`, so we have to terminate it
                    # properly.
                    # This might look like duplicate code (in the branch above we do the same).
                    # We do it this way to have the `then`, `else`, and merge block in order.
                    # (Blocks are added in the order of calls to `self.new_block()`).
                    assert not self.block.terminator
                    self.block.terminator = Jump(merge_block)
                    # Walk `else_block`.
                    prev_block.terminator = Branch(cond_reg, then_block, else_block)
                    self.block = else_block
                    self.generate(node.else_block)
                    assert not self.block.terminator
                    self.block.terminator = Jump(merge_block)
                    # Insert a phi node to signal that the result of the if expression is based
                    # on the branch taken.
                    reg = self.reg(self.typ(self.type_env.get_node_type(node)))
                    self.block = merge_block
                    self.emit(
                        Phi(
                            reg,
                            [
                                PhiIncoming(self.node_regs[node.then_block.id], then_block),
                                PhiIncoming(self.node_regs[node.else_block.id], else_block),
                            ],
                        ),
                        node,
                    )
                self.block = merge_block
            case ast.StrLit():
                const = self.ir.constant_pool.get(node.value)
                if not const:
                    self.next_const += 1
                    reg = Reg(f"s{self.next_const}", Str)
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
                if node.name in self.vars:
                    self.node_regs[node.id] = self.vars[node.name]
            case ast.Call():
                assert isinstance(node.callee, ast.Ident), "Currently, only named functions are supported."
                result_typ = self.type_env.get_node_type(node)
                ast.walk(node, self.generate)
                args = [self.node_regs[x.id] for x in node.args]
                reg = NoneReg
                if not isinstance(result_typ, types.NoneTyp):
                    reg = self.reg(self.typ(result_typ))
                self.emit(Call(reg, node.callee.name, args), node)
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
                    case _:
                        raise AssertionError(f"Unsupported binary op: {node.op}")
            case ast.FnDecl():
                pass
            case _:
                raise AssertionError(f"Unsupported node: {node.__class__}")


def generate_ir(module: ast.Module, type_env: typechecker.TypeEnv) -> IR:
    fn_defs: list[ast.FnDef] = []

    def collect_fn(node: ast.Node) -> None:
        if isinstance(node, ast.FnDef):
            fn_defs.append(node)
        ast.walk(node, collect_fn)

    ast.walk(module, collect_fn)

    ir = IR(fn_irs=[], constant_pool={})
    for fn_def in fn_defs:
        gen = FnGen(fn_def, type_env, ir)
        gen.generate(fn_def)
        ir.fn_irs.append(gen.fn_ir)

    return ir
