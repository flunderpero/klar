from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass
from typing import TYPE_CHECKING, cast

from . import ir

if TYPE_CHECKING:
    from collections.abc import Generator

Reg = str

call_regs = [Reg(f"x{i}") for i in range(8)]
caller_saved = [Reg(f"x{i}") for i in range(9, 16)]
callee_saved = [Reg(f"x{i}") for i in range(19, 29)]
all_regs = call_regs + caller_saved + callee_saved
regs = caller_saved + callee_saved
indirect_fn_call_reg = Reg("x17")


def to_32bit(reg: Reg) -> str:
    return reg.replace("x", "w")


class DataLayout:
    @staticmethod
    def sizeof(typ: ir.Type) -> int:
        match typ:
            case ir.NoneTyp:
                return 0
            case ir.Int():
                return max(typ.bits // 8, 1)
            case ir.Ptr() | ir.Fn():
                return 8
            case ir.Struct():
                if not typ.fields:
                    return 0
                size = DataLayout.field_offset(typ, len(typ.fields) - 1) + DataLayout.sizeof(typ.fields[-1])
                return (size + 7) & ~7
            case _:
                raise AssertionError(f"Unknown type: {typ}")

    @staticmethod
    def field_offset(typ: ir.Struct, index: int) -> int:
        offset = 0
        for field in typ.fields[:index]:
            offset += DataLayout.sizeof(field)
            alignment = DataLayout.alignment(field)
            offset += (alignment - offset % alignment) % alignment
        return offset

    @staticmethod
    def alignment(typ: ir.Type) -> int:
        match typ:
            case ir.Int():
                return max(typ.bits // 8, 1)
            case ir.Ptr():
                return 8
            case ir.Struct():
                return 8
            case _:
                raise AssertionError(f"Unknown type: {typ}")


class StackAllocator:
    size: int = 16

    def allocate(self, size: int) -> int:
        offset = self.size
        self.size += size
        return offset

    def aligned_size(self) -> int:
        return (self.size + 15) & ~15


@dataclass
class RegAlloc:
    reg: Reg | None
    ir_regs: list[ir.Reg]
    stack_offset: int = 0

    def allocate_on_stack_if_needed(self, stack_allocator: StackAllocator) -> int:
        if not self.stack_offset:
            self.stack_offset = stack_allocator.allocate(8)
        return self.stack_offset

    def is_in_reg(self) -> bool:
        return self.reg is not None


IRRegConstraint = set[ir.RegId]


class RegAllocator:
    asm: ASM
    regs: dict[ir.RegId, RegAlloc]
    used: dict[Reg, RegAlloc | None]
    stack_allocator: StackAllocator
    # All IR-registers in each entry have to map to the same ASM register.
    constraints: list[IRRegConstraint]

    def __init__(self, stack_allocator: StackAllocator, constraints: list[IRRegConstraint], asm: ASM) -> None:
        self.asm = asm
        self.used = dict.fromkeys(regs, None)
        self.regs = {}
        self.constraints = constraints
        self.stack_allocator = stack_allocator

    def already_constrained(self, ir_reg: ir.Reg) -> RegAlloc | None:
        for constraint in self.constraints:
            if ir_reg.id in constraint:
                for reg_id in constraint:
                    if reg_id in self.regs:
                        return self.regs[reg_id]
        return None

    def allocate(self, ir_reg: ir.Reg) -> RegAlloc:
        alloc = self.already_constrained(ir_reg)
        if alloc:
            alloc.ir_regs.append(ir_reg)
            self.regs[ir_reg.id] = alloc
            return alloc
        if ir_reg.id in self.used:
            raise AssertionError(f"IR register {ir_reg} is already allocated")
        alloc = RegAlloc(None, [ir_reg])
        self.regs[ir_reg.id] = alloc
        return alloc

    def _restore_alloc(self, alloc: RegAlloc) -> Reg:
        """Read the register from the stack if needed."""
        if alloc.is_in_reg():
            assert alloc.reg is not None
            return alloc.reg
        never_spilled = alloc.stack_offset == 0
        # Find a free register.
        for reg, used in self.used.items():
            if used is not None:
                continue
            self.used[reg] = alloc
            alloc.reg = reg
            break
        else:
            raise AssertionError("Out of registers")
        if not never_spilled:
            self.asm.emit(f"ldr {alloc.reg}, [fp, #{alloc.stack_offset}]")
        return alloc.reg

    def _spill_alloc(self, alloc: RegAlloc) -> None:
        if not alloc.is_in_reg():
            return
        assert alloc.reg is not None
        offset = alloc.allocate_on_stack_if_needed(self.stack_allocator)
        self.asm.emit(f"str {alloc.reg}, [fp, #{offset}]")
        self.used[alloc.reg] = None
        alloc.reg = None

    def forget(self, alloc: RegAlloc) -> None:
        if len(alloc.ir_regs) > 1:
            raise AssertionError(f"RegAlloc {alloc} has more than one IR register")
        del self.regs[alloc.ir_regs[0].id]
        if not alloc.is_in_reg():
            return
        assert alloc.reg is not None
        self.used[alloc.reg] = None
        alloc.reg = None

    @contextmanager
    def use_read(self, *allocs: RegAlloc) -> Generator[list[Reg]]:
        for alloc in allocs:
            if not alloc.is_in_reg():
                self._restore_alloc(alloc)
        yield [cast(Reg, alloc.reg) for alloc in allocs]

    @contextmanager
    def use_write(self, *allocs: RegAlloc) -> Generator[list[Reg]]:
        for alloc in allocs:
            if not alloc.is_in_reg():
                self._restore_alloc(alloc)
        yield [cast(Reg, alloc.reg) for alloc in allocs]
        for alloc in allocs:
            self._spill_alloc(alloc)

    @contextmanager
    def use_with_registers(self, allocs: list[RegAlloc], regs: list[Reg]) -> Generator[None]:
        for alloc, reg in zip(allocs, regs):
            if not alloc.is_in_reg():
                if alloc.stack_offset != 0:
                    self.asm.emit(f"ldr {reg}, [fp, #{alloc.stack_offset}]")
                continue
            if alloc.reg != reg:
                self.asm.emit(f"mov {reg}, {alloc.reg}")
        yield

    @contextmanager
    def with_spilled_caller_saved_regs(self) -> Generator[None]:
        return self._with_spilled_saved_regs(caller_saved)

    @contextmanager
    def with_spilled_callee_saved_regs(self) -> Generator[None]:
        return self._with_spilled_saved_regs(callee_saved)

    def _with_spilled_saved_regs(self, regs: list[Reg]) -> Generator[None]:
        for reg in regs:
            alloc = self.used[reg]
            if alloc and alloc.is_in_reg():
                self._spill_alloc(alloc)
        yield


class FnGen:
    asm: ASM
    ir: ir.FnIR
    reg_allocator: RegAllocator
    stack_allocator: StackAllocator
    allocs: dict[ir.RegId, RegAlloc]

    def __init__(self, ir: ir.FnIR) -> None:
        self.ir = ir
        self.asm = ASM()
        self.allocs = {}
        self.stack_allocator = StackAllocator()
        self.reg_allocator = RegAllocator(self.stack_allocator, self.allocator_constraints(), self.asm)

    def allocator_constraints(self) -> list[IRRegConstraint]:
        """Go through all `phi` nodes and build a list of register constraints,
        that is a list of sets of register ids that have to be mapped to the same
        ASM register.
        """
        res: list[IRRegConstraint] = []
        for block in self.ir.blocks:
            for phi in (x for x in block.insts if isinstance(x, ir.Phi)):
                regs = [x.reg for x in phi.incoming] + [phi.reg]
                # Find existing constraint.
                for constraint in res:
                    if any(x.id in constraint for x in regs):
                        constraint.update(x.id for x in regs)
                        break
                else:
                    res.append({x.id for x in regs})
        return res

    def fn_name(self, name: str) -> str:
        if name == "main":
            return "_main"
        return f".{name}"

    def block_label(self, suffix: str | int) -> str:
        if isinstance(suffix, int):
            suffix = str(suffix)
        return f".{self.ir.fn_name}_{suffix}"

    def generate(self) -> ASM:
        # First generate all the code so that we know how large the stack frame will have to be
        # and which callee-saved registers have to be preserved.
        fn_name = self.fn_name(self.ir.fn_name)
        self.asm.inc_indent()
        for i, param in enumerate(self.ir.params):
            alloc = self.reg_allocator.allocate(param.reg)
            with self.reg_allocator.use_write(alloc) as [reg]:
                self.asm.emit(f"mov {reg}, x{i}")
            self.allocs[param.reg.id] = alloc
        self.asm.dec_indent()
        for block in self.ir.blocks:
            self.asm.emit(f"{self.block_label(block.id)}:")
            self.asm.inc_indent()
            for inst in block.insts:
                self.inst(inst)
            term = block.terminator
            match term:
                case ir.Jump():
                    self.asm.emit(f"b {self.block_label(term.target.id)}")
                case ir.Branch():
                    cond = self.allocs[term.reg.id]
                    with self.reg_allocator.use_read(cond) as [reg]:
                        self.asm.emit(f"cbnz {reg}, {self.block_label(term.then_block.id)}")
                        self.asm.emit(f"b {self.block_label(term.else_block.id)}")
                    self.reg_allocator.forget(cond)
                case ir.Return():
                    if term.reg == ir.NoneReg:
                        self.asm.emit("mov x0, xzr")
                    else:
                        with self.reg_allocator.use_read(self.allocs[term.reg.id]) as [reg]:
                            self.asm.emit(f"mov x0, {reg}")
                    self.asm.emit(f"b {self.block_label('ret')}")
                case _:
                    raise AssertionError(f"Unknown terminator: {term}")
            self.asm.dec_indent()
        # Now generate the surrounding code.
        res = ASM()
        res.emit(f"{fn_name}:")
        # `with_spilled_callee_saved_regs` might increase the stack size, so we call it
        # first and then prepare the stack frame.
        body = ASM()
        self.reg_allocator.asm = body
        body.inc_indent()
        with self.reg_allocator.with_spilled_callee_saved_regs():
            # Add the generated code.
            body.extend(self.asm)
            # Return block.
            body.dec_indent()
            body.emit(f"{self.block_label('ret')}:")
            body.inc_indent()

        # Prepare stack frame.
        res.inc_indent()
        res.emit(f"stp fp, lr, [sp, #-{self.stack_allocator.aligned_size()}]!")
        res.emit("mov fp, sp")
        # Add the body.
        res.extend(body)
        # Release stack frame and return.
        res.emit(f"ldp fp, lr, [sp], #{self.stack_allocator.aligned_size()}")
        if fn_name == "_main":
            res.emit("mov x0, xzr")
        res.emit("ret")
        return res

    def inst(self, inst: ir.Inst) -> None:
        match inst:
            case ir.Phi():
                alloc = self.reg_allocator.allocate(inst.reg)
                self.allocs[inst.reg.id] = alloc
            case ir.IntConst():
                if inst.reg.typ == ir.I1:
                    assert inst.value in (0, 1), f"Invalid I1 value: {inst.value}"
                    alloc = self.reg_allocator.allocate(inst.reg)
                    with self.reg_allocator.use_write(alloc) as [reg]:
                        self.asm.emit(f"movz {reg}, #{inst.value}")
                    self.allocs[inst.reg.id] = alloc
                    return
                assert inst.reg.typ == ir.I64, "For now, only I64 is supported"
                alloc = self.reg_allocator.allocate(inst.reg)
                with self.reg_allocator.use_write(alloc) as [reg]:
                    # It is not straight forward to load int values > 16bit. There are a lot of ways to optimize
                    # this, but that's an exercise for another day.
                    value = inst.value
                    if value >= 0 and value <= 0xFFFF:
                        self.asm.emit(f"movz {reg}, #{value}")
                    else:
                        chunk0 = value & 0xFFFF
                        chunk1 = (value >> 16) & 0xFFFF
                        chunk2 = (value >> 32) & 0xFFFF
                        chunk3 = (value >> 48) & 0xFFFF
                        mov = "movz"
                        if chunk3 != 0:
                            self.asm.emit(f"{mov} {reg}, #{chunk3}, lsl #48")
                            mov = "movk"
                        if chunk2 != 0:
                            self.asm.emit(f"{mov} {reg}, #{chunk2}, lsl #32")
                            mov = "movk"
                        if chunk1 != 0:
                            self.asm.emit(f"{mov} {reg}, #{chunk1}, lsl #16")
                            mov = "movk"
                        self.asm.emit(f"{mov} {reg}, #{chunk0}")
                self.allocs[inst.reg.id] = alloc
            case ir.GetPtr():
                alloc = self.reg_allocator.allocate(inst.reg)
                match inst.src.typ:
                    case ir.Struct() as typ:
                        if typ == ir.Str:
                            src = inst.src
                            with self.reg_allocator.use_write(alloc) as [reg]:
                                self.asm.emit(f"adrp {reg}, .{src}@PAGE")
                                self.asm.emit(f"add {reg}, {reg}, .{src}@PAGEOFF")
                        else:
                            src = self.allocs[inst.src.id]
                            offset = DataLayout().field_offset(typ, inst.field)
                            with self.reg_allocator.use_write(alloc, src) as [reg, src_reg]:
                                self.asm.emit(f"add {reg}, {src_reg}, #{offset}")
                    case _:
                        raise AssertionError(f"Unknown type: {inst.reg.typ}")
                self.allocs[inst.reg.id] = alloc
            case ir.GetFnPtr():
                fn_name = self.fn_name(str(inst.src.fqn))
                alloc = self.reg_allocator.allocate(inst.reg)
                with self.reg_allocator.use_write(alloc) as [reg]:
                    self.asm.emit(f"adrp {reg}, {fn_name}@PAGE")
                    self.asm.emit(f"add {reg}, {reg}, {fn_name}@PAGEOFF")
                self.allocs[inst.reg.id] = alloc
            case ir.Load():
                alloc = self.reg_allocator.allocate(inst.reg)
                src = self.allocs[inst.src.id]
                assert isinstance(inst.src.typ, ir.Ptr)
                typ = inst.src.typ.typ
                with self.reg_allocator.use_write(alloc) as [reg], self.reg_allocator.use_read(src) as [src_reg]:
                    match typ:
                        case ir.Int():
                            if typ.bits <= 8:
                                self.asm.emit(f"ldrb {to_32bit(reg)}, [{src_reg}]")
                            elif typ.bits == 16:
                                self.asm.emit(f"ldrh {to_32bit(reg)}, [{src_reg}]")
                            elif typ.bits == 32:
                                self.asm.emit(f"ldr {to_32bit(reg)}, [{src_reg}]")
                            else:
                                self.asm.emit(f"ldr {reg}, [{src_reg}]")
                        case ir.Struct() | ir.Ptr() | ir.Fn():
                            self.asm.emit(f"ldr {reg}, [{src_reg}]")
                        case _:
                            raise AssertionError(f"Unexpected type: {typ}")
                self.allocs[inst.reg.id] = alloc
            case ir.Store():
                target = self.allocs[inst.target.id]
                src = self.allocs[inst.src.id]
                with (
                    self.reg_allocator.use_write(src) as [src_reg],
                    self.reg_allocator.use_read(target) as [target_reg],
                ):
                    self.asm.emit(f"str {src_reg}, [{target_reg}]")
            case ir.Call():
                with (
                    self.reg_allocator.use_with_registers([self.allocs[x.id] for x in inst.args], call_regs),
                    self.reg_allocator.with_spilled_caller_saved_regs(),
                ):
                    match inst.callee:
                        case str():
                            self.asm.emit(f"bl {self.fn_name(inst.callee)}")
                        case ir.Reg():
                            callee = self.allocs[inst.callee.id]
                            with self.reg_allocator.use_with_registers([callee], [indirect_fn_call_reg]):
                                self.asm.emit(f"blr {indirect_fn_call_reg}")
                        case _:
                            raise AssertionError(f"Unknown callee type: {inst.callee}")
                if inst.reg != ir.NoneReg:
                    alloc = self.reg_allocator.allocate(inst.reg)
                    with self.reg_allocator.use_write(alloc) as [reg]:
                        self.asm.emit(f"mov {reg}, x0")
                    self.allocs[inst.reg.id] = alloc
            case ir.Alloc():
                typ = inst.reg.typ
                assert isinstance(typ, ir.Struct)
                size = DataLayout.sizeof(typ)
                with self.reg_allocator.with_spilled_caller_saved_regs():
                    alloc = self.reg_allocator.allocate(inst.reg)
                    with self.reg_allocator.use_write(alloc) as [reg]:
                        self.asm.emit(f"mov x0, #{size}")
                        self.asm.emit("bl _malloc")
                        self.asm.emit(f"mov {reg}, x0")
                    self.allocs[inst.reg.id] = alloc
                for i, field in enumerate(inst.args):
                    offset = DataLayout.field_offset(typ, i)
                    field_alloc = self.allocs[field.id]
                    with (
                        self.reg_allocator.use_write(field_alloc) as [field_reg],
                        self.reg_allocator.use_read(alloc) as [reg],
                    ):
                        match DataLayout.sizeof(field.typ):
                            case 1:
                                self.asm.emit(f"strb {to_32bit(field_reg)}, [{reg}, #{offset}]")
                            case 2:
                                self.asm.emit(f"strh {to_32bit(field_reg)}, [{reg}, #{offset}]")
                            case 4:
                                self.asm.emit(f"str {to_32bit(field_reg)}, [{reg}, #{offset}]")
                            case _:
                                self.asm.emit(f"str {field_reg}, [{reg}, #{offset}]")
            case ir.IAddO() | ir.ISubO():
                asm_inst = ""
                match inst:
                    case ir.IAddO():
                        asm_inst = "add"
                    case ir.ISubO():
                        asm_inst = "sub"
                    case _:
                        raise AssertionError(f"Unknown instruction: {inst}")
                lhs = self.allocs[inst.lhs.id]
                rhs = self.allocs[inst.rhs.id]
                alloc = self.reg_allocator.allocate(inst.reg)
                with (
                    self.reg_allocator.use_write(alloc) as [reg],
                    self.reg_allocator.use_read(lhs) as [lhs_reg],
                    self.reg_allocator.use_read(rhs) as [rhs_reg],
                ):
                    self.asm.emit(f"{asm_inst} {reg}, {lhs_reg}, {rhs_reg}")
                self.allocs[inst.reg.id] = alloc
            case ir.ICmp():
                lhs = self.allocs[inst.lhs.id]
                rhs = self.allocs[inst.rhs.id]
                alloc = self.reg_allocator.allocate(inst.reg)
                with (
                    self.reg_allocator.use_write(alloc) as [reg],
                    self.reg_allocator.use_read(lhs) as [lhs_reg],
                    self.reg_allocator.use_read(rhs) as [rhs_reg],
                ):
                    self.asm.emit(f"cmp {lhs_reg}, {rhs_reg}")
                    self.asm.emit(f"cset {reg}, {inst.op.value}")
                self.allocs[inst.reg.id] = alloc
            case _:
                raise AssertionError(f"Unknown instruction: {inst}")


class ASM:
    lines: list[str]
    indent = 0

    def __init__(self) -> None:
        self.lines = []

    def emit(self, s: str, indent: int = 0) -> None:
        self.lines.append(" " * (self.indent + indent) * 4 + s)

    def extend(self, other: ASM | list[str]) -> None:
        if isinstance(other, ASM):
            other = other.lines
        self.lines.extend(other)

    def prepend(self, other: ASM | list[str]) -> None:
        if isinstance(other, ASM):
            other = other.lines
        self.lines = other + self.lines

    def inc_indent(self) -> None:
        self.indent += 1

    def dec_indent(self) -> None:
        self.indent -= 1

    def __str__(self) -> str:
        return "\n".join(self.lines)


def generate_builtins(asm: ASM) -> None:
    asm.emit(
        """
.print:
    stp fp, lr, [sp, #-32]!
    mov fp, sp
    ldr x1, [x0, 8]
    ldr x2, [x0]
    mov x0, 1
    bl _write
    ; Write a CR using the stack as the memory location
    mov w1, #13
    strb w1, [sp, #16]
    add x1, sp, #16
    mov x2, #1
    mov x0, #1
    bl _write
    ldp fp, lr, [sp], #32
    mov x0, xzr
    ret

.bool_to_str:
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    cmp x0, #1
    b.eq .bool_to_str_true
    adrp x0, .false@PAGE
    add x0, x0, .false@PAGEOFF
    b .bool_to_str_done
.bool_to_str_true:
    adrp x0, .true@PAGE
    add x0, x0, .true@PAGEOFF
.bool_to_str_done:
    ldp fp, lr, [sp], #16
    ret

.int_to_str:
    stp fp, lr, [sp, #-32]!
    mov fp, sp
    str x0, [fp, #16]
    mov x0, #36
    bl _malloc
    ldr x10, [fp, #16]
    mov x11, x10
    mov x5, x0
    mov x6, x0
    mov x1, #36
    add x5, x5, x1
    mov x7, x5
    cmp x10, 0
    b.ge .int_to_str_loop
    neg x10, x10
.int_to_str_loop:
    mov x1, #10
    udiv x2, x10, x1
    msub x3, x2, x1, x10
    add x3, x3, #48
    strb w3, [x5]
    sub x5, x5, #1
    mov x10, x2
    cmp x10, 0
    b.gt .int_to_str_loop
    cmp x11, 0
    b.ge .int_to_str_store
    mov x3, #45
    strb w3, [x5]
    sub x5, x5, #1
.int_to_str_store:
    add x5, x5, #1
    sub x1, x7, x5
    mov x2, 1
    add x1, x1, x2
    str x1, [x6]
    str x5, [x6, #8]
    mov x0, x6
    ldp fp, lr, [sp], #32
    ret
    """.strip()
    )


def generate(ir_: ir.IR) -> str:
    asm = ASM()
    asm.emit(".global _main")
    asm.emit(".text\n")
    generate_builtins(asm)
    for fn_ir in ir_.fn_irs:
        asm.emit("")
        asm.extend(FnGen(fn_ir).generate())
    asm.emit("\n.data")
    constants = dict(ir_.constant_pool)
    constants["true"] = ir.StrConst(ir.Reg("true", ir.Str), "true")
    constants["false"] = ir.StrConst(ir.Reg("false", ir.Str), "false")
    for text, const in constants.items():
        asm.emit(".align 3")
        asm.emit(f".{const.reg}_bytes:")
        asm.emit(f'    .ascii "{text}"')
        asm.emit(".align 3")
        asm.emit(f".{const.reg}:")
        asm.emit(f"    .quad {len(text)}")
        asm.emit(f"    .quad .{const.reg}_bytes")

    return str(asm)
