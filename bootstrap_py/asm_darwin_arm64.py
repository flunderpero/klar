from __future__ import annotations

from contextlib import contextmanager
from dataclasses import dataclass
from typing import TYPE_CHECKING

from . import ir

if TYPE_CHECKING:
    from collections.abc import Generator

Reg = str

call_regs = [Reg(f"x{i}") for i in range(8)]
caller_saved = [Reg(f"x{i}") for i in range(9, 16)]
callee_saved = [Reg(f"x{i}") for i in range(19, 29)]
all_regs = call_regs + caller_saved + callee_saved
regs = caller_saved + callee_saved


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
    reg: Reg
    ir_reg: ir.Reg
    stack_offset: int = 0

    def allocate_on_stack_if_needed(self, stack_allocator: StackAllocator) -> int:
        if not self.stack_offset:
            self.stack_offset = stack_allocator.allocate(8)
        return self.stack_offset


class RegAllocator:
    regs: dict[ir.RegId, RegAlloc]
    used: dict[Reg, RegAlloc]
    next_free_reg_idx = 0
    stack_allocator: StackAllocator

    def __init__(self, stack_allocator: StackAllocator) -> None:
        self.used = {}
        self.regs = {}
        self.stack_allocator = stack_allocator

    def allocate(self, ir_reg: ir.Reg) -> RegAlloc:
        if self.next_free_reg_idx == len(regs) - 1:
            raise AssertionError("Out of registers")
        reg = regs[self.next_free_reg_idx]
        self.next_free_reg_idx += 1
        alloc = RegAlloc(reg, ir_reg)
        self.regs[ir_reg.id] = alloc
        self.used[reg] = alloc
        return alloc

    def move(self, reg: Reg, alloc: RegAlloc, asm: ASM) -> None:
        asm.emit(f"mov {reg}, {alloc.reg}")

    @contextmanager
    def with_spilled_caller_saved_regs(self, asm: ASM) -> Generator[None]:
        return self._with_spilled_saved_regs(caller_saved, asm)

    @contextmanager
    def with_spilled_callee_saved_regs(self, asm: ASM) -> Generator[None]:
        return self._with_spilled_saved_regs(callee_saved, asm)

    def _with_spilled_saved_regs(self, regs: list[Reg], asm: ASM) -> Generator[None]:
        allocs: list[RegAlloc] = []
        for reg in regs:
            if reg in self.used:
                alloc = self.used[reg]
                offset = alloc.allocate_on_stack_if_needed(self.stack_allocator)
                asm.emit(f"str {reg}, [fp, #{offset}]")
                allocs.append(alloc)
        yield
        for alloc in allocs:
            asm.emit(f"ldr {alloc.reg}, [fp, #{alloc.stack_offset}]")


class FnGen:
    asm: ASM
    ir: ir.FnIR
    reg_allocator: RegAllocator
    stack_allocator: StackAllocator
    ir_regs: dict[ir.RegId, RegAlloc]

    def __init__(self, ir: ir.FnIR) -> None:
        self.ir = ir
        self.asm = ASM()
        self.ir_regs = {}
        self.stack_allocator = StackAllocator()
        self.reg_allocator = RegAllocator(self.stack_allocator)

    def fn_name(self, name: str) -> str:
        if name == "main":
            return "_main"
        return f".{name}"

    def block_label(self, suffix: str | int) -> str:
        if isinstance(suffix, int):
            suffix = str(suffix)
        name = self.ir.fn_def.decl.name
        return f".{name}_{suffix}"

    def generate(self) -> ASM:
        # First generate all the code so that we know how large the stack frame will have to be
        # and which callee-saved registers have to be preserved.
        fn_name = self.fn_name(self.ir.fn_def.decl.name)
        self.asm.inc_indent()
        for i, param in enumerate(self.ir.params):
            alloc = self.reg_allocator.allocate(param.reg)
            self.ir_regs[param.reg.id] = alloc
            self.asm.emit(f"mov {alloc.reg}, x{i}")
        self.asm.dec_indent()
        for block in self.ir.blocks:
            self.asm.emit(f"{self.block_label(block.id)}:")
            self.asm.inc_indent()
            for inst in block.insts:
                self.inst(inst)
            self.asm.dec_indent()
        # Now generate the surrounding code.
        asm = ASM()
        asm.emit(f"{fn_name}:")
        # Prepare stack frame.
        asm.inc_indent()
        asm.emit(f"stp fp, lr, [sp, #-{self.stack_allocator.aligned_size()}]!")
        asm.emit("mov fp, sp")
        with self.reg_allocator.with_spilled_callee_saved_regs(asm):
            asm.dec_indent()
            # Add the generated code.
            asm.extend(self.asm)
            # Return block.
            asm.emit(f"{self.block_label('ret')}:")
            asm.inc_indent()
        # Release stack frame and return.
        asm.emit(f"ldp fp, lr, [sp], #{self.stack_allocator.aligned_size()}")
        if fn_name == "_main":
            asm.emit("mov x0, xzr")
        asm.emit("ret")
        asm.dec_indent()
        return asm

    def inst(self, inst: ir.Inst) -> None:
        match inst:
            case ir.IntConst():
                if inst.reg.typ == ir.I1:
                    assert inst.value in (0, 1), f"Invalid I1 value: {inst.value}"
                    alloc = self.reg_allocator.allocate(inst.reg)
                    self.asm.emit(f"movz {alloc.reg}, #{inst.value}")
                    self.ir_regs[inst.reg.id] = alloc
                    return
                assert inst.reg.typ == ir.I64, "For now, only I64 is supported"
                alloc = self.reg_allocator.allocate(inst.reg)
                # It is not straight forward to load int values > 16bit. There are a lot of ways to optimize
                # this, but that's an exercise for another day.
                value = inst.value
                if value >= 0 and value <= 0xFFFF:
                    self.asm.emit(f"movz {alloc.reg}, #{value}")
                else:
                    chunk0 = value & 0xFFFF
                    chunk1 = (value >> 16) & 0xFFFF
                    chunk2 = (value >> 32) & 0xFFFF
                    chunk3 = (value >> 48) & 0xFFFF
                    mov = "movz"
                    if chunk3 != 0:
                        self.asm.emit(f"{mov} {alloc.reg}, #{chunk3}, lsl #48")
                        mov = "movk"
                    if chunk2 != 0:
                        self.asm.emit(f"{mov} {alloc.reg}, #{chunk2}, lsl #32")
                        mov = "movk"
                    if chunk1 != 0:
                        self.asm.emit(f"{mov} {alloc.reg}, #{chunk1}, lsl #16")
                        mov = "movk"
                    self.asm.emit(f"{mov} {alloc.reg}, #{chunk0}")
                self.ir_regs[inst.reg.id] = alloc
            case ir.GetPtr():
                assert inst.field == 0, "For now, only field 0 is supported"
                alloc = self.reg_allocator.allocate(inst.reg)
                # For now, we know that the source is a string constant and we
                # know its asm label.
                src = inst.src
                self.asm.emit(f"adrp {alloc.reg}, .{src}@PAGE")
                self.asm.emit(f"add {alloc.reg}, {alloc.reg}, .{src}@PAGEOFF")
                self.ir_regs[inst.reg.id] = alloc
            case ir.Call():
                for i, arg in enumerate(inst.args):
                    alloc = self.ir_regs[arg.id]
                    self.reg_allocator.move(call_regs[i], alloc, self.asm)
                with self.reg_allocator.with_spilled_caller_saved_regs(self.asm):
                    self.asm.emit(f"bl {self.fn_name(inst.callee)}")
                if inst.reg != ir.NoneReg:
                    alloc = self.reg_allocator.allocate(inst.reg)
                    self.asm.emit(f"mov {alloc.reg}, x0")
                    self.ir_regs[inst.reg.id] = alloc
            case ir.IAddO() | ir.ISubO():
                asm_inst = ""
                match inst:
                    case ir.IAddO():
                        asm_inst = "add"
                    case ir.ISubO():
                        asm_inst = "sub"
                    case _:
                        raise AssertionError(f"Unknown instruction: {inst}")
                lhs = self.ir_regs[inst.lhs.id]
                rhs = self.ir_regs[inst.rhs.id]
                alloc = self.reg_allocator.allocate(inst.reg)
                self.asm.emit(f"{asm_inst} {alloc.reg}, {lhs.reg}, {rhs.reg}")
                self.ir_regs[inst.reg.id] = alloc
            case ir.Return():
                alloc = self.ir_regs[inst.reg.id]
                self.asm.emit(f"mov x0, {alloc.reg}")
                self.asm.emit(f"b {self.block_label('ret')}")
            case _:
                raise AssertionError(f"Unknown instruction: {inst}")


class ASM:
    lines: list[str]
    indent = 0

    def __init__(self) -> None:
        self.lines = []

    def emit(self, s: str, indent: int = 0) -> None:
        self.lines.append(" " * (self.indent + indent) * 4 + s)

    def extend(self, other: ASM) -> None:
        self.lines.extend(other.lines)

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
