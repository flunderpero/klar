from __future__ import annotations

from dataclasses import dataclass

from . import ir

Reg = str

call_regs = [Reg(f"x{i}") for i in range(8)]
caller_saved = [Reg(f"x{i}") for i in range(9, 16)]
callee_saved = [Reg(f"x{i}") for i in range(19, 29)]
all_regs = call_regs + caller_saved + callee_saved
regs = caller_saved + callee_saved


@dataclass
class RegAlloc:
    reg: Reg
    ir_reg: ir.Reg


class RegAllocator:
    regs: dict[ir.Reg, RegAlloc]
    next_free_reg_idx = 0
    asm: ASM

    def __init__(self, asm: ASM) -> None:
        self.used = {}
        self.asm = asm

    def allocate(self, ir_reg: ir.Reg) -> RegAlloc:
        if self.next_free_reg_idx == len(regs) - 1:
            raise AssertionError("Out of registers")
        reg = regs[self.next_free_reg_idx]
        self.next_free_reg_idx += 1
        return RegAlloc(reg, ir_reg)

    def move(self, reg: Reg, alloc: RegAlloc) -> None:
        self.asm.emit(f"mov {reg}, {alloc.reg}")


class FnGen:
    asm: ASM
    ir: ir.FnIR
    reg_allocator: RegAllocator
    ir_regs: dict[ir.RegId, RegAlloc]

    def __init__(self, ir: ir.FnIR, asm: ASM) -> None:
        self.ir = ir
        self.asm = asm
        self.ir_regs = {}
        self.reg_allocator = RegAllocator(asm)

    def fn_name(self, name: str) -> str:
        if name == "main":
            return "_main"
        return f".{name}"

    def generate(self) -> None:
        fn_name = self.fn_name(self.ir.fn_def.decl.name)
        self.asm.emit(f"{fn_name}:")
        # Prepare stack frame.
        self.asm.inc_indent()
        self.asm.emit("stp fp, lr, [sp, #-16]!")
        self.asm.emit("mov fp, sp")
        # Define parameters.
        for i, param in enumerate(self.ir.params):
            reg = self.reg_allocator.allocate(param.reg)
            self.ir_regs[param.reg.id] = reg
            self.asm.emit(f"mov {reg.reg}, x{i}")
        self.asm.dec_indent()
        for block in self.ir.blocks:
            self.asm.emit(f".{fn_name}_${block.id}:")
            self.asm.inc_indent()
            for inst in block.insts:
                self.inst(inst)
            self.asm.dec_indent()
        self.asm.emit(f"{fn_name}_ret:")
        self.asm.inc_indent()
        # Release stack frame and return.
        self.asm.emit("ldp fp, lr, [sp], #16")
        self.asm.emit("ret")
        self.asm.dec_indent()

    def inst(self, inst: ir.Inst) -> None:
        match inst:
            case ir.IntConst():
                if inst.reg.typ == ir.I1:
                    assert inst.value in (0, 1), f"Invalid I1 value: {inst.value}"
                    reg = self.reg_allocator.allocate(inst.reg)
                    self.asm.emit(f"movz {reg.reg}, #{inst.value}")
                    self.ir_regs[inst.reg.id] = reg
                    return
                assert inst.reg.typ == ir.I64, "For now, only I64 is supported"
                reg = self.reg_allocator.allocate(inst.reg)
                # It is not straight forward to load int values > 16bit. There are a lot of ways to optimize
                # this, but that's an exercise for another day.
                value = inst.value
                if value >= 0 and value <= 0xFFFF:
                    self.asm.emit(f"movz {reg.reg}, #{value}")
                else:
                    chunk0 = value & 0xFFFF
                    chunk1 = (value >> 16) & 0xFFFF
                    chunk2 = (value >> 32) & 0xFFFF
                    chunk3 = (value >> 48) & 0xFFFF
                    mov = "movz"
                    if chunk3 != 0:
                        self.asm.emit(f"{mov} {reg.reg}, #{chunk3}, lsl #48")
                        mov = "movk"
                    if chunk2 != 0:
                        self.asm.emit(f"{mov} {reg.reg}, #{chunk2}, lsl #32")
                        mov = "movk"
                    if chunk1 != 0:
                        self.asm.emit(f"{mov} {reg.reg}, #{chunk1}, lsl #16")
                        mov = "movk"
                    self.asm.emit(f"{mov} {reg.reg}, #{chunk0}")
                self.ir_regs[inst.reg.id] = reg
            case ir.GetPtr():
                assert inst.field == 0, "For now, only field 0 is supported"
                reg = self.reg_allocator.allocate(inst.reg)
                # For now, we know that the source is a string constant and we
                # know its asm label.
                src = inst.src
                self.asm.emit(f"adrp {reg.reg}, .{src}@PAGE")
                self.asm.emit(f"add {reg.reg}, {reg.reg}, .{src}@PAGEOFF")
                self.ir_regs[inst.reg.id] = reg
            case ir.Call():
                for i, arg in enumerate(inst.args):
                    reg = self.ir_regs[arg.id]
                    self.reg_allocator.move(call_regs[i], reg)
                self.asm.emit(f"bl {self.fn_name(inst.callee)}")
                if inst.reg != ir.NoneReg:
                    reg = self.reg_allocator.allocate(inst.reg)
                    self.asm.emit(f"mov {reg.reg}, x0")
                    self.ir_regs[inst.reg.id] = reg
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
                reg = self.reg_allocator.allocate(inst.reg)
                self.asm.emit(f"{asm_inst} {reg.reg}, {lhs.reg}, {rhs.reg}")
                self.ir_regs[inst.reg.id] = reg
            case ir.Return():
                reg = self.ir_regs[inst.reg.id]
                self.asm.emit(f"mov x0, {reg.reg}")
                self.asm.emit(f"b {self.fn_name(self.ir.fn_def.decl.name)}_ret")
            case _:
                raise AssertionError(f"Unknown instruction: {inst}")


class ASM:
    lines: list[str]
    indent = 0

    def __init__(self) -> None:
        self.lines = []

    def emit(self, s: str, indent: int = 0) -> None:
        self.lines.append(" " * (self.indent + indent) * 4 + s)

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
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    mov x20, x0
    mov x21, x0
    mov x0, #36
    bl _malloc
    mov x5, x0
    mov x6, x0
    mov x1, #36
    add x5, x5, x1
    mov x7, x5
    cmp x20, 0
    b.ge .int_to_str_loop
    neg x20, x20
.int_to_str_loop:
    mov x1, #10
    udiv x2, x20, x1
    msub x3, x2, x1, x20
    add x3, x3, #48
    strb w3, [x5]
    sub x5, x5, #1
    mov x20, x2
    cmp x20, 0
    b.gt .int_to_str_loop
    cmp x21, 0
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
    ldp fp, lr, [sp], #16
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
        FnGen(fn_ir, asm).generate()
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
