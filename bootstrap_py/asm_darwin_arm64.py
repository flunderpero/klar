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
        self.asm.dec_indent()
        for block in self.ir.blocks:
            self.asm.emit(f".{fn_name}_${block.id}:")
            self.asm.inc_indent()
            for inst in block.insts:
                self.inst(inst)
            self.asm.dec_indent()
        self.asm.inc_indent()
        # Release stack frame and return.
        self.asm.emit("ldp fp, lr, [sp], #16")
        self.asm.emit("mov x0, xzr")
        self.asm.emit("ret")
        self.asm.dec_indent()

    def inst(self, inst: ir.Inst) -> None:
        match inst:
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
                assert inst.reg == ir.NoneReg, "Return values are not supported yet"
                for i, arg in enumerate(inst.args):
                    reg = self.ir_regs[arg.id]
                    self.reg_allocator.move(call_regs[i], reg)
                self.asm.emit(f"bl {self.fn_name(inst.callee)}")


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
    for text, const in ir_.constant_pool.items():
        asm.emit(".align 3")
        asm.emit(f".{const.reg}_bytes:")
        asm.emit(f'    .ascii "{text}"')
        asm.emit(".align 3")
        asm.emit(f".{const.reg}:")
        asm.emit(f"    .quad {len(text)}")
        asm.emit(f"    .quad .{const.reg}_bytes")

    return str(asm)
