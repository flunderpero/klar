from __future__ import annotations

from collections import defaultdict
from dataclasses import dataclass

from . import ir


@dataclass
class GetPtrInfo:
    inst: ir.GetPtr
    block: ir.Block
    duplicates: int

    def matches(self, b: ir.GetPtr) -> bool:
        return self.inst.src == b.src and self.inst.field == b.field


def ir_remove_duplicate_getptr(fn_ir: ir.FnIR) -> None:
    """Remove duplicate GetPtr instructions and hoist them after the instruction that creates
    `GetPtr.src` instruction.
    """
    seen: dict[ir.Reg, list[GetPtrInfo]] = defaultdict(list)
    replace_regs: dict[ir.Reg, ir.Reg] = {}

    # Pass 1: Find all GetPtr instructions that are duplicates.
    for block in fn_ir.blocks:
        i = 0
        while i < len(block.insts):
            inst = block.insts[i]
            if isinstance(inst, ir.GetPtr):
                candidates = seen.get(inst.src)
                existing = next((x for x in candidates if x.matches(inst)), None) if candidates else None
                if existing is not None:
                    replace_regs[inst.reg] = existing.inst.reg
                    block.insts.pop(i)
                    i -= 1
                    existing.duplicates += 1
                else:
                    seen[inst.src].append(GetPtrInfo(inst, block, 0))
            i += 1

    # Remove all GetPtr instructions from `seen` that are not duplicated.
    for src, infos in list(seen.items()):
        infos = [x for x in infos if x.duplicates > 0]
        if not infos:
            del seen[src]
            continue
        seen[src] = infos

    # Pass 2: Replace registers in all instructions and hoist GetPtr instructions.
    for block in fn_ir.blocks:
        i = 0
        while i < len(block.insts):
            inst = block.insts[i]
            if inst.reg in seen:
                # Hoist the GetPtr instructions.
                for info in seen[inst.reg]:
                    block.insts.insert(i + 1, info.inst)
                    if block.id != info.block.id:
                        i += 1
                    info.block.insts.remove(info.inst)
            replace_reg(inst, replace_regs)
            i += 1


def replace_reg(inst: ir.Inst, replace_regs: dict[ir.Reg, ir.Reg]) -> None:
    match inst:
        case ir.Load():
            if inst.src in replace_regs:
                inst.src = replace_regs[inst.src]
        case ir.IAddO() | ir.ISubO():
            if inst.lhs in replace_regs:
                inst.lhs = replace_regs[inst.lhs]
            if inst.rhs in replace_regs:
                inst.rhs = replace_regs[inst.rhs]
        case ir.Store():
            if inst.src in replace_regs:
                inst.src = replace_regs[inst.src]
        case ir.Call():
            if isinstance(inst.callee, ir.Reg) and inst.callee in replace_regs:
                inst.callee = replace_regs[inst.callee]
            for i, arg in enumerate(inst.args):
                if arg in replace_regs:
                    inst.args[i] = replace_regs[arg]
        case ir.ICmp():
            if inst.lhs in replace_regs:
                inst.lhs = replace_regs[inst.lhs]
            if inst.rhs in replace_regs:
                inst.rhs = replace_regs[inst.rhs]
        case ir.Phi():
            for i, phi in enumerate(inst.incoming):
                if phi.reg in replace_regs:
                    inst.incoming[i].reg = replace_regs[phi.reg]
        case ir.IntConst() | ir.Alloc() | ir.GetPtr() | ir.GetFnPtr():
            pass
        case _:
            raise AssertionError(f"Unhandled inst: {inst.__class__}")
