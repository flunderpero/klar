from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from . import ir

type InstPos = int


@dataclass
class RegisterLifetime:
    reg: ir.Reg
    """The last position inside each block where the register is alive."""
    alive: dict[ir.BlockId, int]
    blocks: list[ir.Block]

    def can_be_evicted(self, block: ir.Block, pos: int) -> bool:
        """A register can be evicted if block and pos point to the last usage of the register
        and no other block can reenter the given block.
        """
        if len(self.alive) > 1:
            # todo: Registers spanning multiple blocks should also be checked for liveness.
            return False
        if block.id not in self.alive:
            return False
        if pos != self.alive[block.id]:
            return False
        # Only if the alive block cannot be reentered, the register is not alive.
        for b in self.blocks:
            if not b.terminator:
                continue
            if any(x for x in b.terminator.successors() if x.id == block.id):
                return False
        return True

    def is_used_in_one_block_only(self) -> bool:
        """Check if the register is used in one block only and that block is not entered again."""
        if len(self.alive) != 1:
            return False
        block_id = next(iter(self.alive.values()))
        for b in self.blocks:
            if not b.terminator:
                continue
            if any(x for x in b.terminator.successors() if x.id == block_id):
                return False
        return True


def calculate_register_lifetimes(fn_ir: ir.FnIR) -> dict[ir.RegId, RegisterLifetime]:
    """For now, we only try to detect whether registers are alive in a single or multiple blocks."""
    res: dict[ir.RegId, RegisterLifetime] = {}
    for param in fn_ir.params:
        if param.reg.id in res:
            res[param.reg.id].alive[fn_ir.blocks[0].id] = 0
        else:
            res[param.reg.id] = RegisterLifetime(param.reg, {fn_ir.blocks[0].id: 0}, fn_ir.blocks)
    for block in fn_ir.blocks:
        for pos, inst in enumerate(block.insts):
            for reg in inst.regs():
                if reg.id in res:
                    res[reg.id].alive[block.id] = pos
                else:
                    res[reg.id] = RegisterLifetime(reg, {block.id: pos}, fn_ir.blocks)
        if not block.terminator:
            continue
        for reg in block.terminator.regs():
            if reg.id in res:
                res[reg.id].alive[block.id] = len(block.insts)
            else:
                res[reg.id] = RegisterLifetime(reg, {block.id: len(block.insts)}, fn_ir.blocks)
    return res
