/*
# Register Lifetime Analysis

This is a pass over the IR to calculate the lifetime of each register.
*/
package ir

import (
	"fmt"
)

type RegisterLifetime struct {
	Register      Register
	FirstBlock    *Block
	FirstBlockPos int
	LastBlock     *Block
	LastBlockPos  int
	Usages        int
}

type RegisterExpirations struct {
	expirations map[uint64][]Register
	lifetimes   map[RegisterId]*RegisterLifetime
}

// Return a list of registers expiring at the given position.
func (self *RegisterExpirations) Expired(block *Block, pos int) []Register {
	res, ok := self.expirations[self.key(block, pos)]
	if !ok {
		return []Register{}
	}
	return res
}

func (self *RegisterExpirations) key(block *Block, pos int) uint64 {
	return uint64(block.Id)<<32 | (uint64(pos))
}

func (self *RegisterExpirations) visitBlock(block *Block) error {
	for i, inst := range block.Instructions {
		if inst.Register() != NoneRegister {
			if _, ok := self.lifetimes[inst.Register().Id]; ok {
				panic(fmt.Sprintf("register %v already exists", inst.Register().Id))
			}
			self.lifetimes[inst.Register().Id] = &RegisterLifetime{
				Register:      inst.Register(),
				FirstBlock:    block,
				FirstBlockPos: i,
				LastBlock:     block,
				LastBlockPos:  i,
				Usages:        1,
			}
		}
		for _, reg := range inst.ParamRegisters() {
			lifetime := self.lifetimes[reg.Id]
			lifetime.LastBlock = block
			lifetime.LastBlockPos = i
			lifetime.Usages += 1
		}
	}
	for _, reg := range block.Terminator.Registers() {
		if reg == NoneRegister {
			continue
		}
		lifetime := self.lifetimes[reg.Id]
		lifetime.LastBlock = block
		lifetime.LastBlockPos = len(block.Instructions)
		lifetime.Usages += 1
	}
	return nil
}

func (self *RegisterExpirations) finalize() {
	for _, lifetime := range self.lifetimes {
		self.expirations[self.key(lifetime.LastBlock, lifetime.LastBlockPos)] =
			append(self.expirations[self.key(lifetime.LastBlock, lifetime.LastBlockPos)], lifetime.Register)
	}
}

func calculateRegisterExpirations(entyBlock *Block, paramRegs []Register) *RegisterExpirations {
	res := &RegisterExpirations{
		expirations: map[uint64][]Register{},
		lifetimes:   map[RegisterId]*RegisterLifetime{},
	}
	for _, reg := range paramRegs {
		res.lifetimes[reg.Id] = &RegisterLifetime{
			Register:      reg,
			FirstBlock:    entyBlock,
			FirstBlockPos: 0,
			LastBlock:     entyBlock,
			LastBlockPos:  0,
			Usages:        1,
		}
	}
	if err := WalkBlock(entyBlock, res.visitBlock); err != nil {
		panic(err)
	}
	res.finalize()
	return res
}
