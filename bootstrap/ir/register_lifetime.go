/*
# Register Lifetime Analysis

This is a pass over the IR to calculate the lifetime of each register.
*/
package ir

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

func (self *RegisterExpirations) update(block *Block, blockPos int, reg Register) {
	if reg == NoneRegister {
		return
	}
	lifetime, ok := self.lifetimes[reg.Id]
	if !ok {
		lifetime = &RegisterLifetime{
			Register:      reg,
			FirstBlock:    block,
			FirstBlockPos: blockPos,
			LastBlock:     block,
			LastBlockPos:  blockPos,
			Usages:        1,
		}
		self.lifetimes[reg.Id] = lifetime
		return
	}
	lifetime.LastBlock = block
	lifetime.LastBlockPos = len(block.Instructions)
	lifetime.Usages += 1
}

func (self *RegisterExpirations) visitBlock(block *Block) error {
	for blockPos, inst := range block.Instructions {
		self.update(block, blockPos, inst.Register())
		for _, reg := range inst.ParamRegisters() {
			self.update(block, blockPos, reg)
		}
	}
	for _, reg := range block.Terminator.Registers() {
		self.update(block, len(block.Instructions), reg)
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
