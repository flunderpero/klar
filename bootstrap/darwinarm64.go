package main

import (
	"fmt"
	"strings"
)

type Register string

const (
	// Function call parameters / result:
	x0 Register = "x0"
	x1 Register = "x1"
	x2 Register = "x2"
	x3 Register = "x3"
	x4 Register = "x4"
	x5 Register = "x5"
	x6 Register = "x6"
	x7 Register = "x7"
	x8 Register = "x8"

	// Scratch registers (caller saved):
	x9  Register = "x9"
	x10 Register = "x10"
	x11 Register = "x11"
	x12 Register = "x12"
	x13 Register = "x13"
	x14 Register = "x14"
	x15 Register = "x15"

	// Scratch registers (callee saved):
	x19 Register = "x19"
	x20 Register = "x20"
	x21 Register = "x21"
	x22 Register = "x22"
	x23 Register = "x23"
	x24 Register = "x24"
	x25 Register = "x25"
	x26 Register = "x26"
	x27 Register = "x27"
	x28 Register = "x28"
	x29 Register = "x29"
)

type RegisterAllocation struct {
	reg         Register
	stackOffset int
}

func (ra *RegisterAllocation) IsInRegister() bool {
	return ra.reg != ""
}

func (ra *RegisterAllocation) String() string {
	if !ra.IsInRegister() {
		panic("Cannot stringify a register allocation that is not in a register")
	}
	return string(ra.reg)
}

type RegisterAllocator struct {
	registers     []Register
	usedRegisters map[Register]*RegisterAllocation
	allocations   []*RegisterAllocation
	stackSize     int
	code          *Code
}

func NewRegisterAllocator(registers []Register, code *Code) RegisterAllocator {
	return RegisterAllocator{
		registers:     registers,
		usedRegisters: make(map[Register]*RegisterAllocation),
		allocations:   []*RegisterAllocation{},
		stackSize:     0,
		code:          code,
	}
}

func (r *RegisterAllocator) Move(target Register, allocation *RegisterAllocation) {
	if allocation.IsInRegister() {
		if allocation.reg != target {
			r.code.emit("mov %s, %s", target, allocation.reg)
		}
	} else {
		r.code.emit("ldr %s, [sp, #%d]", target, allocation.stackOffset)
	}
}

func (r *RegisterAllocator) EnsureInRegister(allocation *RegisterAllocation) Register {
	if allocation.IsInRegister() {
		return allocation.reg
	}
	var spillAllocation *RegisterAllocation
	for _, reg := range r.registers {
		usedAllocation := r.usedRegisters[reg]
		if usedAllocation == nil {
			allocation.reg = reg
			r.usedRegisters[reg] = allocation
			return reg
		}
		spillAllocation = usedAllocation
	}
	// No registers available, spill a random register to the stack.
	reg := spillAllocation.reg
	r.spill(spillAllocation)
	return reg
}

func (r *RegisterAllocator) Allocate() *RegisterAllocation {
	var spillAllocation *RegisterAllocation
	for _, reg := range r.registers {
		allocation := r.usedRegisters[reg]
		if allocation == nil {
			allocation := &RegisterAllocation{reg: reg, stackOffset: -1}
			r.usedRegisters[reg] = allocation
			r.allocations = append(r.allocations, allocation)
			return allocation
		}
		spillAllocation = allocation
	}
	// No registers available, spill a random register to the stack.
	allocation := &RegisterAllocation{reg: spillAllocation.reg, stackOffset: -1}
	r.spill(spillAllocation)
	r.usedRegisters[allocation.reg] = allocation
	r.allocations = append(r.allocations, allocation)
	return allocation
}

func (r *RegisterAllocator) spill(allocation *RegisterAllocation) {
	if r.usedRegisters[allocation.reg] != allocation {
		panic(fmt.Sprintf("The allocation is not in the register: %s", allocation.reg))
	}
	r.usedRegisters[allocation.reg] = nil
	if allocation.stackOffset == -1 {
		// This is the first time this allocation is spilled. Reserve the stack space.
		allocation.stackOffset = r.stackSize
		r.stackSize += 16
	}
	r.code.emit("str %s, [sp, #%d]", allocation.reg, allocation.stackOffset)
	allocation.reg = ""
}

type Code struct {
	indent            string
	lines             []string
	stringConstants   []*IRStringConst
	values            map[IRRegister]*RegisterAllocation
	registerAllocator RegisterAllocator
}

func (c *Code) String() string {
	return strings.Join(c.lines, "\n")
}

func (c *Code) emit(s string, args ...any) *Code {
	c.lines = append(c.lines, c.indent+fmt.Sprintf(s, args...))
	return c
}

func (c *Code) addStringConst(constant *IRStringConst) {
	c.stringConstants = append(c.stringConstants, constant)
}

func (c *Code) incIndent() *Code {
	c.indent += "    "
	return c
}

func (c *Code) decIndent() *Code {
	c.indent = c.indent[:len(c.indent)-4]
	return c
}

func (c *Code) mustLookupValue(reg IRRegister) *RegisterAllocation {
	result, ok := c.values[reg]
	if !ok {
		panic(fmt.Sprintf("Value not found for IR register: %s", reg))
	}
	return result
}

func (c *Code) generateBlock(block *IRBlock) error {
	callArgRegs := []Register{x0, x1, x2, x3, x4, x5, x6, x7, x8}
	c.emit("%s:", block.Id)
	c.incIndent()
	for _, inst := range block.Instructions {
		switch inst := inst.(type) {
		case *IRBoolConst:
			reg := c.registerAllocator.Allocate()
			c.emit("mov %s, %d", reg, inst.Value)
			c.values[inst.Register()] = reg
		case *IRInt32Const:
			reg := c.registerAllocator.Allocate()
			c.emit("mov %s, %d", reg, inst.Value)
			c.values[inst.Register()] = reg
		case *IRInt64Const:
			reg := c.registerAllocator.Allocate()
			c.emit("mov %s, %d", reg, inst.Value)
			c.values[inst.Register()] = reg
		case *IRGetPtr:
			reg := c.registerAllocator.Allocate()
			// For now we only know about constant strings.
			c.emit("adrp %s, str_%d@PAGE", reg, inst.Source)
			c.emit("add %s, %s, str_%d@PAGEOFF", reg, reg, inst.Source)
			c.values[inst.Register()] = reg
		case *IRCall:
			if inst.Function.Name != "print" {
				panic("For now we only know about the print function")
			}
			// Move the arguments to the argument registers (x0 .. x8)
			for i, arg := range inst.Args {
				argReg := c.mustLookupValue(arg)
				callArgReg := callArgRegs[i]
				c.registerAllocator.Move(callArgReg, argReg)
			}
			c.emit("bl _write")
		case *IRStringConst:
			c.addStringConst(inst)
		default:
			return fmt.Errorf("unknown instruction: %T", inst)
		}
	}
	switch terminator := block.Terminator.(type) {
	case *IRJump:
		c.emit("b %s", terminator.Target.Id)
	case *IRCondBranch:
		condRegister := c.mustLookupValue(terminator.Condition)
		c.emit("cbnz %s, %s", condRegister, terminator.TrueBlock.Id)
		c.emit("b %s", terminator.FalseBlock.Id)
	case *IRReturn:
	default:
		return fmt.Errorf("unknown terminator: %T", terminator)
	}
	c.decIndent()
	return nil
}

func GenerateDarwinArm64ASM(block *IRBlock) (Code, error) {
	c := Code{
		values: make(map[IRRegister]*RegisterAllocation),
		indent: "",
		lines:  []string{},
	}
	c.registerAllocator = NewRegisterAllocator(
		[]Register{x9, x10, x11, x12, x13, x14, x15, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29},
		&c,
	)
	c.emit(".global _main")
	c.emit(".text")
	c.emit("")
	c.emit("_main:")
	if err := WalkBlock(block, c.generateBlock); err != nil {
		return c, err
	}
	c.incIndent()
	c.emit("mov x0, #0")
	c.emit("bl _exit")
	c.decIndent()
	c.emit("")
	c.emit(".data")
	c.incIndent()
	for _, constant := range c.stringConstants {
		c.emit("str_%d:", constant.Register())
		c.incIndent().emit(".ascii \"%s\"", constant.Value).decIndent()
	}
	c.decIndent()
	return c, nil
}
