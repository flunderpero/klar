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

type ASMText struct {
	lines  []string
	indent string
}

func (asm *ASMText) String() string {
	return strings.Join(asm.lines, "\n")
}

func (asm *ASMText) emit(s string, args ...any) *ASMText {
	asm.lines = append(asm.lines, asm.indent+fmt.Sprintf(s, args...))
	return asm
}

func (asm *ASMText) incIndent() *ASMText {
	asm.indent += "    "
	return asm
}

func (c *ASMText) decIndent() *ASMText {
	c.indent = c.indent[:len(c.indent)-4]
	return c
}

type Code struct {
	ASMText
	function          *IRFunction
	stringConstants   *[]*IRStringConst
	values            map[IRRegister]*RegisterAllocation
	registerAllocator RegisterAllocator
}

func (c *Code) addStringConst(constant *IRStringConst) {
	*c.stringConstants = append(*c.stringConstants, constant)
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
			// Move the arguments to the argument registers (x0 .. x8)
			for i, arg := range inst.Args {
				argReg := c.mustLookupValue(arg)
				callArgReg := callArgRegs[i]
				c.registerAllocator.Move(callArgReg, argReg)
			}
			if inst.Function.Name == "print" {
				c.emit("bl _write")
			} else {
				c.emit("bl _%s", inst.Function.Name)
			}
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
		// Restore the stack frame.
		c.emit("ldp fp, lr, [sp], #16")
		if c.function.Name == "main" {
			// We have to implicitly return the status code in `main`.
			c.emit("mov x0, xzr")
		}
		c.emit("ret")
	default:
		return fmt.Errorf("unknown terminator: %T", terminator)
	}
	c.decIndent()
	return nil
}

func generateFunction(function *IRFunction, stringConstants *[]*IRStringConst) (Code, error) {
	c := Code{
		function:        function,
		stringConstants: stringConstants,
		values:          make(map[IRRegister]*RegisterAllocation),
	}
	c.registerAllocator = NewRegisterAllocator(
		[]Register{x9, x10, x11, x12, x13, x14, x15, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29},
		&c,
	)
	c.emit("_%s:", function.Name)
	// Prepare the stack frame.
	c.incIndent()
	c.emit("stp fp, lr, [sp, #-16]!")
	c.emit("mov fp, sp")
	c.decIndent()
	if err := WalkBlock(function.Entry, c.generateBlock); err != nil {
		return c, err
	}
	return c, nil
}

func GenerateDarwinArm64ASM(irModule *IRModule) (ASMText, error) {
	asm := ASMText{}
	stringConstants := &[]*IRStringConst{}
	asm.emit(".global _main")
	asm.emit(".text")
	for _, function := range irModule.Functions {
		code, err := generateFunction(function, stringConstants)
		if err != nil {
			return asm, nil
		}
		asm.emit("")
		asm.lines = append(asm.lines, code.lines...)
	}
	asm.emit("")
	asm.emit(".data")
	for _, constant := range *stringConstants {
		asm.emit("str_%d:", constant.Register())
		asm.incIndent().emit(".ascii \"%s\"", constant.Value).decIndent()
	}
	return asm, nil
}
