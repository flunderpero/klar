package codegen

import (
	"fmt"
	"slices"
	"strings"

	"github.com/flunderpero/klar/bootstrap/ir"
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
)

var calleeSavedRegisters = []Register{x19, x20, x21, x22, x23, x24, x25, x26, x27, x28}
var callArgsRegisters = []Register{x0, x1, x2, x3, x4, x5, x6, x7, x8}

type StackAllocator struct {
	size int
}

// Allocate `size` bytes on the stack and return the stack offset that can be used.
func (s *StackAllocator) Allocate(size int) int {
	result := s.size
	s.size += size
	return result
}

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
	registers            []Register
	usedScratchRegisters map[Register]*RegisterAllocation
	usedCallRegisters    map[Register]*RegisterAllocation
	allocations          []*RegisterAllocation
	code                 *Code
}

func NewRegisterAllocator(registers []Register, code *Code) RegisterAllocator {
	return RegisterAllocator{
		registers:            registers,
		usedScratchRegisters: make(map[Register]*RegisterAllocation),
		usedCallRegisters:    make(map[Register]*RegisterAllocation),
		allocations:          []*RegisterAllocation{},
		code:                 code,
	}
}

func (r *RegisterAllocator) UsedCalleeSavedRegisters() []Register {
	usedCalleeSaved := []Register{}
	for reg, _ := range r.usedScratchRegisters {
		if slices.Contains(calleeSavedRegisters, reg) {
			usedCalleeSaved = append(usedCalleeSaved, reg)
		}
	}
	slices.SortFunc(usedCalleeSaved, func(a Register, b Register) int {
		return strings.Compare(string(a), string(b))
	})
	return usedCalleeSaved
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
	var freeReg Register = ""
	for _, reg := range r.registers {
		usedAllocation := r.usedScratchRegisters[reg]
		if usedAllocation == nil {
			freeReg = reg
			break
		}
		spillAllocation = usedAllocation
	}
	if freeReg == "" {
		// No registers available, spill a random register to the stack.
		freeReg = spillAllocation.reg
		r.spill(spillAllocation)
	}
	allocation.reg = freeReg
	r.usedScratchRegisters[freeReg] = allocation
	r.code.emit("ldr %s, [sp, #%d]", freeReg, allocation.stackOffset)
	return freeReg
}

func (r *RegisterAllocator) Allocate() *RegisterAllocation {
	var spillAllocation *RegisterAllocation
	for _, reg := range r.registers {
		allocation := r.usedScratchRegisters[reg]
		if allocation == nil {
			allocation := &RegisterAllocation{reg: reg, stackOffset: -1}
			r.usedScratchRegisters[reg] = allocation
			r.allocations = append(r.allocations, allocation)
			return allocation
		}
		spillAllocation = allocation
	}
	// No registers available, spill a random register to the stack.
	allocation := &RegisterAllocation{reg: spillAllocation.reg, stackOffset: -1}
	r.spill(spillAllocation)
	r.usedScratchRegisters[allocation.reg] = allocation
	r.allocations = append(r.allocations, allocation)
	return allocation
}

// Allocate one of the call registers and spill any previous allocation
// for that register.
func (r *RegisterAllocator) AllocateCallRegister(index int) *RegisterAllocation {
	reg := callArgsRegisters[index]
	usedAllocation, found := r.usedCallRegisters[reg]
	if found {
		r.spill(usedAllocation)
	}
	allocation := &RegisterAllocation{reg: reg, stackOffset: -1}
	r.usedCallRegisters[reg] = allocation
	return allocation
}

func (r *RegisterAllocator) spill(allocation *RegisterAllocation) {
	if allocation.stackOffset == -1 {
		// This is the first time this allocation is spilled. Reserve the stack space.
		allocation.stackOffset = r.code.stackAllocator.Allocate(16)
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
	function          *ir.IRFunction
	stringConstants   *[]*ir.IRStringConst
	values            map[ir.IRRegister]*RegisterAllocation
	registerAllocator RegisterAllocator
	stackAllocator    *StackAllocator
}

func (c *Code) offset() int {
	return len(c.lines)
}

func (c *Code) emitAtOffset(offset int, s string, args ...any) *Code {
	c.lines = append(c.lines[:offset], append([]string{c.indent + fmt.Sprintf(s, args...)}, c.lines[offset:]...)...)
	return c
}

func (c *Code) mustLookupValue(reg ir.IRRegister) *RegisterAllocation {
	result, ok := c.values[reg]
	if !ok {
		panic(fmt.Sprintf("Value not found for IR register: %s", reg))
	}
	return result
}

func (c *Code) blockLabel(block *ir.IRBlock) string {
	return fmt.Sprintf("%s_%s", c.function.Name, block.Id)
}

func (c *Code) generateBlock(block *ir.IRBlock) error {
	c.emit("%s:", c.blockLabel(block))
	c.incIndent()
	for _, inst := range block.Instructions {
		switch inst := inst.(type) {
		case *ir.IRBoolConst:
			reg := c.registerAllocator.Allocate()
			c.emit("mov %s, %d", reg, inst.Value)
			c.values[inst.Register()] = reg
		case *ir.IRInt32Const:
			reg := c.registerAllocator.Allocate()
			c.emit("mov %s, %d", reg, inst.Value)
			c.values[inst.Register()] = reg
		case *ir.IRInt64Const:
			reg := c.registerAllocator.Allocate()
			c.emit("mov %s, %d", reg, inst.Value)
			c.values[inst.Register()] = reg
		case *ir.IRGetPtr:
			var reg *RegisterAllocation
			offset := 0
			if inst.FieldIndex > 0 {
				structType, ok := inst.Type.(*ir.IRStructType)
				if !ok {
					return fmt.Errorf("expected a struct type, got: %T", inst.Type)
				}
				for _, field := range structType.Fields[:inst.FieldIndex] {
					offset += field.Size()
				}
			}
			if inst.Source.IsConstant() {
				reg = c.registerAllocator.Allocate()
				c.emit("adrp %s, %s@PAGE", reg, inst.Source)
				c.emit("add %s, %s, %s@PAGEOFF+%d", reg, reg, inst.Source, offset)
			} else {
				reg = c.mustLookupValue(inst.Source)
				if offset > 0 {
					source := c.registerAllocator.EnsureInRegister(reg)
					reg = c.registerAllocator.Allocate()
					c.emit("add %s, %s, #%d", reg, source, offset)
				}
			}
			c.values[inst.Register()] = reg
		case *ir.IRLoad:
			source := c.registerAllocator.EnsureInRegister(c.mustLookupValue(inst.Source))
			reg := c.registerAllocator.Allocate()
			switch ty := inst.FieldType.(type) {
			case ir.IRBasicType:
				if ty == ir.IRInt64 {
					c.emit("ldr %s, [%s]", reg, source)
				} else {
					// We need `wx` registers to load other types.
					return fmt.Errorf("we don't know how to load a value of type %d yet", inst.FieldType)

				}
			case *ir.IRPointerType:
				c.emit("ldr %s, [%s]", reg, source)
			default:
				return fmt.Errorf("invalid target type for load instruction: %T", ty)
			}
			c.values[inst.Register()] = reg
		case *ir.IRCall:
			for i, arg := range inst.Args {
				argReg := c.mustLookupValue(arg)
				callArgAllocation := c.registerAllocator.AllocateCallRegister(i)
				c.registerAllocator.Move(callArgAllocation.reg, argReg)
			}
			if inst.Function.Name == "print" {
				c.emit("bl _write")
			} else {
				c.emit("bl _%s", inst.Function.Name)
			}
		default:
			return fmt.Errorf("unknown instruction: %T", inst)
		}
	}
	switch terminator := block.Terminator.(type) {
	case *ir.IRJump:
		c.emit("b %s", c.blockLabel(terminator.Target))
	case *ir.IRCondBranch:
		condRegister := c.mustLookupValue(terminator.Condition)
		c.emit("cbnz %s, %s", condRegister, c.blockLabel(terminator.TrueBlock))
		c.emit("b %s", c.blockLabel(terminator.FalseBlock))
	case *ir.IRReturn:
		// Nothing to do, this is handled in `generateFunction`.
	default:
		return fmt.Errorf("unknown terminator: %T", terminator)
	}
	c.decIndent()
	return nil
}

func generateFunction(function *ir.IRFunction, stringConstants *[]*ir.IRStringConst) (Code, error) {
	stackAllocator := &StackAllocator{size: 16}
	c := Code{
		function:        function,
		stringConstants: stringConstants,
		values:          make(map[ir.IRRegister]*RegisterAllocation),
		stackAllocator:  stackAllocator,
	}
	c.registerAllocator = NewRegisterAllocator(
		[]Register{x9, x10, x11, x12, x13, x14, x15, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28},
		&c,
	)
	for i, args := range function.Args {
		c.values[args.Register] = c.registerAllocator.AllocateCallRegister(i)
	}
	c.emit("_%s:", function.Name)
	// Remember the location where we will have to insert the correct stack frame setup.
	// We don't know the size of the stack yet, so we have to come back later and insert
	// the correct code.
	stackFrameSetupOffset := c.offset()
	// Generate the function body code.
	if err := ir.WalkBlock(function.Entry, c.generateBlock); err != nil {
		return c, err
	}
	c.incIndent()
	// Setup and clean up the stack frame.
	// First we have to preserve the callee saved registers (x19 .. x28).
	// We only preserve the registers we actually used.
	usedCalleeSaved := c.registerAllocator.UsedCalleeSavedRegisters()
	for i, reg := range usedCalleeSaved {
		stackOffset := stackAllocator.Allocate(16)
		c.emitAtOffset(stackFrameSetupOffset+i, "str %s, [sp, #%d]", reg, stackOffset)
		c.emit("ldr %s, [sp, #%d]", reg, stackOffset)
	}
	// Calculate the stack size needed, adjust the sp and save fp and lr.
	if stackAllocator.size <= 504 {
		// We can use the shorthand notation.
		c.emitAtOffset(stackFrameSetupOffset, "stp fp, lr, [sp, #-%d]!", stackAllocator.size)
		c.emitAtOffset(stackFrameSetupOffset+1, "mov fp, sp")
		c.emit("ldp fp, lr, [sp], #%d", stackAllocator.size)
	} else {
		// We have to update the sp offset explicitly.
		c.emitAtOffset(stackFrameSetupOffset, "sub sp, sp, #%d", stackAllocator.size)
		c.emitAtOffset(stackFrameSetupOffset+1, "stp fp, lr, [sp, #0]")
		c.emitAtOffset(stackFrameSetupOffset+2, "mov fp, sp")
		c.emit("ldp fp, lr, [sp]")
		c.emit("add sp, sp, #%d", stackAllocator.size)
	}
	if c.function.Name == "main" {
		// We have to implicitly return the status code (`0`) in `main`.
		c.emit("mov x0, xzr")
	}
	c.emit("ret")
	c.decIndent()
	return c, nil
}

func GenerateDarwinArm64ASM(irModule *ir.IRModule) (ASMText, error) {
	asm := ASMText{}
	asm.emit(".global _main")
	asm.emit(".text")
	for _, function := range irModule.Functions {
		code, err := generateFunction(function, &irModule.Constants)
		if err != nil {
			return asm, err
		}
		asm.emit("")
		asm.lines = append(asm.lines, code.lines...)
	}
	asm.emit("")
	asm.emit(".data")
	for _, constant := range irModule.Constants {
		asm.emit(".align 3")
		asm.emit("%s_bytes:", constant.Register())
		asm.incIndent()
		asm.emit(".ascii \"%s\"", constant.Value)
		asm.decIndent()
		asm.emit(".align 3")
		asm.emit("%s:", constant.Register())
		asm.incIndent()
		asm.emit(".quad %d", len(constant.Value))
		asm.emit(".quad %s_bytes", constant.Register())
		asm.decIndent()
	}
	return asm, nil
}
