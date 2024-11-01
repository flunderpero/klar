package codegen

import (
	"fmt"
	"slices"
	"strings"

	"github.com/flunderpero/klar/bootstrap/ir"
)

type register string

const (
	// Function call parameters / result:
	x0 register = "x0"
	x1 register = "x1"
	x2 register = "x2"
	x3 register = "x3"
	x4 register = "x4"
	x5 register = "x5"
	x6 register = "x6"
	x7 register = "x7"
	x8 register = "x8"

	// Scratch registers (caller saved):
	x9  register = "x9"
	x10 register = "x10"
	x11 register = "x11"
	x12 register = "x12"
	x13 register = "x13"
	x14 register = "x14"
	x15 register = "x15"

	// Scratch registers (callee saved):
	x19 register = "x19"
	x20 register = "x20"
	x21 register = "x21"
	x22 register = "x22"
	x23 register = "x23"
	x24 register = "x24"
	x25 register = "x25"
	x26 register = "x26"
	x27 register = "x27"
	x28 register = "x28"
)

var calleeSavedRegisters = []register{x19, x20, x21, x22, x23, x24, x25, x26, x27, x28}
var callArgsRegisters = []register{x0, x1, x2, x3, x4, x5, x6, x7, x8}

type stackAllocator struct {
	size int
}

// allocate `size` bytes on the stack and return the stack offset that can be used.
func (s *stackAllocator) allocate(size int) int {
	result := s.size
	s.size += size
	return result
}

type registerAllocation struct {
	reg         register
	stackOffset int
}

func (ra *registerAllocation) isInRegister() bool {
	return ra.reg != ""
}

func (ra *registerAllocation) String() string {
	if !ra.isInRegister() {
		panic("Cannot stringify a register allocation that is not in a register")
	}
	return string(ra.reg)
}

type registerAllocator struct {
	registers            []register
	usedScratchRegisters map[register]*registerAllocation
	usedCallRegisters    map[register]*registerAllocation
	allocations          []*registerAllocation
	code                 *Code
}

func newRegisterAllocator(registers []register, code *Code) registerAllocator {
	return registerAllocator{
		registers:            registers,
		usedScratchRegisters: make(map[register]*registerAllocation),
		usedCallRegisters:    make(map[register]*registerAllocation),
		allocations:          []*registerAllocation{},
		code:                 code,
	}
}

func (r *registerAllocator) usedCalleeSavedRegisters() []register {
	usedCalleeSaved := []register{}
	for reg, _ := range r.usedScratchRegisters {
		if slices.Contains(calleeSavedRegisters, reg) {
			usedCalleeSaved = append(usedCalleeSaved, reg)
		}
	}
	slices.SortFunc(usedCalleeSaved, func(a register, b register) int {
		return strings.Compare(string(a), string(b))
	})
	return usedCalleeSaved
}

func (r *registerAllocator) move(target register, allocation *registerAllocation) {
	if allocation.isInRegister() {
		if allocation.reg != target {
			r.code.emit("mov %s, %s", target, allocation.reg)
		}
	} else {
		r.code.emit("ldr %s, [sp, #%d]", target, allocation.stackOffset)
	}
}

func (r *registerAllocator) ensureInRegister(allocation *registerAllocation) register {
	if allocation.isInRegister() {
		return allocation.reg
	}
	var spillAllocation *registerAllocation
	var freeReg register = ""
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

func (r *registerAllocator) allocateScratchRegister() *registerAllocation {
	var spillAllocation *registerAllocation
	for _, reg := range r.registers {
		allocation := r.usedScratchRegisters[reg]
		if allocation == nil {
			allocation := &registerAllocation{reg: reg, stackOffset: -1}
			r.usedScratchRegisters[reg] = allocation
			r.allocations = append(r.allocations, allocation)
			return allocation
		}
		spillAllocation = allocation
	}
	// No registers available, spill a random register to the stack.
	allocation := &registerAllocation{reg: spillAllocation.reg, stackOffset: -1}
	r.spill(spillAllocation)
	r.usedScratchRegisters[allocation.reg] = allocation
	r.allocations = append(r.allocations, allocation)
	return allocation
}

// Allocate one of the call registers and spill any previous allocation
// for that register.
func (r *registerAllocator) allocateCallRegister(index int) *registerAllocation {
	reg := callArgsRegisters[index]
	usedAllocation, found := r.usedCallRegisters[reg]
	if found {
		r.spill(usedAllocation)
	}
	allocation := &registerAllocation{reg: reg, stackOffset: -1}
	r.usedCallRegisters[reg] = allocation
	return allocation
}

func (r *registerAllocator) spill(allocation *registerAllocation) {
	if allocation.stackOffset == -1 {
		// This is the first time this allocation is spilled. Reserve the stack space.
		allocation.stackOffset = r.code.stackAllocator.allocate(16)
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
	function          *ir.Function
	stringConstants   *[]*ir.StrConst
	values            map[ir.Register]*registerAllocation
	registerAllocator registerAllocator
	stackAllocator    *stackAllocator
}

func (c *Code) offset() int {
	return len(c.lines)
}

func (c *Code) emitAtOffset(offset int, s string, args ...any) *Code {
	c.lines = append(c.lines[:offset], append([]string{c.indent + fmt.Sprintf(s, args...)}, c.lines[offset:]...)...)
	return c
}

func (c *Code) mustLookupValue(reg ir.Register) *registerAllocation {
	result, ok := c.values[reg]
	if !ok {
		panic(fmt.Sprintf("Value not found for IR register: %s", reg))
	}
	return result
}

func (c *Code) blockLabel(block *ir.Block) string {
	return fmt.Sprintf("%s_%s", c.function.Name, block.Id)
}

func (c *Code) generateBlock(block *ir.Block) error {
	c.emit("%s:", c.blockLabel(block))
	c.incIndent()
	for _, inst := range block.Instructions {
		switch inst := inst.(type) {
		case *ir.BoolConst:
			reg := c.registerAllocator.allocateScratchRegister()
			c.emit("mov %s, %d", reg, inst.Value)
			c.values[inst.Register()] = reg
		case *ir.Int32Const:
			reg := c.registerAllocator.allocateScratchRegister()
			c.emit("mov %s, %d", reg, inst.Value)
			c.values[inst.Register()] = reg
		case *ir.Int64Const:
			reg := c.registerAllocator.allocateScratchRegister()
			c.emit("mov %s, %d", reg, inst.Value)
			c.values[inst.Register()] = reg
		case *ir.GetPointer:
			var reg *registerAllocation
			offset := 0
			if inst.FieldIndex > 0 {
				structType, ok := inst.Type.(*ir.StructType)
				if !ok {
					return fmt.Errorf("expected a struct type, got: %T", inst.Type)
				}
				for _, field := range structType.Fields[:inst.FieldIndex] {
					offset += field.Size()
				}
			}
			if inst.Source.IsConstant() {
				reg = c.registerAllocator.allocateScratchRegister()
				c.emit("adrp %s, %s@PAGE", reg, inst.Source)
				c.emit("add %s, %s, %s@PAGEOFF+%d", reg, reg, inst.Source, offset)
			} else {
				reg = c.mustLookupValue(inst.Source)
				if offset > 0 {
					source := c.registerAllocator.ensureInRegister(reg)
					reg = c.registerAllocator.allocateScratchRegister()
					c.emit("add %s, %s, #%d", reg, source, offset)
				}
			}
			c.values[inst.Register()] = reg
		case *ir.Load:
			source := c.registerAllocator.ensureInRegister(c.mustLookupValue(inst.Source))
			reg := c.registerAllocator.allocateScratchRegister()
			switch ty := inst.FieldType.(type) {
			case ir.BuiltInType:
				if ty == ir.Int64Type {
					c.emit("ldr %s, [%s]", reg, source)
				} else {
					// We need `wx` registers to load other types.
					return fmt.Errorf("we don't know how to load a value of type %d yet", inst.FieldType)

				}
			case *ir.PointerType:
				c.emit("ldr %s, [%s]", reg, source)
			default:
				return fmt.Errorf("invalid target type for load instruction: %T", ty)
			}
			c.values[inst.Register()] = reg
		case *ir.Call:
			for i, arg := range inst.Args {
				argReg := c.mustLookupValue(arg)
				callArgAllocation := c.registerAllocator.allocateCallRegister(i)
				c.registerAllocator.move(callArgAllocation.reg, argReg)
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
	case *ir.Jump:
		c.emit("b %s", c.blockLabel(terminator.Target))
	case *ir.CondBranch:
		condRegister := c.mustLookupValue(terminator.Condition)
		c.emit("cbnz %s, %s", condRegister, c.blockLabel(terminator.TrueBlock))
		c.emit("b %s", c.blockLabel(terminator.FalseBlock))
	case *ir.Return:
		// Nothing to do, this is handled in `generateFunction`.
	default:
		return fmt.Errorf("unknown terminator: %T", terminator)
	}
	c.decIndent()
	return nil
}

func generateFunction(function *ir.Function, stringConstants *[]*ir.StrConst) (Code, error) {
	stackAllocator := &stackAllocator{size: 16}
	c := Code{
		function:        function,
		stringConstants: stringConstants,
		values:          make(map[ir.Register]*registerAllocation),
		stackAllocator:  stackAllocator,
	}
	c.registerAllocator = newRegisterAllocator(
		[]register{x9, x10, x11, x12, x13, x14, x15, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28},
		&c,
	)
	for i, args := range function.Args {
		c.values[args.Register] = c.registerAllocator.allocateCallRegister(i)
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
	usedCalleeSaved := c.registerAllocator.usedCalleeSavedRegisters()
	for i, reg := range usedCalleeSaved {
		stackOffset := stackAllocator.allocate(16)
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

func GenerateDarwinArm64ASM(irModule *ir.Module) (ASMText, error) {
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
