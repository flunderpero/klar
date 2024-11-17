package codegen

import (
	"fmt"
	"slices"
	"strings"

	"github.com/flunderpero/klar/bootstrap/ir"
	"github.com/flunderpero/klar/bootstrap/typed"
	"github.com/pkg/errors"
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
var callerSavedRegisters = []register{x9, x10, x11, x12, x13, x14, x15}
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
	allocations          map[ir.Register]*registerAllocation
	constraints          ir.RegisterConstraints
	code                 *Code
}

func newRegisterAllocator(registers []register, constraints ir.RegisterConstraints, code *Code) registerAllocator {
	return registerAllocator{
		registers:            registers,
		usedScratchRegisters: make(map[register]*registerAllocation),
		usedCallRegisters:    make(map[register]*registerAllocation),
		allocations:          make(map[ir.Register]*registerAllocation),
		constraints:          constraints,
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

func (r *registerAllocator) allocateScratchRegister(irReg ir.Register) *registerAllocation {
	if _, found := r.allocations[irReg]; found {
		panic(fmt.Sprintf("we should never try to create an allocation for the same IR register twice: %s", irReg))
	}
	// First look if the given IR register is part of a constraint.
	constrainedRegisters, isConstrained := r.constraints.Lookup(irReg)
	if isConstrained {
		for _, constrainedReg := range *constrainedRegisters {
			allocation, found := r.allocations[constrainedReg]
			if found {
				r.ensureInRegister(allocation)
				return allocation
			}
		}
	}
	var spillAllocation *registerAllocation
	for _, reg := range r.registers {
		allocation := r.usedScratchRegisters[reg]
		if allocation == nil {
			allocation := &registerAllocation{reg: reg, stackOffset: -1}
			r.usedScratchRegisters[reg] = allocation
			r.allocations[irReg] = allocation
			return allocation
		}
		spillAllocation = allocation
	}
	// No registers available, spill a random register to the stack.
	allocation := &registerAllocation{reg: spillAllocation.reg, stackOffset: -1}
	r.spill(spillAllocation)
	r.usedScratchRegisters[allocation.reg] = allocation
	r.allocations[irReg] = allocation
	return allocation
}

// Save the call result register x0 to a new register allocation so it doesn't get lost.
func (r *registerAllocator) saveCallResultRegister(reg ir.Register) *registerAllocation {
	allocation := r.allocateScratchRegister(reg)
	r.code.emit("mov %s, x0", allocation.reg)
	return allocation
}

// Spill currently used call argument registers from 0 to `mox`.
func (r *registerAllocator) spillCallRegisters(max int) {
	for i := 0; i <= max; i++ {
		reg := callArgsRegisters[i]
		usedAllocation, found := r.usedCallRegisters[reg]
		if !found {
			continue
		}
		r.spill(usedAllocation)
		delete(r.usedCallRegisters, reg)
	}
}

// Spill all caller saved registers in use.
func (r *registerAllocator) spillCallerSavedRegisters() []*registerAllocation {
	res := []*registerAllocation{}
	for _, reg := range callerSavedRegisters {
		usedAllocation, found := r.usedScratchRegisters[reg]
		if !found {
			continue
		}
		res = append(res, usedAllocation)
		// We remember the register the allocation occupied so we can restore it
		// in `restoreCallerSavedRegisters`.
		reg := usedAllocation.reg
		r.spill(usedAllocation)
		usedAllocation.reg = reg
		delete(r.usedScratchRegisters, reg)
	}
	return res
}

// Restore all spilled caller saved registers.
func (r *registerAllocator) restoreCallerSavedRegisters(allocations []*registerAllocation) {
	for _, allocation := range allocations {
		r.code.emit("ldr %s, [sp, #%d]", allocation.reg, allocation.stackOffset)
		r.usedScratchRegisters[allocation.reg] = allocation
	}
}

func (r *registerAllocator) allocateCallRegister(index int) *registerAllocation {
	reg := callArgsRegisters[index]
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
	function          *ir.FunctionDefinition
	stringConstants   *[]*ir.StrConst
	values            map[ir.RegisterId]*registerAllocation
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

func (c *Code) mustLookupRegisterAllocation(reg ir.Register) *registerAllocation {
	result, ok := c.values[reg.Id]
	if !ok {
		panic(fmt.Sprintf("Value not found for IR register: %s", reg))
	}
	return result
}

func (c *Code) blockLabel(block *ir.Block) string {
	return fmt.Sprintf("%s_%s", c.function.Id, block.Id)
}

func (c *Code) prepareBinaryOperation(resReg ir.Register, lhsReg ir.Register, rhsReg ir.Register) (reg *registerAllocation, lhs register, rhs register) {
	reg = c.registerAllocator.allocateScratchRegister(resReg)
	lhsAllocation := c.mustLookupRegisterAllocation(lhsReg)
	rhsAllocation := c.mustLookupRegisterAllocation(rhsReg)
	lhs = c.registerAllocator.ensureInRegister(lhsAllocation)
	rhs = c.registerAllocator.ensureInRegister(rhsAllocation)
	return reg, lhs, rhs
}

// It is not straight forward to load int values > 16bit. There are a lot of ways to optimize
// this, but that's an exercise for another day.
func (c *Code) generateIntImmediate(target register, value int64) {
	if value >= 0 && value <= 0xFFFF {
		c.emit("mov %s, #%d", target, value)
		return
	}
	chunk0 := value & 0xFFFF
	chunk1 := (value >> 16) & 0xFFFF
	chunk2 := (value >> 32) & 0xFFFF
	chunk3 := (value >> 48) & 0xFFFF
	mov := "movz"
	if chunk3 != 0 {
		c.emit("%s %s, #%d, lsl #48", mov, target, chunk3)
		mov = "movk"
	}
	if chunk2 != 0 {
		c.emit("%s %s, #%d, lsl #32", mov, target, chunk2)
		mov = "movk"
	}
	if chunk1 != 0 {
		c.emit("%s %s, #%d, lsl #16", mov, target, chunk1)
		mov = "movk"
	}
	c.emit("%s %s, #%d", mov, target, chunk0)
}

func (c *Code) generateBlock(block *ir.Block) error {
	c.emit("%s:", c.blockLabel(block))
	c.incIndent()
	for _, inst := range block.Instructions {
		switch inst := inst.(type) {
		case *ir.BoolConst:
			reg := c.registerAllocator.allocateScratchRegister(inst.Register())
			c.emit("mov %s, #%d", reg, inst.Value)
			c.values[inst.Register().Id] = reg
		case *ir.Int32Const:
			reg := c.registerAllocator.allocateScratchRegister(inst.Register())
			c.generateIntImmediate(reg.reg, inst.Value)
			c.values[inst.Register().Id] = reg
		case *ir.Int64Const:
			reg := c.registerAllocator.allocateScratchRegister(inst.Register())
			c.generateIntImmediate(reg.reg, inst.Value)
			c.values[inst.Register().Id] = reg
		case *ir.SignedInt64AddWithOverflow:
			reg, lhs, rhs := c.prepareBinaryOperation(inst.Register(), inst.Lhs, inst.Rhs)
			c.emit("adds %s, %s, %s", reg, lhs, rhs)
			c.values[inst.Register().Id] = reg
		case *ir.Int64Compare:
			reg, lhs, rhs := c.prepareBinaryOperation(inst.Register(), inst.Lhs, inst.Rhs)
			c.values[inst.Register().Id] = reg
			switch inst.Op {
			case ir.Int64CompOpEQ:
				c.emit("cmp %s, %s", lhs, rhs)
				c.emit("cset %s, eq", reg)
			default:
				return errors.Errorf("unknown comparison operator: %s", inst.Op)
			}
		case *ir.GetPointer:
			var reg *registerAllocation
			offset := 0
			if inst.FieldIndex > 0 {
				structType, ok := inst.SourceType.(*ir.StructType)
				if !ok {
					return errors.Errorf("expected a struct type, got: %T", inst.SourceType)
				}
				for _, field := range structType.Fields[:inst.FieldIndex] {
					offset += field.Size()
				}
			}
			switch source := inst.Source.(type) {
			case *ir.StrConst:
				reg = c.registerAllocator.allocateScratchRegister(inst.Register())
				c.emit("adrp %s, %s@PAGE", reg, source.Id)
				c.emit("add %s, %s, %s@PAGEOFF+%d", reg, reg, source.Id, offset)
			case ir.DefinedFunction:
				reg = c.registerAllocator.allocateScratchRegister(inst.Register())
				c.emit("adrp %s, %s@PAGE", reg, source.Id)
				c.emit("add %s, %s, %s@PAGEOFF+%d", reg, reg, source.Id, offset)
			case ir.Register:
				reg = c.mustLookupRegisterAllocation(source)
				if offset > 0 {
					source := c.registerAllocator.ensureInRegister(reg)
					reg = c.registerAllocator.allocateScratchRegister(inst.Register())
					c.emit("add %s, %s, #%d", reg, source, offset)
				}
			default:
				panic(fmt.Sprintf("unknown source type: %T", inst.Source))
			}
			c.values[inst.Register().Id] = reg
		case *ir.Load:
			source := c.registerAllocator.ensureInRegister(c.mustLookupRegisterAllocation(inst.Source))
			reg := c.registerAllocator.allocateScratchRegister(inst.Register())
			switch ty := inst.TargetType.(type) {
			case ir.BuiltInType:
				if ty != ir.Int64Type {
					// We need `wx` registers to load other types.
					return errors.Errorf("we don't know how to load a value of type %d yet", inst.TargetType)
				}
			case *ir.PointerType:
			default:
				return errors.Errorf("invalid target type for load instruction: %T", ty)
			}
			c.emit("ldr %s, [%s] ; %s", reg, source, inst.Register())
			c.values[inst.Register().Id] = reg
		case *ir.Store:
			target := c.registerAllocator.ensureInRegister(c.mustLookupRegisterAllocation(inst.Target))
			value := c.registerAllocator.ensureInRegister(c.mustLookupRegisterAllocation(inst.Value))
			switch ty := inst.ValueType.(type) {
			case ir.BuiltInType:
				if ty != ir.Int64Type {
					// We need `wx` registers to store other types.
					return errors.Errorf("we don't know how to store a value of type %d yet", inst.ValueType)

				}
			case *ir.PointerType:
			default:
				return errors.Errorf("invalid target type for load instruction: %T", ty)
			}
			c.emit("str %s, [%s]", value, target)
		case *ir.Call:
			c.registerAllocator.spillCallRegisters(len(c.function.Type.Args))
			savedCallerRegisters := c.registerAllocator.spillCallerSavedRegisters()
			for i, arg := range inst.Args {
				argReg := c.mustLookupRegisterAllocation(arg)
				c.registerAllocator.move(callArgsRegisters[i], argReg)
			}
			switch callee := inst.Callee.(type) {
			case ir.Register:
				reg := c.registerAllocator.ensureInRegister(c.mustLookupRegisterAllocation(callee))
				c.emit("blr %s", reg)
			case ir.DefinedFunction:
				c.emit("bl %s", callee.Id)
			default:
				panic(fmt.Sprintf("unknown callee type: %T", callee))
			}
			c.registerAllocator.restoreCallerSavedRegisters(savedCallerRegisters)
			if inst.FunctionType.ReturnType != ir.VoidType {
				allocation := c.registerAllocator.saveCallResultRegister(inst.Register())
				c.values[inst.Register().Id] = allocation
			}
		default:
			return errors.Errorf("unknown instruction: %T", inst)
		}
	}
	if block.Result.Type != ir.VoidType {
		// Move the value of the block expression to x0.
		resultAllocation := c.mustLookupRegisterAllocation(block.Result)
		c.registerAllocator.move(x0, resultAllocation)
	}
	switch terminator := block.Terminator.(type) {
	case *ir.Jump:
		c.emit("b %s", c.blockLabel(terminator.Target))
	case *ir.CondBranch:
		condRegister := c.mustLookupRegisterAllocation(terminator.Condition)
		c.emit("cbnz %s, %s", condRegister, c.blockLabel(terminator.TrueBlock))
		c.emit("b %s", c.blockLabel(terminator.FalseBlock))
	case *ir.Return:
		// Nothing to do, this is handled in `generateFunction`.
	default:
		return errors.Errorf("unknown terminator: %T", terminator)
	}
	c.decIndent()
	return nil
}

func generateFunction(function *ir.FunctionDefinition, module *ir.Module, isMain bool) (Code, error) {
	stackAllocator := &stackAllocator{size: 16}
	c := Code{
		function:        function,
		stringConstants: &module.Constants,
		values:          make(map[ir.RegisterId]*registerAllocation),
		stackAllocator:  stackAllocator,
	}
	c.registerAllocator = newRegisterAllocator(
		slices.Concat(callerSavedRegisters, calleeSavedRegisters),
		function.RegisterConstraints,
		&c,
	)
	for i, args := range function.Type.Args {
		c.values[args.Register.Id] = c.registerAllocator.allocateCallRegister(i)
	}
	if isMain {
		c.emit("_main:")
	} else {
		symbol := module.TypeInfo.MustLookupSymbol(function.Id)
		c.emit("; Function: %s", symbol.Name)
		c.emit("%s:", function.Id)
	}
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
	if isMain {
		// We have to implicitly return the status code (`0`) in `main`.
		c.emit("mov x0, xzr")
	}
	c.emit("ret")
	c.decIndent()
	return c, nil
}

func defineBuiltInPrintFunction(asm *ASMText) {
	asm.emit(
		`
; Function: print
%s:
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    ldr x1, [x0, 8]
    ldr x2, [x0]
    mov x0, 1
    bl _write
    ldp fp, lr, [sp], #16
    mov x0, xzr
    ret`, typed.BuiltInPrintFunction.Id())
}

func defineBuiltInPrintIntFunction(asm *ASMText) {
	asm.emit(
		`
; Function: print_int
%s:
    stp fp, lr, [sp, #-32]!
    mov fp, sp
    str x0, [sp]
    adrp x0, _print_int_format@PAGE
    add x0, x0, _print_int_format@PAGEOFF+0
    bl _printf
    mov x0, 0
    bl _fflush
    ldp fp, lr, [sp], #32
    mov x0, xzr
    ret`, typed.BuiltInPrintIntFunction.Id())
}

func defineBuiltInUnsafeMalloc(asm *ASMText) {
	asm.emit(
		`
; Function: __unsafe_malloc
%s:
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    bl _malloc
    cmp x0, #0
    bgt _success      
    adrp x0, _unsafe_malloc_failed@PAGE
    add x0, x0, _unsafe_malloc_failed@PAGEOFF
    bl _puts
    mov x0, #1
    bl _exit
_success:
    ldp fp, lr, [sp], #16
    ret`, typed.BuiltInUnsafeMallocFunction.Id())
}

func GenerateDarwinArm64ASM(irModule *ir.Module) (ASMText, error) {
	if irModule.Main == nil {
		panic("no main function found")
	}
	asm := ASMText{}
	asm.emit(".global _main")
	asm.emit(".text")
	defineBuiltInUnsafeMalloc(&asm)
	defineBuiltInPrintFunction(&asm)
	defineBuiltInPrintIntFunction(&asm)
	for _, function := range irModule.Functions {
		code, err := generateFunction(function, irModule, function == irModule.Main)
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
		asm.emit("%s_bytes:", constant.Id)
		asm.incIndent()
		asm.emit(".ascii %q", constant.Value)
		asm.decIndent()
		asm.emit(".align 3")
		asm.emit("%s:", constant.Id)
		asm.incIndent()
		asm.emit(".quad %d", len(constant.Value))
		asm.emit(".quad %s_bytes", constant.Id)
		asm.decIndent()
	}
	// Needed for builtin functions.
	asm.emit(".align 3")
	asm.emit("_print_int_format:")
	asm.incIndent().emit(".asciz \"%%lld\"").decIndent()
	asm.emit(".align 3")
	asm.emit("_unsafe_malloc_failed:")
	asm.incIndent().emit(".asciz \"out of memory\"").decIndent()
	return asm, nil
}
