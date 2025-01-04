package codegen

import (
	"fmt"
	"slices"
	"strings"

	"github.com/flunderpero/klar/bootstrap/ir"
	"github.com/flunderpero/klar/bootstrap/typed"
	"github.com/pkg/errors"
)

const DEBUG = true

type DataLayout struct{}

func (self DataLayout) SizeOf(ty ir.Type) int {
	switch ty := ty.(type) {
	case ir.IntType:
		switch ty {
		case ir.Int1Type:
			return 1
		case ir.Int8Type, ir.UInt8Type:
			return 1
		case ir.Int16Type, ir.UInt16Type:
			return 2
		case ir.Int32Type, ir.UInt32Type:
			return 4
		case ir.Int64Type, ir.UInt64Type:
			return 8
		default:
			panic(fmt.Sprintf("Unknown integer type: %s", ty))
		}
	case ir.NoneType:
		return 0
	case *ir.PointerType:
		return 8
	case *ir.StructType:
		if len(ty.Fields) == 0 {
			return 0
		}
		size := self.FieldOffset(ty, len(ty.Fields)-1)
		lastField := ty.Fields[len(ty.Fields)-1]
		size += self.SizeOf(lastField)
		size = (size + 8 - 1) &^ (8 - 1)
		return size
	}
	panic(fmt.Sprintf("Unknown type: %T", ty))
}

func (self DataLayout) Alignment(ty ir.Type) int {
	switch ty := ty.(type) {
	case ir.IntType, ir.NoneType, *ir.PointerType:
		return self.SizeOf(ty)
	}
	panic(fmt.Sprintf("Unknown type: %T", ty))
}

func (self DataLayout) FieldOffset(ty *ir.StructType, index int) int {
	offset := 0
	for i := 0; i < index; i++ {
		offset += self.SizeOf(ty.Fields[i])
		alignment := self.Alignment(ty.Fields[i]) - 1
		offset = (offset + alignment) &^ (alignment)
	}
	return offset
}

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

func (r register) to32bit() register {
	return register(strings.Replace(string(r), "x", "w", 1))
}

var calleeSavedRegisters = []register{x19, x20, x21, x22, x23, x24, x25, x26, x27, x28}
var callerSavedRegisters = []register{x9, x10, x11, x12, x13, x14, x15}
var callArgsRegisters = []register{x0, x1, x2, x3, x4, x5, x6, x7, x8}

type stackAllocator struct {
	size int
}

func (s *stackAllocator) alignedSize() int {
	return (s.size + 16 - 1) &^ (16 - 1)
}

// allocate `size` bytes on the stack and return the stack offset that can be used.
func (s *stackAllocator) allocate(size int) int {
	result := s.size
	s.size += size
	return result
}

type registerAllocation struct {
	irReg       ir.Register
	permReg     register
	tempReg     register
	stackOffset int
	constraints *[]ir.Register
}

func (self *registerAllocation) isInRegister() bool {
	return self.permReg != "" || self.tempReg != ""
}

func (self *registerAllocation) isTemp() bool {
	return self.permReg == ""
}

func (self *registerAllocation) reg() register {
	reg := self.permReg
	if reg == "" {
		reg = self.tempReg
	}
	if reg == "" {
		panic("Cannot stringify a register allocation that is not in a register")
	}
	return reg
}

func (self *registerAllocation) String() string {
	return string(self.reg())
}

type registerAllocator struct {
	permRegisters           []register
	tempRegisters           []register
	allocationsByIRReg      map[ir.RegisterId]*registerAllocation
	allocationsByRegister   map[register]*registerAllocation
	usedRegisters           map[register]bool
	constraints             *ir.RegisterConstraints
	callerSavedStackOffsets []int
}

func newRegisterAllocator(registers []register, constraints *ir.RegisterConstraints) *registerAllocator {
	return &registerAllocator{
		permRegisters:         registers[3:],
		tempRegisters:         registers[0:3],
		allocationsByIRReg:    make(map[ir.RegisterId]*registerAllocation),
		allocationsByRegister: make(map[register]*registerAllocation),
		usedRegisters:         make(map[register]bool),
		constraints:           constraints,
	}
}

func (self *registerAllocator) usedCalleeSavedRegisters() []register {
	res := []register{}
	for _, reg := range calleeSavedRegisters {
		if _, ok := self.usedRegisters[reg]; ok {
			res = append(res, reg)
		}
	}
	return res
}

func (self *registerAllocator) move(target register, allocation *registerAllocation, code *blockCode) {
	if allocation.isInRegister() {
		if allocation.reg() != target {
			code.emit("mov %s, %s", target, allocation)
		}
	} else {
		code.emit("ldr %s, [sp, #%d]", target, allocation.stackOffset)
	}
}

func (self *registerAllocator) ensureInRegister(allocation *registerAllocation, code *blockCode) {
	if allocation.isInRegister() {
		return
	}
	self.assignTempRegister(allocation)
	code.emitDebug("load temp %s", allocation.irReg)
	code.emit("ldr %s, [sp, #%d]", allocation.tempReg, allocation.stackOffset)
}

func (self *registerAllocator) assignTempRegister(allocation *registerAllocation) {
	if !allocation.isTemp() {
		panic(fmt.Sprintf("allocation is not temporary: %s", allocation))
	}
	for _, tempReg := range self.tempRegisters {
		if _, ok := self.allocationsByRegister[tempReg]; ok {
			continue
		}
		allocation.tempReg = tempReg
		self.usedRegisters[tempReg] = true
		self.allocationsByRegister[tempReg] = allocation
		return
	}

	panic("all temp registers are used")
}

func (self *registerAllocator) spillIfTempAllocation(allocation *registerAllocation, code *blockCode) {
	if allocation.isTemp() {
		if allocation.tempReg == "" {
			panic(fmt.Sprintf("allocation is temporary but has no register assigned: %s", allocation))
		}
		code.emitDebug("spill temp %s", allocation.irReg)
		code.emit("str %s, [sp, #%d]", allocation.tempReg, allocation.stackOffset)
		delete(self.allocationsByRegister, allocation.tempReg)
		allocation.tempReg = ""
	}
}

// If the allocation is a temp allocation, release the temp register.
func (self *registerAllocator) releaseIfTempAllocation(allocation *registerAllocation) {
	if allocation.isTemp() {
		if allocation.tempReg == "" {
			panic(fmt.Sprintf("allocation is temporary but has no temp register: %s", allocation))
		}
		delete(self.allocationsByRegister, allocation.tempReg)
		allocation.tempReg = ""
	}
}

func (self *registerAllocator) allocate(irReg ir.Register, code *blockCode) *registerAllocation {
	if _, found := self.allocationsByIRReg[irReg.Id]; found {
		panic(fmt.Sprintf("we should never try to create an allocation for the same IR register twice: %s", irReg))
	}
	// First look if the given IR register is part of a constraint.
	constrainedRegisters, isConstrained := self.constraints.Lookup(irReg)
	if isConstrained {
		for _, constrainedReg := range *constrainedRegisters {
			allocation, found := self.allocationsByIRReg[constrainedReg.Id]
			if found {
				self.allocationsByIRReg[irReg.Id] = allocation
				self.ensureInRegister(allocation, code)
				return allocation
			}
		}
	}
	// Do we still have a permRegisters free?
	for _, reg := range self.permRegisters {
		if _, ok := self.allocationsByRegister[reg]; ok {
			continue
		}
		allocation := &registerAllocation{irReg: irReg, permReg: reg, constraints: constrainedRegisters}
		self.allocationsByIRReg[irReg.Id] = allocation
		self.allocationsByRegister[reg] = allocation
		self.usedRegisters[reg] = true
		return allocation
	}
	// Ok, we need to allocate a spill register.
	allocation := &registerAllocation{irReg: irReg, stackOffset: code.stackAllocator.allocate(16), constraints: constrainedRegisters}
	self.allocationsByIRReg[irReg.Id] = allocation
	self.assignTempRegister(allocation)
	return allocation
}

func (self *registerAllocator) forget(irReg ir.Register, c *blockCode) {
	allocation, ok := self.allocationsByIRReg[irReg.Id]
	if !ok {
		panic(fmt.Sprintf("we should never try to forget an allocation that doesn't exist: %s", irReg))
	}
	c.emitDebug("forget %s (perm: %s, temp: %s, stack: %d) ", irReg, allocation.permReg, allocation.tempReg, allocation.stackOffset)
	if allocation.constraints != nil && len(*allocation.constraints) > 0 {
		newConstraints := []ir.Register{}
		for _, reg := range *allocation.constraints {
			if reg.Id != irReg.Id {
				newConstraints = append(newConstraints, reg)
			}
		}
		if len(newConstraints) > 0 {
			allocation.constraints = &newConstraints
			return
		}
	}
	// base.Debug("forget", irReg, "found", ok, "map", self.allocationsByIRReg)
	if allocation.isInRegister() {
		delete(self.allocationsByRegister, allocation.reg())
	}
	// delete(self.allocationsByIRReg, irReg.Id)
	allocation.permReg = register(fmt.Sprintf("destroyed perm: %s (perm: %s, temp: %s, stack: %d)", irReg, allocation.permReg, allocation.tempReg, allocation.stackOffset))
	allocation.tempReg = register(fmt.Sprintf("destroyed temp: %s (perm: %s, temp: %s, stack: %d)", irReg, allocation.permReg, allocation.tempReg, allocation.stackOffset))
}

// Save the call result register x0 to a new register allocation so it doesn't get lost.
func (self *registerAllocator) saveCallResultRegister(reg ir.Register, code *blockCode) *registerAllocation {
	allocation := self.allocate(reg, code)
	code.emit("mov %s, x0", allocation)
	self.spillIfTempAllocation(allocation, code)
	return allocation
}

type callerSaved struct {
	reg         register
	stackOffset int
}

// Store all caller saved registers in use on the stack.
func (self *registerAllocator) saveCallerSavedRegisters(code *blockCode) []callerSaved {
	res := []callerSaved{}
	for i, callerSavedReg := range callerSavedRegisters {
		if allocation, ok := self.allocationsByRegister[callerSavedReg]; ok {
			for i >= len(self.callerSavedStackOffsets) {
				self.callerSavedStackOffsets = append(self.callerSavedStackOffsets, code.stackAllocator.allocate(16))
			}
			stackOffset := self.callerSavedStackOffsets[i]
			code.emit("str %s, [sp, #%d]", allocation, stackOffset)
			res = append(res, callerSaved{callerSavedReg, stackOffset})
		}
	}
	return res
}

// Restore all saved caller saved registers.
func (r *registerAllocator) restoreCallerSavedRegisters(callerSaved []callerSaved, code *blockCode) {
	for _, saved := range callerSaved {
		code.emit("ldr %s, [sp, #%d]", saved.reg, saved.stackOffset)
	}
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

func (asm *ASMText) emitDebug(s string, args ...any) *ASMText {
	if DEBUG {
		return asm.emit("; "+s, args...)
	}
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

func funcName(id typed.TypeId, typeInfo *typed.TypeInfo) string {
	fqn := typeInfo.MustLookupSymbol(id).FQN()
	return "." + strings.ReplaceAll(fqn, "::", "$$")
}

type blockCode struct {
	ASMText
	block             *ir.Block
	registerAllocator *registerAllocator
	stackAllocator    *stackAllocator
	function          *ir.FunctionDefinition
	values            map[ir.RegisterId]*registerAllocation
	dataLayout        ir.DataLayout
	typeInfo          *typed.TypeInfo
}

func (c *blockCode) funcName(id typed.TypeId) string {
	return funcName(id, c.typeInfo)
}

func (c *blockCode) mustLookupRegisterAllocation(reg ir.Register) *registerAllocation {
	result, ok := c.values[reg.Id]
	if !ok {
		panic(fmt.Sprintf("Value not found for IR register: %s", reg))
	}
	return result
}

func (c *blockCode) blockLabel(block *ir.Block) string {
	return fmt.Sprintf("%s_%s", c.funcName(c.function.Id), block.Id)
}

// It is not straight forward to load int values > 16bit. There are a lot of ways to optimize
// this, but that's an exercise for another day.
func (c *blockCode) generateIntImmediate(target register, value int64) {
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

func (c *blockCode) sign_extend_or_zero_extend(reg register, ty ir.IntType) {
	switch ty {
	case ir.Int1Type, ir.Int8Type:
		c.emit("sxtb %s, %s", reg, reg.to32bit())
	case ir.Int16Type:
		c.emit("sxth %s, %s", reg, reg.to32bit())
	case ir.Int32Type:
		c.emit("sxtw %s, %s", reg, reg)
	case ir.Int64Type:
		// Nothing to do.
	case ir.UInt8Type:
		c.emit("uxtb %s, %s", reg, reg.to32bit())
	case ir.UInt16Type:
		c.emit("uxth %s, %s", reg, reg.to32bit())
	case ir.UInt32Type:
		c.emit("uxtw %s, %s", reg, reg)
	case ir.UInt64Type:
		// Nothing to do.
	default:
		panic(fmt.Sprintf("we don't know how to sign extend or zero extend a value of type %q yet", ty))
	}
}

func (c *blockCode) generateBlock(block *ir.Block) error {
	c.emit("%s:", c.blockLabel(block))
	c.incIndent()
	for i, inst := range block.Instructions {
		c.emitDebug("%s", inst)
		switch inst := inst.(type) {
		case *ir.Local:
			reg := c.registerAllocator.allocate(inst.Register(), c)
			c.values[inst.Register().Id] = reg
			c.registerAllocator.spillIfTempAllocation(reg, c)
		case *ir.BoolConst:
			reg := c.registerAllocator.allocate(inst.Register(), c)
			c.emit("mov %s, #%d", reg, inst.Value)
			c.values[inst.Register().Id] = reg
			c.registerAllocator.spillIfTempAllocation(reg, c)
		case *ir.UIntConst:
			reg := c.registerAllocator.allocate(inst.Register(), c)
			c.generateIntImmediate(reg.reg(), int64(inst.Value))
			c.values[inst.Register().Id] = reg
			c.registerAllocator.spillIfTempAllocation(reg, c)
		case *ir.IntConst:
			reg := c.registerAllocator.allocate(inst.Register(), c)
			c.generateIntImmediate(reg.reg(), inst.Value)
			c.values[inst.Register().Id] = reg
			c.registerAllocator.spillIfTempAllocation(reg, c)
		case ir.BinaryInst:
			reg := c.registerAllocator.allocate(inst.Register(), c)
			lhs := c.mustLookupRegisterAllocation(inst.Lhs())
			rhs := c.mustLookupRegisterAllocation(inst.Rhs())
			c.registerAllocator.ensureInRegister(lhs, c)
			c.registerAllocator.ensureInRegister(rhs, c)
			switch inst := inst.(type) {
			case *ir.SignedIntAddWithOverflow:
				c.emit("adds %s, %s, %s", reg, lhs, rhs)
				c.sign_extend_or_zero_extend(reg.reg(), inst.Type)
			case *ir.UnsignedIntAddWithOverflow:
				c.emit("add %s, %s, %s", reg, lhs, rhs)
				c.sign_extend_or_zero_extend(reg.reg(), inst.Type)
			case *ir.IntMultiplicationWithOverflow:
				c.emit("mul %s, %s, %s", reg, lhs, rhs)
				c.sign_extend_or_zero_extend(reg.reg(), inst.Type)
			case *ir.SignedIntDivision:
				// Check if inst.RHS is zero and panic.
				c.emit("cmp %s, #0", rhs)
				c.emit("b.eq .internal_panic_divide_by_zero")
				c.emit("sdiv %s, %s, %s", reg, lhs, rhs)
			case *ir.UnsignedIntDivision:
				// Check if inst.RHS is zero and panic.
				c.emit("cmp %s, #0", rhs)
				c.emit("b.eq .internal_panic_divide_by_zero")
				c.emit("udiv %s, %s, %s", reg, lhs, rhs)
			case *ir.SignedIntModulo:
				// Check if inst.RHS is zero and panic.
				c.emit("cmp %s, #0", rhs)
				c.emit("b.eq .internal_panic_divide_by_zero")
				c.emit("sdiv %s, %s, %s", reg, lhs, rhs)
				c.emit("msub %s, %s, %s, %s", reg, reg, rhs, lhs)
			case *ir.UnsignedIntModulo:
				// Check if inst.RHS is zero and panic.
				c.emit("cmp %s, #0", rhs)
				c.emit("b.eq .internal_panic_divide_by_zero")
				c.emit("udiv %s, %s, %s", reg, lhs, rhs)
				c.emit("msub %s, %s, %s, %s", reg, reg, rhs, lhs)
			case *ir.IntCompare:
				c.values[inst.Register().Id] = reg
				c.emit("cmp %s, %s", lhs, rhs)
				c.emit("cset %s, %s", reg, inst.Op)
			case *ir.BinaryLogic:
				var op string
				switch inst.Op {
				case ir.BinaryLogicOpAnd:
					op = "and"
				case ir.BinaryLogicOpOr:
					op = "orr"
				default:
					return errors.Errorf("unknown binary logic operation: %s", inst.Op)
				}
				c.emit("%s %s, %s, %s", op, reg, lhs, rhs)
			}
			c.values[inst.Register().Id] = reg
			c.registerAllocator.spillIfTempAllocation(reg, c)
			c.registerAllocator.releaseIfTempAllocation(lhs)
			if lhs != rhs {
				c.registerAllocator.releaseIfTempAllocation(rhs)
			}
		case *ir.UnaryLogic:
			reg := c.registerAllocator.allocate(inst.Register(), c)
			value := c.mustLookupRegisterAllocation(inst.Value)
			c.registerAllocator.ensureInRegister(value, c)
			c.values[inst.Register().Id] = reg
			c.emit("eor %s, %s, #1", reg, value)
			c.registerAllocator.spillIfTempAllocation(reg, c)
			c.registerAllocator.releaseIfTempAllocation(value)
		case *ir.GetPointer:
			var reg *registerAllocation
			offset := 0
			if inst.FieldIndex > 0 {
				structType, ok := inst.SourceType.(*ir.StructType)
				if !ok {
					return errors.Errorf("expected a struct type, got: %T", inst.SourceType)
				}
				offset = c.dataLayout.FieldOffset(structType, inst.FieldIndex)
			}
			switch source := inst.Source.(type) {
			case *ir.StrConst:
				reg = c.registerAllocator.allocate(inst.Register(), c)
				c.emit("adrp %s, %s@PAGE", reg, source.Id)
				c.emit("add %s, %s, %s@PAGEOFF+%d", reg, reg, source.Id, offset)
				c.registerAllocator.spillIfTempAllocation(reg, c)
			case ir.DefinedFunction:
				reg = c.registerAllocator.allocate(inst.Register(), c)
				sourceName := c.funcName(source.Id)
				c.emit("adrp %s, %s@PAGE", reg, sourceName)
				c.emit("add %s, %s, %s@PAGEOFF+%d", reg, reg, sourceName, offset)
				c.registerAllocator.spillIfTempAllocation(reg, c)
			case ir.Register:
				sourceReg := c.mustLookupRegisterAllocation(source)
				c.registerAllocator.ensureInRegister(sourceReg, c)
				reg = c.registerAllocator.allocate(inst.Register(), c)
				if offset == 0 {
					// todo: If the offset is 0 we can actually just re-use `sourceReg`.
					//       But for this to work we have to extend the lifetime of
					//       `sourceReg.irReg` to at least the lifetime of `inst.Register()`.
					c.emit("mov %s, %s", reg, sourceReg)
				} else {
					c.emit("add %s, %s, #%d", reg, sourceReg, offset)
				}
				c.registerAllocator.releaseIfTempAllocation(sourceReg)
				c.registerAllocator.spillIfTempAllocation(reg, c)
			default:
				panic(fmt.Sprintf("unknown source type: %T", inst.Source))
			}
			c.values[inst.Register().Id] = reg
		case *ir.Load:
			source := c.mustLookupRegisterAllocation(inst.Source)
			c.registerAllocator.ensureInRegister(source, c)
			reg := c.registerAllocator.allocate(inst.Register(), c)
			switch ty := inst.TargetType.(type) {
			case ir.IntType:
				switch ty {
				case ir.Int1Type, ir.Int8Type, ir.UInt8Type:
					c.emit("ldrb %s, [%s]", reg.reg().to32bit(), source)
				case ir.Int16Type, ir.UInt16Type:
					c.emit("ldrh %s, [%s]", reg.reg().to32bit(), source)
				case ir.Int32Type, ir.UInt32Type:
					c.emit("ldr %s, [%s]", reg.reg().to32bit(), source)
				case ir.Int64Type, ir.UInt64Type:
					c.emit("ldr %s, [%s]", reg, source)
				default:
					return errors.Errorf("we don't know how to load a value of type %q yet", inst.TargetType)
				}
			case *ir.PointerType:
				c.emit("ldr %s, [%s]", reg, source)
			default:
				return errors.Errorf("invalid target type for load instruction: %T", ty)
			}
			c.values[inst.Register().Id] = reg
			c.registerAllocator.spillIfTempAllocation(reg, c)
			c.registerAllocator.releaseIfTempAllocation(source)
		case *ir.Store:
			target := c.mustLookupRegisterAllocation(inst.Target)
			value := c.mustLookupRegisterAllocation(inst.Value)
			c.registerAllocator.ensureInRegister(target, c)
			c.registerAllocator.ensureInRegister(value, c)
			switch ty := inst.Type.(type) {
			case ir.IntType:
				switch ty {
				case ir.Int1Type, ir.Int8Type, ir.UInt8Type:
					c.emit("strb %s, [%s]", value.reg().to32bit(), target)
				case ir.Int16Type, ir.UInt16Type:
					c.emit("strh %s, [%s]", value.reg().to32bit(), target)
				case ir.Int32Type, ir.UInt32Type:
					c.emit("str %s, [%s]", value.reg().to32bit(), target)
				case ir.Int64Type, ir.UInt64Type:
					c.emit("str %s, [%s]", value, target)
				default:
					return errors.Errorf("we don't know how to store a value of type %q yet", inst.Type)
				}
			case *ir.PointerType:
				c.emit("str %s, [%s]", value, target)
			default:
				return errors.Errorf("invalid target type for store instruction: %T", ty)
			}
			c.registerAllocator.releaseIfTempAllocation(target)
			c.registerAllocator.releaseIfTempAllocation(value)
		case *ir.Call:
			savedCallerRegisters := c.registerAllocator.saveCallerSavedRegisters(c)
			for i, arg := range inst.Args {
				argReg := c.mustLookupRegisterAllocation(arg)
				c.registerAllocator.move(callArgsRegisters[i], argReg, c)
			}
			switch callee := inst.Callee.(type) {
			case ir.Register:
				reg := c.mustLookupRegisterAllocation(callee)
				c.registerAllocator.ensureInRegister(reg, c)
				c.emit("blr %s", reg)
				c.registerAllocator.releaseIfTempAllocation(reg)
			case ir.DefinedFunction:
				c.emit("bl %s", c.funcName(callee.Id))
			default:
				panic(fmt.Sprintf("unknown callee type: %T", callee))
			}
			c.registerAllocator.restoreCallerSavedRegisters(savedCallerRegisters, c)
			if _, ok := inst.FunctionType.Result.(ir.NoneType); !ok {
				allocation := c.registerAllocator.saveCallResultRegister(inst.Register(), c)
				c.values[inst.Register().Id] = allocation
			}
		case *ir.KeepAlive:
		default:
			return errors.Errorf("unknown instruction: %T", inst)
		}
		for _, reg := range c.function.RegisterExpirations.Expired(c.block, i) {
			c.registerAllocator.forget(reg, c)
		}
	}
	switch terminator := block.Terminator.(type) {
	case *ir.Jump:
		c.emit("b %s", c.blockLabel(terminator.Target))
	case *ir.CondBranch:
		condRegister := c.mustLookupRegisterAllocation(terminator.Condition)
		c.registerAllocator.ensureInRegister(condRegister, c)
		c.emit("cbnz %s, %s", condRegister, c.blockLabel(terminator.TrueBlock))
		c.emit("b %s", c.blockLabel(terminator.FalseBlock))
		c.registerAllocator.releaseIfTempAllocation(condRegister)
	case *ir.Return:
		if terminator.Value == ir.NoneRegister {
			c.emit("mov x0, xzr")
		} else {
			value := c.mustLookupRegisterAllocation(terminator.Value)
			c.registerAllocator.move(x0, value, c)
		}
		c.emit("b %s_ret", c.funcName(c.function.Id))
	default:
		return errors.Errorf("unknown terminator: %T", terminator)
	}
	c.decIndent()
	return nil
}

func generateFunction(function *ir.FunctionDefinition, module *ir.Module, isMain bool) (*ASMText, error) {
	stackAllocator := &stackAllocator{size: 16}
	registerAllocator := newRegisterAllocator(
		slices.Concat(callerSavedRegisters, calleeSavedRegisters),
		function.RegisterConstraints,
	)
	values := make(map[ir.RegisterId]*registerAllocation)
	blockCodes := []*blockCode{}
	// Generate the function body code.
	if err := ir.WalkBlock(function.Entry, func(block *ir.Block) error {
		code := &blockCode{
			block:             block,
			function:          function,
			values:            values,
			stackAllocator:    stackAllocator,
			registerAllocator: registerAllocator,
			dataLayout:        module.DataLayout,
			typeInfo:          module.TypeInfo,
		}
		if block == function.Entry {
			code.incIndent()
			// todo: Be smarter and don't just blindly copy parameters to new registers.
			for i, param := range function.Type.Params {
				allocation := registerAllocator.allocate(param.Register, code)
				code.emit("mov %s, %s", allocation, callArgsRegisters[i])
				values[param.Register.Id] = allocation
			}
			code.decIndent()
		}
		if err := code.generateBlock(block); err != nil {
			return err
		}
		blockCodes = append(blockCodes, code)
		return nil
	}); err != nil {
		return nil, err
	}
	c := &ASMText{}
	if isMain {
		c.emit("_main:")
	} else {
		c.emit("%s:", funcName(function.Id, module.TypeInfo))
	}
	c.incIndent()
	// Save used callee saved registers.
	usedCalleeSaved := registerAllocator.usedCalleeSavedRegisters()
	calleeSavedCode := &ASMText{}
	calleeSavedCode.incIndent()
	usedCalleeSavedStackOffset := stackAllocator.size
	for _, reg := range usedCalleeSaved {
		stackOffset := stackAllocator.allocate(16)
		calleeSavedCode.emit("str %s, [sp, #%d]", reg, stackOffset)
	}
	// Setup stack and frame pointer.
	if stackAllocator.size <= 504 {
		// We can use the shorthand notation.
		c.emit("stp fp, lr, [sp, #-%d]!", stackAllocator.alignedSize())
		c.emit("mov fp, sp")
	} else {
		// We have to update the sp offset explicitly.
		c.emit("sub sp, sp, #%d", stackAllocator.alignedSize())
		c.emit("stp fp, lr, [sp, #0]")
		c.emit("mov fp, sp")
	}
	c.lines = append(c.lines, calleeSavedCode.lines...)
	// Insert block code.
	for _, block := range blockCodes {
		c.lines = append(c.lines, block.lines...)
	}
	c.decIndent()
	// Return point:
	c.emit("%s_ret:", funcName(function.Id, module.TypeInfo))
	c.incIndent()
	// Restore callee saved registers.
	for _, reg := range usedCalleeSaved {
		c.emit("ldr %s, [sp, #%d]", reg, usedCalleeSavedStackOffset)
		usedCalleeSavedStackOffset = usedCalleeSavedStackOffset + 16
	}
	// Restore stack and frame pointer.
	if stackAllocator.alignedSize() <= 504 {
		// We can use the shorthand notation.
		c.emit("ldp fp, lr, [sp], #%d", stackAllocator.alignedSize())
	} else {
		// We have to update the sp offset explicitly.
		c.emit("ldp fp, lr, [sp]")
		c.emit("add sp, sp, #%d", stackAllocator.alignedSize())
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
.print:
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    ldr x1, [x0, 8]
    ldr x2, [x0]
    mov x0, 1
    bl _write
    ldp fp, lr, [sp], #16
    mov x0, xzr
    ret`)
}

func defineBuiltInPrintCharFunction(asm *ASMText) {
	asm.emit(
		`
.print_char:
    stp fp, lr, [sp, #-32]!
    mov fp, sp
    str x0, [sp]
    adrp x0, _print_char_format@PAGE
    add x0, x0, _print_char_format@PAGEOFF+0
    bl _printf
    mov x0, 0
    bl _fflush
    ldp fp, lr, [sp], #32
    mov x0, xzr
    ret`)
}

func defineBuiltInPrintIntFunction(asm *ASMText) {
	asm.emit(
		`
.print_int:
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
    ret`)
}

func defineBuiltInPrintUIntFunction(asm *ASMText) {
	asm.emit(
		`
.print_uint:
    stp fp, lr, [sp, #-32]!
    mov fp, sp
    str x0, [sp]
    adrp x0, _print_uint_format@PAGE
    add x0, x0, _print_uint_format@PAGEOFF+0
    bl _printf
    mov x0, 0
    bl _fflush
    ldp fp, lr, [sp], #32
    mov x0, xzr
    ret`)
}

func defineBuiltInPrintBoolFunction(asm *ASMText) {
	asm.emit(
		`
.print_bool:
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    cmp x0, #0
    bne .print_bool_true
    adrp x0, _print_bool_false@PAGE
    add x0, x0, _print_bool_false@PAGEOFF+0
    b .print_bool_end
.print_bool_true:
    adrp x0, _print_bool_true@PAGE
    add x0, x0, _print_bool_true@PAGEOFF+0
.print_bool_end:
    bl _printf
    mov x0, 0
    bl _fflush
    ldp fp, lr, [sp], #16
    mov x0, xzr
    ret`)
}

func defineBuiltInInternalMalloc(asm *ASMText) {
	asm.emit(
		`
.internal_malloc:
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    bl _malloc
    cmp x0, #0
    bgt .internal_malloc_success      
    adrp x0, _internal_malloc_failed@PAGE
    add x0, x0, _internal_malloc_failed@PAGEOFF
    bl _puts
    mov x0, #1
    bl _exit
.internal_malloc_success:
    ldp fp, lr, [sp], #16
    ret`)
}

func defineBuiltInInternalFree(asm *ASMText) {
	asm.emit(
		`
.internal_free:
    stp fp, lr, [sp, #-16]!
    mov fp, sp
    bl _free
    ldp fp, lr, [sp], #16
    ret`)
}

func defineBuiltInInternalExit(asm *ASMText) {
	asm.emit(
		`
.internal_exit:
    bl _exit
    `)
}

func definePanicDivideByZero(asm *ASMText, typeInfo *typed.TypeInfo) {
	panicFuncName := funcName(typeInfo.Panic.Id(), typeInfo)
	asm.emit(
		`
.internal_panic_divide_by_zero:
    adrp x0, _internal_divide_by_zero@PAGE
    add x0, x0, _internal_divide_by_zero@PAGEOFF
    bl %s
    `, panicFuncName)
}

func GenerateDarwinArm64ASM(irModule *ir.Module) (*ASMText, error) {
	if irModule.Main == nil {
		panic("no main function found")
	}
	asm := &ASMText{}
	asm.emit(".global _main")
	asm.emit(".text")
	defineBuiltInInternalMalloc(asm)
	defineBuiltInInternalFree(asm)
	defineBuiltInInternalExit(asm)
	defineBuiltInPrintFunction(asm)
	defineBuiltInPrintCharFunction(asm)
	defineBuiltInPrintIntFunction(asm)
	defineBuiltInPrintUIntFunction(asm)
	defineBuiltInPrintBoolFunction(asm)
	definePanicDivideByZero(asm, irModule.TypeInfo)
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
	constants := append(irModule.Constants, &ir.StrConst{Id: "_internal_divide_by_zero", Value: "divide by zero"})
	for _, constant := range constants {
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
	asm.emit("_print_uint_format:")
	asm.incIndent().emit(".asciz \"%%llu\"").decIndent()
	asm.emit(".align 3")
	asm.emit("_print_char_format:")
	asm.incIndent().emit(".asciz \"%%c\"").decIndent()
	asm.emit(".align 3")
	asm.emit("_print_bool_true:")
	asm.incIndent().emit(".asciz \"true\"").decIndent()
	asm.emit(".align 3")
	asm.emit("_print_bool_false:")
	asm.incIndent().emit(".asciz \"false\"").decIndent()
	asm.emit(".align 3")
	asm.emit("_internal_malloc_failed:")
	asm.incIndent().emit(".asciz \"out of memory\"").decIndent()
	return asm, nil
}
