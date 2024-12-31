package ir

import (
	"fmt"
	"slices"
	"strings"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/base"
	"github.com/flunderpero/klar/bootstrap/lower"
	"github.com/flunderpero/klar/bootstrap/typed"
	"github.com/pkg/errors"
)

type Type interface {
	String() string
}

type IntType string

const (
	Int1Type   IntType = "i1"
	Int8Type   IntType = "i8"
	Int16Type  IntType = "i16"
	Int32Type  IntType = "i32"
	Int64Type  IntType = "i64"
	UInt8Type  IntType = "u8"
	UInt16Type IntType = "u16"
	UInt32Type IntType = "u32"
	UInt64Type IntType = "u64"
)

func (t IntType) String() string {
	return string(t)
}

func (t IntType) IsSigned() bool {
	switch t {
	case Int1Type, Int8Type, Int16Type, Int32Type, Int64Type:
		return true
	}
	return false
}

type NoneType struct{}

var noneType = NoneType{}

func (t NoneType) String() string {
	return "none"
}

type PointerType struct {
	ElementType Type
}

func (t PointerType) String() string {
	return fmt.Sprintf("%s*", t.ElementType)
}

func isValueType(ty Type) bool {
	switch ty.(type) {
	case IntType, *PointerType:
		return true
	}
	return false
}

type StructType struct {
	Fields []Type
}

func (t StructType) String() string {
	fields := ""
	for _, field := range t.Fields {
		if len(fields) > 0 {
			fields += ", "
		}
		fields += field.String()
	}
	return fmt.Sprintf("{ %s }", fields)
}

type FunctionParam struct {
	Type     Type
	Register Register
}

func (fa FunctionParam) String() string {
	return fmt.Sprintf("%s %s", fa.Type, fa.Register)
}

type FunctionType struct {
	Params []FunctionParam
	Result Type
}

func (t FunctionType) String() string {
	params := ""
	for i, param := range t.Params {
		if i > 0 {
			params += ", "
		}
		params += param.String()
	}
	return fmt.Sprintf("%s (%s)", t.Result, params)
}

var StrType = &StructType{Fields: []Type{Int64Type, &PointerType{Int8Type}}}

type DataLayout interface {
	SizeOf(ty Type) int
	Alignment(ty Type) int
	FieldOffset(ty *StructType, index int) int
}

type BlockId int

func (b BlockId) String() string {
	return fmt.Sprintf("block_%d", b)
}

type Block struct {
	Id           BlockId
	Instructions []Instruction
	Terminator   Terminator
	Predecessors []*Block
	Result       Register
}

func (ir *Block) append(instruction Instruction) {
	ir.Instructions = append(ir.Instructions, instruction)
}

func (ir Block) String() string {
	s := ""
	for _, inst := range ir.Instructions {
		s += fmt.Sprintf("\n    %s", inst)
	}
	s += fmt.Sprintf("\n    %s", ir.Terminator)
	return fmt.Sprintf("%s:%s", ir.Id, s)
}

type Terminator interface {
	String() string
	Targets() []*Block
	Registers() []Register
}

type Jump struct {
	Target *Block
}

func (ir Jump) String() string {
	return fmt.Sprintf("jmp %s", ir.Target.Id)
}

func (ir *Jump) Targets() []*Block {
	return []*Block{ir.Target}
}

func (ir Jump) Registers() []Register {
	return []Register{}
}

type CondBranch struct {
	Condition  Register
	TrueBlock  *Block
	FalseBlock *Block
}

func (ir CondBranch) String() string {
	return fmt.Sprintf("br i1 %s, %s, %s", ir.Condition, ir.TrueBlock.Id, ir.FalseBlock.Id)
}

func (ir *CondBranch) Targets() []*Block {
	return []*Block{ir.TrueBlock, ir.FalseBlock}
}

func (ir CondBranch) Registers() []Register {
	return []Register{ir.Condition}
}

type Return struct{}

func (ir Return) String() string {
	return "ret"
}

func (ir *Return) Targets() []*Block {
	return []*Block{}
}

func (ir Return) Registers() []Register {
	return []Register{}
}

type FunctionDefinition struct {
	Id                  typed.TypeId
	Type                FunctionType
	TypeInfo            *lower.SpecializedTypeInfo
	Entry               *Block
	RegisterConstraints *RegisterConstraints
	RegisterExpirations *RegisterExpirations
}

func (t FunctionDefinition) String() string {
	return fmt.Sprintf("%s = %s", t.Id, t.Type)
}

type Module struct {
	DataLayout    DataLayout
	Functions     []*FunctionDefinition
	Constants     []*StrConst
	DeclaredTypes *DeclaredTypes
	Main          *FunctionDefinition
	TypeInfo      *typed.TypeInfo
}

func (m Module) String() string {
	sb := strings.Builder{}
	indent := 0
	writeln := func(parts ...any) {
		indentStr := base.IndentString("", indent)
		s := indentStr
		for _, part := range parts {
			switch part := part.(type) {
			case fmt.Stringer:
				s += part.String()
			case string:
				s += part
			}
		}
		sb.WriteString(strings.ReplaceAll(s, "\n", "\n"+indentStr))
		sb.WriteByte('\n')
	}
	for _, constant := range m.Constants {
		writeln(constant.DeclareString())
	}
	if len(m.Constants) > 0 {
		writeln()
	}
	for i, function := range m.Functions {
		if i > 0 {
			writeln()
		}
		name := m.TypeInfo.MustLookupSymbol(function.Id).FQN()
		writeln(fmt.Sprintf("declare %s %s\n{", name, function.Type))
		indent += 1
		err := WalkBlock(function.Entry, func(block *Block) error {
			writeln(block)
			return nil
		})
		if err != nil {
			panic(fmt.Sprintf("Failed to print the IR: %+v\n", err))
		}
		if !function.RegisterConstraints.IsEmpty() {
			writeln(function.RegisterConstraints)
		}
		indent -= 1
		writeln("}")
	}
	return strings.TrimRight(sb.String(), "\n")
}

type RegisterId int

type Register struct {
	Id   RegisterId
	Type Type
}

func (r Register) getPointerSourceMarker() {}

func (r Register) calleeMarker() {}

func (r Register) String() string {
	if r.Id == 0 {
		return "none"
	}
	return fmt.Sprintf("%%%d", r.Id)
}

func newRegister(id int, ty Type) Register {
	return Register{Id: RegisterId(id), Type: ty}
}

var NoneRegister = Register{Id: 0, Type: noneType}

type Instruction interface {
	String() string
	Register() Register
	ParamRegisters() []Register
}

type KeepAlive struct {
	params []Register
}

func (i KeepAlive) String() string {
	params := ""
	for _, param := range i.params {
		if len(params) > 0 {
			params += ", "
		}
		params += param.String()
	}
	return fmt.Sprintf("keepalive %s", params)
}

func (i *KeepAlive) Register() Register {
	return NoneRegister
}

func (i KeepAlive) ParamRegisters() []Register {
	return i.params
}

type StrConst struct {
	Id    string
	Value string
}

func (i StrConst) DeclareString() string {
	return fmt.Sprintf("declare const %s = @str %q", i.Id, i.Value)
}

func (i StrConst) String() string {
	return i.Id
}

func (i StrConst) getPointerSourceMarker() {}

type IntConst struct {
	register Register
	Value    int64
	Type     IntType
}

func (i IntConst) String() string {
	return fmt.Sprintf("%s = %s %d", i.register, i.Type, i.Value)
}

func (i *IntConst) Register() Register {
	return i.register
}

func (i IntConst) ParamRegisters() []Register {
	return []Register{}
}

type UIntConst struct {
	register Register
	Value    uint64
	Type     IntType
}

func (i UIntConst) String() string {
	return fmt.Sprintf("%s = %s %d", i.register, i.Type, i.Value)
}

func (i *UIntConst) Register() Register {
	return i.register
}

func (i UIntConst) ParamRegisters() []Register {
	return []Register{}
}

type BoolConst struct {
	register Register
	Value    int
}

func (i BoolConst) String() string {
	return fmt.Sprintf("%s = i1 %d", i.register, i.Value)
}

func (i *BoolConst) Register() Register {
	return i.register
}

func (i BoolConst) ParamRegisters() []Register {
	return []Register{}
}

type GetPointerSource interface {
	getPointerSourceMarker()
}

type GetPointer struct {
	register   Register
	Source     GetPointerSource
	SourceType Type
	FieldIndex int
}

func (i GetPointer) String() string {
	return fmt.Sprintf("%s = getptr %s %s, %d", i.register, i.SourceType, i.Source, i.FieldIndex)
}

func (i *GetPointer) Register() Register {
	return i.register
}

func (i *GetPointer) ParamRegisters() []Register {
	if reg, ok := i.Source.(Register); ok {
		return []Register{reg}
	}
	return []Register{}
}

type Load struct {
	register   Register
	Source     Register
	TargetType Type
}

func (i *Load) Register() Register {
	return i.register
}

func (i *Load) ParamRegisters() []Register {
	return []Register{i.Source}
}

func (i Load) String() string {
	return fmt.Sprintf("%s = load %s %s", i.register, i.TargetType, i.Source)
}

type Store struct {
	Target Register
	Value  Register
	Type   Type
}

func (s *Store) Register() Register {
	return NoneRegister
}

func (s *Store) ParamRegisters() []Register {
	return []Register{s.Target, s.Value}
}

func (s Store) String() string {
	return fmt.Sprintf("store %s %s, %s", s.Type, s.Value, s.Target)
}

type BinaryInst interface {
	Instruction
	binaryInstructionMarker()
	Lhs() Register
	Rhs() Register
}

type binaryInst struct {
	register Register
	lhs      Register
	rhs      Register
}

func (i *binaryInst) binaryInstructionMarker() {}

func (i *binaryInst) Lhs() Register {
	return i.lhs
}

func (i *binaryInst) Rhs() Register {
	return i.rhs
}

func (i *binaryInst) Register() Register {
	return i.register
}

func (i *binaryInst) ParamRegisters() []Register {
	return []Register{i.lhs, i.rhs}
}

func (i *binaryInst) String() string {
	panic("not implemented")
}

type SignedIntAddWithOverflow struct {
	binaryInst
	Type IntType
}

func (i SignedIntAddWithOverflow) String() string {
	return fmt.Sprintf("%s = iaddo %s %s, %s", i.register, i.Type, i.lhs, i.rhs)
}

type IntMultiplicationWithOverflow struct {
	binaryInst
	Type IntType
}

func (i IntMultiplicationWithOverflow) String() string {
	return fmt.Sprintf("%s = imulo %s %s, %s", i.register, i.Type, i.lhs, i.rhs)
}

type SignedIntDivision struct {
	binaryInst
	Type IntType
}

func (i SignedIntDivision) String() string {
	return fmt.Sprintf("%s = sidiv %s %s, %s", i.register, i.Type, i.lhs, i.rhs)
}

type UnsignedIntDivision struct {
	binaryInst
	Type IntType
}

func (i UnsignedIntDivision) String() string {
	return fmt.Sprintf("%s = idiv %s %s, %s", i.register, i.Type, i.lhs, i.rhs)
}

type SignedIntModulo struct {
	binaryInst
	Type IntType
}

func (i SignedIntModulo) String() string {
	return fmt.Sprintf("%s = simod %s %s, %s", i.register, i.Type, i.lhs, i.rhs)
}

type UnsignedIntModulo struct {
	binaryInst
	Type IntType
}

func (i UnsignedIntModulo) String() string {
	return fmt.Sprintf("%s = imod %s %s, %s", i.register, i.Type, i.lhs, i.rhs)
}

type UnsignedIntAddWithOverflow struct {
	binaryInst
	Type IntType
}

func (i UnsignedIntAddWithOverflow) String() string {
	return fmt.Sprintf("%s = addo %s %s, %s", i.register, i.Type, i.lhs, i.rhs)
}

type IntCompOp string

const (
	IntCompOpEQ  IntCompOp = "eq"
	IntCompOpNEQ IntCompOp = "ne"
	IntCompOpLT  IntCompOp = "lt"
	IntCompOpLTE IntCompOp = "le"
	IntCompOpGT  IntCompOp = "gt"
	IntCompOpGTE IntCompOp = "ge"
)

type IntCompare struct {
	binaryInst
	Op   IntCompOp
	Type IntType
}

func (i IntCompare) String() string {
	return fmt.Sprintf("%s = icmp %s %s %s, %s", i.register, i.Op, i.Type, i.lhs, i.rhs)
}

type BinaryLogicOp string

const (
	BinaryLogicOpAnd BinaryLogicOp = "and"
	BinaryLogicOpOr  BinaryLogicOp = "or"
)

type BinaryLogic struct {
	binaryInst
	Op BinaryLogicOp
}

func (self BinaryLogic) String() string {
	return fmt.Sprintf("%s = %s i1 %s, %s", self.register, self.Op, self.lhs, self.rhs)
}

type UnaryLogicOp string

const (
	UnaryLogicOpNot UnaryLogicOp = "not"
)

type UnaryLogic struct {
	register Register
	Value    Register
	Op       UnaryLogicOp
}

func (self UnaryLogic) Register() Register {
	return self.register
}

func (self UnaryLogic) ParamRegisters() []Register {
	return []Register{self.Value}
}

func (self UnaryLogic) String() string {
	return fmt.Sprintf("%s = %s i1 %s", self.register, self.Op, self.Value)
}

type Callee interface {
	calleeMarker()
}

type DefinedFunction struct {
	Id  typed.TypeId
	FQN string
}

func (self DefinedFunction) String() string {
	return self.FQN
}

func (self DefinedFunction) calleeMarker() {}

func (self DefinedFunction) getPointerSourceMarker() {}

type Call struct {
	register     Register
	Callee       Callee
	FunctionType *FunctionType
	Args         []Register
	IsIndirect   bool
}

func (inst Call) String() string {
	args := ""
	for i, reg := range inst.Args {
		arg := inst.FunctionType.Params[i]
		if len(args) > 0 {
			args += ", "
		}
		args += fmt.Sprintf("%s %s", arg.Type, reg)
	}
	assign := ""
	if inst.register.Type != noneType {
		assign = fmt.Sprintf("%s = ", inst.register)
	}
	prefix := ""
	if inst.IsIndirect {
		prefix = "i"
	}
	return fmt.Sprintf("%s%scall %s %s(%s)", assign, prefix, inst.FunctionType.Result, inst.Callee, args)
}

func (inst Call) Register() Register {
	return inst.register
}

func (inst Call) ParamRegisters() []Register {
	if reg, ok := inst.Callee.(Register); ok {
		return append([]Register{reg}, inst.Args...)
	}
	return inst.Args
}

/*
RegisterConstraints make sure that mutating values bound to symbols
in different branches generates correct code.

Consider this simple if/else and its corresponding IR:

mut a = 1

	if true {
	    a = 2
	} else {
	    a = 3
	}

_main:

	%1 = i64 1          -- mut a = 1
	%2 = i1 1           -- `true`
	%3 = br i1 %2, true_block, else_block

true_block:

	%4 = i64 2          -- a = 2
	jmp merge_block

false_block:

	%5 = i64 3          -- a = 3
	jmp merge_block

merge_block:

	!!! Here, `a` may be in %4 or %5 depending on which branch was taken.

Other compiler frameworks like LLVM insert a so-called phi node to
communicate that we have to look for the value depending where we came
from:

merge_block:

	%6 = phi %4 true_block, %5 else_block

This basically says: If you arrived here from the `true_block` then
%6 will have the value of %4, but %5 if we came from the `else_block`.

We are taking a slightly different approach (with the same result).
During IR generation we add a register constraint telling the code
generation to make sure to put %1, %4, and %5 into the same hardware
register or stack location.
*/
type RegisterConstraints struct {
	constraints []*[]Register
}

func (r RegisterConstraints) IsEmpty() bool {
	return len(r.constraints) == 0
}

func (r *RegisterConstraints) Lookup(reg Register) (*[]Register, bool) {
	for _, constraint := range r.constraints {
		for _, c := range *constraint {
			if c == reg {
				return constraint, true
			}
		}
	}
	return nil, false
}

func (r RegisterConstraints) String() string {
	if len(r.constraints) == 0 {
		return ""
	}
	s := "@constraint "
	for i, c := range r.constraints {
		if i > 0 {
			s += "\n@constraint "
		}
		for i, reg := range *c {
			if i > 0 {
				s += ", "
			}
			s += reg.String()
		}
	}
	return s
}

func (r *RegisterConstraints) add(reg1 Register, reg2 Register) {
	c, found := r.Lookup(reg1)
	if found {
		if !slices.Contains(*c, reg2) {
			*c = append(*c, reg2)
		}
		return
	}
	c, found = r.Lookup(reg2)
	if found {
		if !slices.Contains(*c, reg1) {
			*c = append(*c, reg1)
		}
		return
	}
	r.constraints = append(r.constraints, &[]Register{reg1, reg2})
}

type symbolTable struct {
	symbols map[ast.Ident]Register
	parent  *symbolTable
}

func (s *symbolTable) mustLookup(name ast.Ident) Register {
	if reg, found := s.lookup(name); found {
		return reg
	}
	panic(fmt.Sprintf("undeclared symbol: %s", name))
}

func (s *symbolTable) lookup(name ast.Ident) (Register, bool) {
	table := s
	for table != nil {
		if reg, found := table.symbols[name]; found {
			return reg, true
		}
		table = table.parent
	}
	return Register{}, false
}

func (s *symbolTable) declare(name ast.Ident, reg Register) {
	s.symbols[name] = reg
}

func (s *symbolTable) assign(name ast.Ident, reg Register) {
	table := s
	for table != nil {
		if _, found := table.symbols[name]; found {
			table.symbols[name] = reg
			return
		}
		table = table.parent
	}
	panic(fmt.Sprintf("undeclared symbol: %s", name))
}

func (s *symbolTable) copy() map[ast.Ident]Register {
	table := s
	res := make(map[ast.Ident]Register)
	for table != nil {
		for k, v := range table.symbols {
			if _, found := res[k]; !found {
				res[k] = v
			}
		}
		table = table.parent
	}
	return res
}

type loopScope struct {
	loopBlock     *Block
	exitBlock     *Block
	registerMark  int
	usedRegisters map[RegisterId]Register
}

type generator struct {
	ast.DefaultVisitor
	currentBlock        *Block
	typeInfo            *lower.SpecializedTypeInfo
	registerByNodeId    map[ast.NodeId]Register
	symbolTable         *symbolTable
	globalConstants     *[]*StrConst
	registerIndex       int
	blockIndex          int
	declaredTypes       *DeclaredTypes
	registerConstraints RegisterConstraints
	loopScopes          []loopScope
	definedFunctions    map[typed.TypeId]DefinedFunction
	dataLayout          DataLayout
}

func (g *generator) enterScope() {
	g.symbolTable = &symbolTable{symbols: make(map[ast.Ident]Register), parent: g.symbolTable}
}

func (g *generator) exitScope() {
	g.symbolTable = g.symbolTable.parent
}

func (g *generator) enterLoop(loopBlock *Block, exitBlock *Block) *loopScope {
	scope := loopScope{
		loopBlock: loopBlock, exitBlock: exitBlock, registerMark: g.registerIndex, usedRegisters: make(map[RegisterId]Register)}
	g.loopScopes = append(g.loopScopes, scope)
	return &scope
}

func (g *generator) loopScope() loopScope {
	return g.loopScopes[len(g.loopScopes)-1]
}

func (g *generator) exitLoop() {
	g.loopScopes = g.loopScopes[:len(g.loopScopes)-1]
}

func (g *generator) nextRegister(ty Type) Register {
	g.registerIndex++
	return newRegister(g.registerIndex, ty)
}

func (g *generator) append(instruction Instruction, node ast.Node) {
	for _, reg := range instruction.ParamRegisters() {
		for _, loopScope := range g.loopScopes {
			if reg.Id <= RegisterId(loopScope.registerMark) {
				loopScope.usedRegisters[reg.Id] = reg
			}
		}
	}
	g.currentBlock.append(instruction)
	if node != nil {
		g.registerByNodeId[node.Id()] = instruction.Register()
	}
}

func (g *generator) lookupRegisterByNode(node ast.Node) Register {
	reg, ok := g.registerByNodeId[node.Id()]
	if !ok {
		panic(fmt.Sprintf("No register found for node %s", node))
	}
	return reg
}

func (g *generator) newBlock(predecessors ...*Block) *Block {
	g.blockIndex += 1
	block := &Block{Id: BlockId(g.blockIndex)}
	block.Predecessors = append(block.Predecessors, predecessors...)
	return block
}

func (g *generator) updateRegisterConstraints(symbolTableBefore map[ast.Ident]Register) []Register {
	result := []Register{}
	for symbol, regBefore := range symbolTableBefore {
		regNow := g.symbolTable.mustLookup(symbol)
		if regNow.Id != regBefore.Id {
			g.registerConstraints.add(regNow, regBefore)
			result = append(result, regNow)
		}
	}
	return result
}

func (g *generator) isDefinedFunction(typedTy typed.Type) (DefinedFunction, bool) {
	res, ok := g.definedFunctions[typedTy.Id()]
	if !ok {
		funcType, ok := typedTy.(*typed.FunctionType)
		if ok {
			if base, hasBase := funcType.GenericBase(); hasBase {
				return g.isDefinedFunction(base)
			}
		}
	}
	return res, ok
}

func (g *generator) VisitStringLiteralExpression(expr *ast.StringLiteralExpression) error {
	id := fmt.Sprintf("str%d", len(*g.globalConstants))
	strConst := &StrConst{Id: id, Value: expr.Value}
	*g.globalConstants = append(*g.globalConstants, strConst)
	g.append(&GetPointer{
		register:   g.nextRegister(PointerType{StrType}),
		Source:     strConst,
		SourceType: StrType,
		FieldIndex: 0,
	}, expr)
	return nil
}

func (g *generator) VisitIntLiteralExpression(expr *ast.IntLiteralExpression) error {
	if expr.IsUInt64 {
		g.append(&UIntConst{
			register: g.nextRegister(UInt64Type),
			Value:    expr.UInt64,
			Type:     g.lookupType(expr).(IntType),
		}, expr)
		return nil
	}
	g.append(&IntConst{
		register: g.nextRegister(Int64Type),
		Value:    expr.Int64,
		Type:     g.lookupType(expr).(IntType),
	}, expr)
	return nil
}

func (g *generator) VisitCharLiteralExpression(expr *ast.CharLiteralExpression) error {
	g.append(&UIntConst{
		register: g.nextRegister(UInt32Type),
		Value:    uint64(expr.Value),
		Type:     UInt32Type,
	}, expr)
	return nil
}

func (g *generator) VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) error {
	value := 0
	if expr.Value {
		value = 1
	}
	g.append(&BoolConst{
		register: g.nextRegister(Int1Type),
		Value:    value,
	}, expr)
	return nil
}

func (g *generator) VisitIdentExpression(expr *ast.IdentExpression) error {
	if reg, found := g.symbolTable.lookup(expr.Ident); found {
		g.registerByNodeId[expr.Id()] = reg
		return nil
	}
	typedTy := g.typeInfo.MustLookup(expr)
	sourceType := g.declaredTypes.MustLookup(typedTy)
	var source GetPointerSource
	if _, ok := typedTy.(*typed.NoneType); ok {
		g.registerByNodeId[expr.Id()] = NoneRegister
		return nil
	} else if definedFunc, ok := g.isDefinedFunction(typedTy); ok {
		source = definedFunc
	} else {
		source = g.symbolTable.mustLookup(expr.Ident)
	}
	reg := g.nextRegister(&PointerType{ElementType: sourceType})
	g.append(&GetPointer{
		register:   reg,
		Source:     source,
		SourceType: sourceType,
		FieldIndex: 0,
	}, expr)
	g.registerByNodeId[expr.Id()] = reg
	return nil
}

func (g *generator) VisitCallExpression(expr *ast.CallExpression, w ast.Walker) error {
	calleeType := g.typeInfo.MustLookup(expr.Callee)
	switch calleeType := calleeType.(type) {
	case *typed.StructType:
		for _, arg := range expr.Args {
			if err := g.VisitNode(arg.Value, w); err != nil {
				return err
			}
		}
		structType := g.lookupType(expr).(*StructType)
		sizeReg := g.nextRegister(Int64Type)
		mallocReg := g.nextRegister(Int64Type)
		mallocFuncType := g.declaredTypes.MustLookup(typed.BuiltInInternalMallocFunction).(*FunctionType)
		mallocFuncDef := g.definedFunctions[typed.BuiltInInternalMallocFunction.Id()]
		g.append(&IntConst{
			register: sizeReg,
			Value:    int64(g.dataLayout.SizeOf(structType)),
			Type:     Int64Type,
		}, nil)
		g.append(&Call{
			register:     mallocReg,
			Callee:       mallocFuncDef,
			FunctionType: mallocFuncType,
			Args:         []Register{sizeReg},
		}, expr)
		for i, callArg := range expr.Args {
			fieldType := structType.Fields[i]
			switch fieldType.(type) {
			case IntType, *PointerType:
			default:
				return errors.Errorf("only IntType and PointerType can be stored in struct fields, got %q", fieldType)
			}
			fieldValueReg := g.lookupRegisterByNode(callArg.Value)
			fieldPtrReg := g.nextRegister(PointerType{fieldType})
			g.append(&GetPointer{
				register:   fieldPtrReg,
				Source:     mallocReg,
				FieldIndex: i,
				SourceType: structType,
			}, nil)
			g.append(&Store{
				Target: fieldPtrReg,
				Value:  fieldValueReg,
				Type:   fieldType,
			}, nil)
		}
		g.registerByNodeId[expr.Id()] = mallocReg
	case *typed.FunctionType:
		genericBase, hasGenericBase := calleeType.GenericBase()
		if hasGenericBase && genericBase.Id() == typed.BuiltInSizeOfFunction.Id() {
			// Special handling for `sizeof`.
			typeArg := calleeType.TypeArgs()[0]
			ty := g.declaredTypes.MustLookup(typeArg)
			var size int
			switch ty.(type) {
			case *FunctionType, *StructType:
				size = 8 // These are always pointer types.
			default:
				size = g.dataLayout.SizeOf(ty)
			}
			reg := g.nextRegister(Int64Type)
			g.append(&IntConst{register: reg, Value: int64(size), Type: Int64Type}, expr)
			return nil
		}
		var callee Callee
		if definedFunc, ok := g.isDefinedFunction(calleeType); ok {
			// We need to walk the arguments ourselves since we are not using the default walker.
			for _, arg := range expr.Args {
				if err := g.VisitNode(arg.Value, w); err != nil {
					return err
				}
			}
			callee = definedFunc
		} else {
			if err := w.WalkCallExpression(expr); err != nil {
				return err
			}
			callee = g.lookupRegisterByNode(expr.Callee)
		}
		args := []Register{}
		for _, arg := range expr.Args {
			args = append(args, g.lookupRegisterByNode(arg.Value))
		}
		if hasGenericBase && genericBase.Id() == typed.BuiltInInternalWritePtrFunction.Id() {
			// Special handling for `internal_write_ptr` which becomes just a `Store` instruction.
			valueType := g.declaredTypes.MustLookup(calleeType.TypeArgs()[0])
			if !isValueType(valueType) {
				valueType = &PointerType{ElementType: valueType}
			}
			g.append(&Store{
				Target: args[0],
				Value:  args[1],
				Type:   valueType,
			}, expr)
			return nil
		}
		if hasGenericBase && genericBase.Id() == typed.BuiltInInternalReadPtrFunction.Id() {
			// Special handling for `internal_read_ptr` which becomes just a `Load` instruction.
			targetType := g.declaredTypes.MustLookup(calleeType.TypeArgs()[0])
			if !isValueType(targetType) {
				targetType = &PointerType{ElementType: targetType}
			}
			g.append(&Load{
				register:   g.nextRegister(targetType),
				Source:     args[0],
				TargetType: targetType,
			}, expr)
			return nil
		}
		ty := g.typeInfo.MustLookup(expr.Callee)
		funcType := g.declaredTypes.MustLookup(ty).(*FunctionType)
		var reg Register = NoneRegister
		if funcType.Result != noneType {
			reg = g.nextRegister(funcType.Result)
		}
		g.append(&Call{
			register:     reg,
			Callee:       callee,
			FunctionType: funcType,
			Args:         args,
		}, expr)
	default:
		panic(fmt.Sprintf("unknown callee type: %T", calleeType))
	}
	return nil
}

func (g *generator) VisitBinaryExpression(expr *ast.BinaryExpression, w ast.Walker) error {
	if err := w.WalkBinaryExpression(expr); err != nil {
		return err
	}
	lhs := g.lookupRegisterByNode(expr.Lhs)
	rhs := g.lookupRegisterByNode(expr.Rhs)
	switch expr.Op {
	case ast.OpAdd:
		valueType := g.lookupType(expr).(IntType)
		if valueType.IsSigned() {
			g.append(
				&SignedIntAddWithOverflow{
					binaryInst: binaryInst{register: g.nextRegister(valueType), lhs: lhs, rhs: rhs}, Type: valueType}, expr)
		} else {
			g.append(
				&UnsignedIntAddWithOverflow{
					binaryInst: binaryInst{register: g.nextRegister(valueType), lhs: lhs, rhs: rhs}, Type: valueType}, expr)
		}
	case ast.OpMultiply:
		valueType := g.lookupType(expr).(IntType)
		g.append(
			&IntMultiplicationWithOverflow{
				binaryInst: binaryInst{register: g.nextRegister(valueType), lhs: lhs, rhs: rhs}, Type: valueType}, expr)
	case ast.OpDivide:
		valueType := g.lookupType(expr).(IntType)
		if valueType.IsSigned() {
			g.append(
				&SignedIntDivision{
					binaryInst: binaryInst{register: g.nextRegister(valueType), lhs: lhs, rhs: rhs}, Type: valueType}, expr)
		} else {
			g.append(
				&UnsignedIntDivision{
					binaryInst: binaryInst{register: g.nextRegister(valueType), lhs: lhs, rhs: rhs}, Type: valueType}, expr)
		}
	case ast.OpModulo:
		valueType := g.lookupType(expr).(IntType)
		if valueType.IsSigned() {
			g.append(
				&SignedIntModulo{
					binaryInst: binaryInst{register: g.nextRegister(valueType), lhs: lhs, rhs: rhs}, Type: valueType}, expr)
		} else {
			g.append(
				&UnsignedIntModulo{
					binaryInst: binaryInst{register: g.nextRegister(valueType), lhs: lhs, rhs: rhs}, Type: valueType}, expr)
		}
	case ast.OpEqual, ast.OpNotEqual, ast.OpLessThan, ast.OpLessThanOrEqual, ast.OpGreaterThan, ast.OpGreaterThanOrEqual:
		ty := g.lookupType(expr.Lhs).(IntType)
		ops := map[ast.BinaryOperator]IntCompOp{
			ast.OpEqual:              IntCompOpEQ,
			ast.OpNotEqual:           IntCompOpNEQ,
			ast.OpLessThan:           IntCompOpLT,
			ast.OpLessThanOrEqual:    IntCompOpLTE,
			ast.OpGreaterThan:        IntCompOpGT,
			ast.OpGreaterThanOrEqual: IntCompOpGTE,
		}
		op, ok := ops[expr.Op]
		if !ok {
			panic(fmt.Sprintf("unknown binary operator: %s", expr.Op))
		}
		g.append(&IntCompare{
			binaryInst: binaryInst{register: g.nextRegister(Int1Type), lhs: lhs, rhs: rhs}, Op: op, Type: ty}, expr)
	case ast.OpOr, ast.OpAnd:
		ty := g.lookupType(expr.Lhs).(IntType)
		if ty != Int1Type {
			return errors.Errorf("type of lhs is not a supported int type, but %s", ty)
		}
		var op BinaryLogicOp
		switch expr.Op {
		case ast.OpOr:
			op = BinaryLogicOpOr
		case ast.OpAnd:
			op = BinaryLogicOpAnd
		default:
			return errors.Errorf("unsupported binary operator: %s", expr.Op)
		}
		g.append(&BinaryLogic{
			binaryInst: binaryInst{register: g.nextRegister(Int1Type), lhs: lhs, rhs: rhs}, Op: op}, expr)
	default:
		return errors.Errorf("unsupported binary operator: %s", expr.Op)
	}
	return nil
}

func (g *generator) VisitUnaryExpression(expr *ast.UnaryExpression, w ast.Walker) error {
	if err := w.WalkUnaryExpression(expr); err != nil {
		return err
	}
	switch expr.Op {
	case ast.OpNot:
		ty := g.lookupType(expr).(IntType)
		if ty != Int1Type {
			return errors.Errorf("type of expression is not a supported int type, but %s", ty)
		}
		reg := g.lookupRegisterByNode(expr.Value)
		g.append(&UnaryLogic{register: g.nextRegister(Int1Type), Op: UnaryLogicOpNot, Value: reg}, expr)
	default:
		return errors.Errorf("unsupported unary operator: %s", expr.Op)
	}
	return nil
}

func (g *generator) VisitIfExpression(expr *ast.IfExpression, w ast.Walker) error {
	condBlock := g.newBlock(g.currentBlock)
	g.currentBlock.Terminator = &Jump{Target: condBlock}
	g.currentBlock.Result = NoneRegister
	g.currentBlock = condBlock
	if err := w.WalkNode(expr.Condition); err != nil {
		return err
	}
	trueBlock := g.newBlock(condBlock)
	var falseBlock *Block
	var mergeBlock *Block
	if expr.FalseBody != nil {
		falseBlock = g.newBlock(condBlock)
		mergeBlock = g.newBlock(condBlock, trueBlock, falseBlock)
		falseBlock.Terminator = &Jump{Target: mergeBlock}
	} else {
		mergeBlock = g.newBlock(condBlock, trueBlock)
		// We jump straight to the merge block if we don't have a false branch.
		falseBlock = mergeBlock
	}
	trueBlock.Terminator = &Jump{Target: mergeBlock}
	condBlock.Terminator = &CondBranch{
		Condition:  g.registerByNodeId[expr.Condition.Id()],
		TrueBlock:  trueBlock,
		FalseBlock: falseBlock,
	}
	// Before generating the true and false bodies, we take a snapshot of the symbol table
	// so we can calculate the register constraints afterwards.
	// Remember: We need to constraint registers that point to the same symbol/value
	// so that code generation can make sure they use the same hardware register or stack location.
	symbolTableBeforeBodies := g.symbolTable.copy()
	g.currentBlock = trueBlock
	if err := g.VisitBlockExpression(expr.TrueBody, w); err != nil {
		return err
	}
	if g.currentBlock.Terminator == nil {
		g.currentBlock.Terminator = &Jump{Target: mergeBlock}
	}
	constraints := g.updateRegisterConstraints(symbolTableBeforeBodies)
	if expr.FalseBody != nil {
		g.currentBlock = falseBlock
		if err := g.VisitBlockExpression(expr.FalseBody, w); err != nil {
			return err
		}
		if g.currentBlock.Terminator == nil {
			g.currentBlock.Terminator = &Jump{Target: mergeBlock}
		}
		constraints = append(constraints, g.updateRegisterConstraints(symbolTableBeforeBodies)...)
	}
	// We have to keep all register constraints alive.
	if len(constraints) > 0 {
		mergeBlock.append(&KeepAlive{params: constraints})
	}

	g.currentBlock = mergeBlock
	// We treat if _expressions_ as statements for now.
	condBlock.Result = NoneRegister
	g.registerByNodeId[expr.Id()] = condBlock.Result
	return nil
}

func (g *generator) lookupType(node ast.Node) Type {
	typedType := g.typeInfo.MustLookup(node)
	return g.declaredTypes.MustLookup(typedType)
}

func (g *generator) VisitMemberExpression(expr *ast.MemberExpression, w ast.Walker) error {
	if err := w.WalkMemberExpression(expr); err != nil {
		return err
	}
	source := g.lookupRegisterByNode(expr.Target)
	sourceType := g.lookupType(expr.Target).(*StructType)
	irSourceType := g.typeInfo.MustLookup(expr.Target).(*typed.StructType)
	fieldIndex, found := irSourceType.FindFieldIndex(expr.Field, expr.Span())
	if !found {
		// Note: After lowering there will be no `ast.MemberExpression` that references a
		//       method. All of those have been replaced when lowering to plain function calls.
		return errors.Errorf("field %q not found in struct %q", expr.Field, irSourceType)
	}
	fieldType := sourceType.Fields[fieldIndex]
	getPtrReg := g.nextRegister(PointerType{fieldType})
	g.append(&GetPointer{
		register:   getPtrReg,
		Source:     source,
		SourceType: sourceType,
		FieldIndex: fieldIndex,
	}, expr.Target)
	reg := g.nextRegister(fieldType)
	g.append(&Load{
		register:   reg,
		Source:     getPtrReg,
		TargetType: fieldType,
	}, expr)
	g.registerByNodeId[expr.Id()] = reg
	return nil
}

func (g *generator) VisitBlockExpression(expr *ast.BlockExpression, w ast.Walker) error {
	g.enterScope()
	defer g.exitScope()
	if err := w.WalkBlockExpression(expr); err != nil {
		return err
	}
	var reg = NoneRegister
	if len(expr.Nodes) > 0 {
		lastExpr := expr.Nodes[len(expr.Nodes)-1]
		lastReg, found := g.registerByNodeId[lastExpr.Id()]
		if found {
			reg = lastReg
		}
	}
	g.currentBlock.Result = reg
	g.registerByNodeId[expr.Id()] = reg
	return nil
}

func (g *generator) VisitVariableDefinition(expr *ast.VariableDefinition, w ast.Walker) error {
	if err := w.WalkNode(expr.Value); err != nil {
		return err
	}
	reg := g.lookupRegisterByNode(expr.Value)
	g.symbolTable.declare(expr.Name, reg)
	return nil
}

func (g *generator) VisitAssignmentStatement(stmt *ast.AssignmentStatement, w ast.Walker) error {
	if err := w.WalkNode(stmt.Rhs); err != nil {
		return err
	}
	reg := g.lookupRegisterByNode(stmt.Rhs)
	if stmt.IsAssignToMember() {
		sourceReg := g.symbolTable.mustLookup(stmt.Variable.Ident)
		structType := g.typeInfo.MustLookup(stmt.Variable).(*typed.StructType)
		sourceType := g.lookupType(stmt.Variable).(*StructType)
		fieldIndex, found := structType.FindFieldIndex(ast.MemberExpressionField(*stmt.Field), stmt.Span())
		if !found {
			structSymbol := g.typeInfo.MustLookupSymbol(structType.Id())
			return errors.Errorf("field %q not found in struct %q", *stmt.Field, structSymbol.Name)

		}
		fieldType := sourceType.Fields[fieldIndex]
		getPtrReg := g.nextRegister(PointerType{fieldType})
		g.append(&GetPointer{
			register:   getPtrReg,
			Source:     sourceReg,
			SourceType: sourceType,
			FieldIndex: fieldIndex,
		}, nil)
		g.append(&Store{
			Target: getPtrReg,
			Value:  reg,
			Type:   fieldType,
		}, stmt)
	} else {
		g.symbolTable.assign(stmt.Variable.Ident, reg)
	}
	return nil
}

func (g *generator) VisitLoopStatement(stmt *ast.LoopStatement, w ast.Walker) error {
	loopStartBlock := g.newBlock(g.currentBlock)
	exitBlock := g.newBlock(loopStartBlock)
	g.currentBlock.Terminator = &Jump{Target: loopStartBlock}
	g.currentBlock.Result = NoneRegister
	g.currentBlock = loopStartBlock
	loopScope := g.enterLoop(loopStartBlock, exitBlock)
	// In order to add the register constraints we need to first take a snapshot
	// of the current symbol table.
	symbolTableBeforeBody := g.symbolTable.copy()
	if err := w.WalkNode(stmt.Body); err != nil {
		return err
	}
	constraints := g.updateRegisterConstraints(symbolTableBeforeBody)
	// We have to keep all register constraints alive.
	if len(constraints) > 0 {
		exitBlock.append(&KeepAlive{params: constraints})
	}
	// Also keep alive all registers that existed before the loop
	// and are referenced within the loop.
	if len(loopScope.usedRegisters) > 0 {
		regs := []Register{}
		for regId := range loopScope.usedRegisters {
			regs = append(regs, Register{Id: regId})
		}
		exitBlock.append(&KeepAlive{params: regs})
	}
	g.exitLoop()
	g.currentBlock.Terminator = &Jump{Target: loopStartBlock}
	g.currentBlock.Result = NoneRegister
	g.currentBlock = exitBlock
	g.registerByNodeId[stmt.Id()] = NoneRegister
	return nil
}

func (g *generator) VisitBreakStatement(stmt *ast.BreakStatement) error {
	loopScope := g.loopScope()
	g.currentBlock.Terminator = &Jump{Target: loopScope.exitBlock}
	g.currentBlock.Result = NoneRegister
	g.currentBlock = g.newBlock(nil)
	return nil
}

func (g *generator) VisitContinueStatement(stmt *ast.ContinueStatement) error {
	loopScope := g.loopScope()
	g.currentBlock.Terminator = &Jump{Target: loopScope.loopBlock}
	g.currentBlock.Result = NoneRegister
	g.currentBlock = g.newBlock(nil)
	return nil
}

func (g *generator) VisitReturnStatement(stmt *ast.ReturnStatement, w ast.Walker) error {
	if err := w.WalkNode(stmt.Value); err != nil {
		return err
	}
	reg := g.lookupRegisterByNode(stmt.Value)
	g.currentBlock.Terminator = &Return{}
	g.currentBlock.Result = reg
	g.currentBlock = g.newBlock(nil)
	return nil
}

type DeclaredTypes struct {
	Types map[typed.TypeId]Type
}

func (dt *DeclaredTypes) MustLookup(ty typed.Type) Type {
	switch ty := ty.(type) {
	case *typed.NoneType, *typed.NeverType:
		return noneType
	case *typed.BoolType:
		return Int1Type
	case *typed.Int8Type:
		return Int8Type
	case *typed.Int16Type:
		return Int16Type
	case *typed.Int32Type:
		return Int32Type
	case *typed.Int64Type, *typed.RawPtr:
		return Int64Type
	case *typed.UInt8Type:
		return UInt8Type
	case *typed.UInt16Type:
		return UInt16Type
	case *typed.UInt32Type, *typed.CharType:
		return UInt32Type
	case *typed.UInt64Type:
		return UInt64Type
	case *typed.StructType, *typed.FunctionType:
		if res, found := dt.Types[ty.Id()]; found {
			return res
		}
		dt.declare(ty)
		return dt.Types[ty.Id()]
	}
	panic(fmt.Sprintf("type not found for %s (%T)", ty, ty))
}

func (dt *DeclaredTypes) declare(ty typed.Type) {
	switch ty := ty.(type) {
	case *typed.StructType:
		fieldTypes := []Type{}
		for _, field := range ty.Fields {
			fieldType := dt.MustLookup(field.Type)
			if _, isStructType := fieldType.(*StructType); isStructType {
				// We only ever store references (i.e. pointers) to struct types.
				fieldType = &PointerType{fieldType}
			}
			fieldTypes = append(fieldTypes, fieldType)
		}
		structType := &StructType{Fields: fieldTypes}
		dt.Types[ty.Id()] = structType
	case *typed.FunctionType:
		params := make([]FunctionParam, len(ty.Params))
		for i, tyParam := range ty.Params {
			paramType := dt.MustLookup(tyParam.Type)
			params[i] = FunctionParam{Type: paramType, Register: newRegister(i+1, paramType)}
		}
		result := dt.MustLookup(ty.Result)
		funcType := &FunctionType{Params: params, Result: result}
		dt.Types[ty.Id()] = funcType
	default:
		panic(fmt.Sprintf("cannot declare type %T", ty))
	}
}

func declareFunction(
	declaredTypes *DeclaredTypes,
	rootSymbolTable *symbolTable,
	functionType *typed.FunctionType,
) *FunctionDefinition {
	params := []FunctionParam{}
	for i, tyParam := range functionType.Params {
		if callArgFuncType, ok := tyParam.Type.(*typed.FunctionType); ok {
			declareFunction(declaredTypes, rootSymbolTable, callArgFuncType)
		}
		paramType := declaredTypes.MustLookup(tyParam.Type)
		param := FunctionParam{
			Type:     paramType,
			Register: newRegister(i+1, paramType),
		}
		params = append(params, param)
	}
	result := declaredTypes.MustLookup(functionType.Result)
	res := &FunctionDefinition{
		Id:   functionType.Id(),
		Type: FunctionType{Params: params, Result: result},
	}
	return res
}

func GenerateIR(lowered *lower.LoweredAST, dataLayout DataLayout) (*Module, error) {
	typeInfo := lowered.TypeInfo
	declaredTypes := &DeclaredTypes{Types: make(map[typed.TypeId]Type)}
	rootSymbolTable := symbolTable{symbols: make(map[ast.Ident]Register)}
	funcSpecs := lowered.FuncSpecs
	funcDefs := []*FunctionDefinition{}
	definedFunctions := make(map[typed.TypeId]DefinedFunction)
	var main *FunctionDefinition
	// First forward declare all functions.
	for _, funcSpec := range funcSpecs {
		funcType := funcSpec.Specialized
		funcDef := declareFunction(declaredTypes, &rootSymbolTable, funcType)
		funcDefs = append(funcDefs, funcDef)
		if funcSpec.IsMain {
			main = funcDef
		}
		declaredTypes.declare(funcType)
		fqn := typeInfo.MustLookupSymbol(funcType.Id()).FQN()
		definedFunctions[funcDef.Id] = DefinedFunction{Id: funcDef.Id, FQN: fqn}
	}
	// Declare builtin functions.
	declareBuiltInFunction := func(f *typed.FunctionType, declareType bool) {
		if declareType {
			declaredTypes.declare(f)
		}
		symbol := typeInfo.MustLookupSymbol(f.Id())
		definedFunctions[f.Id()] = DefinedFunction{Id: f.Id(), FQN: symbol.FQN()}
	}
	declareBuiltInFunction(typed.BuiltInPrintFunction, true)
	declareBuiltInFunction(typed.BuiltInPrintCharFunction, true)
	declareBuiltInFunction(typed.BuiltInPrintIntFunction, true)
	declareBuiltInFunction(typed.BuiltInPrintUIntFunction, true)
	declareBuiltInFunction(typed.BuiltInPrintBoolFunction, true)
	declareBuiltInFunction(typed.BuiltInInternalMallocFunction, true)
	declareBuiltInFunction(typed.BuiltInInternalFreeFunction, true)
	declareBuiltInFunction(typed.BuiltInInternalWritePtrFunction, false)
	declareBuiltInFunction(typed.BuiltInInternalReadPtrFunction, false)
	declareBuiltInFunction(typed.BuiltInInternalExitFunction, true)
	declareBuiltInFunction(typed.BuiltInSizeOfFunction, true)
	constants := []*StrConst{}
	// Generate code for each function specialization.
	for i, funcDef := range funcDefs {
		funcSpec := funcSpecs[i]
		gen := &generator{
			DefaultVisitor:      ast.DefaultVisitor{},
			typeInfo:            funcSpec.TypeInfo,
			registerByNodeId:    make(map[ast.NodeId]Register),
			symbolTable:         &symbolTable{symbols: make(map[ast.Ident]Register), parent: &rootSymbolTable},
			globalConstants:     &constants,
			declaredTypes:       declaredTypes,
			registerConstraints: RegisterConstraints{},
			loopScopes:          []loopScope{},
			definedFunctions:    definedFunctions,
			dataLayout:          dataLayout,
		}
		// Make function parameters visible.
		paramRegs := []Register{}
		for _, param := range funcSpec.Specialized.Params {
			ty := declaredTypes.MustLookup(param.Type)
			reg := gen.nextRegister(ty)
			paramRegs = append(paramRegs, reg)
			gen.symbolTable.declare(param.Name, reg)
		}
		block := gen.newBlock()
		gen.currentBlock = block
		walker := &ast.DefaultWalker{Visitor: gen}
		if err := walker.WalkNode(funcSpec.FuncDef.Body); err != nil {
			return nil, err
		}
		if gen.currentBlock.Terminator != nil {
			return nil, errors.Errorf("expecting the last block to not have a terminator, but got: %s", block.Terminator)
		}
		gen.currentBlock.Terminator = &Return{}
		funcDef.Entry = block
		funcDef.RegisterConstraints = &gen.registerConstraints
		funcDef.RegisterExpirations = calculateRegisterExpirations(funcDef.Entry, paramRegs)
	}
	return &Module{
		DataLayout:    dataLayout,
		Functions:     funcDefs,
		Constants:     constants,
		DeclaredTypes: declaredTypes,
		Main:          main,
		TypeInfo:      typeInfo,
	}, nil
}

// Walk the given block and call `visitor` for each block we discover in the graph
// of reachable blocks. It is guaranteed that each unique block is only visited once.
func WalkBlock(block *Block, visit func(block *Block) error) error {
	visited := make(map[BlockId]bool)
	blocks := []*Block{block}
	i := 0
	for i < len(blocks) {
		block := blocks[i]
		i += 1
		if visited[block.Id] {
			continue
		}
		if err := visit(block); err != nil {
			return err
		}
		visited[block.Id] = true
		if block.Terminator == nil {
			return errors.Errorf("block %s has no terminator", block.Id)
		}
		blocks = append(blocks, block.Terminator.Targets()...)
	}
	return nil
}
