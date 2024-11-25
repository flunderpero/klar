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

type BuiltInType string

const (
	NoneType  BuiltInType = "none"
	Int1Type  BuiltInType = "i1"
	Int8Type  BuiltInType = "i8"
	Int32Type BuiltInType = "i32"
	Int64Type BuiltInType = "i64"
)

func (t BuiltInType) String() string {
	return string(t)
}

type PointerType struct {
	ElementType Type
}

func (t PointerType) String() string {
	return fmt.Sprintf("%s*", t.ElementType)
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

type Return struct{}

func (ir Return) String() string {
	return "ret"
}

func (ir *Return) Targets() []*Block {
	return []*Block{}
}

type FunctionDefinition struct {
	Id                  typed.TypeId
	Type                FunctionType
	TypeInfo            *lower.SpecializedTypeInfo
	Entry               *Block
	RegisterConstraints RegisterConstraints
}

func (t FunctionDefinition) String() string {
	name := t.TypeInfo.MustLookupSymbol(t.Id).FQN()
	return fmt.Sprintf("declare %s %s", name, t.Type)
}

type Module struct {
	DataLayout    DataLayout
	Functions     []*FunctionDefinition
	Constants     []*StrConst
	DeclaredTypes *DeclaredTypes
	Main          *FunctionDefinition
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
		writeln(function)
		writeln("{")
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

type RegisterId string

type Register struct {
	Id   RegisterId
	Type Type
}

func (r Register) getPointerSourceMarker() {}

func (r Register) calleeMarker() {}

func (r Register) String() string {
	return string(r.Id)
}

func newRegister(id int, ty Type) Register {
	return Register{Id: RegisterId(fmt.Sprintf("%%%d", id)), Type: ty}
}

var NoneRegister = Register{Id: "none", Type: NoneType}

type Instruction interface {
	String() string
	Register() Register
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

type Int64Const struct {
	register Register
	Value    int64
}

func (i Int64Const) String() string {
	return fmt.Sprintf("%s = i64 %d", i.register, i.Value)
}

func (i *Int64Const) Register() Register {
	return i.register
}

type Int32Const struct {
	register Register
	Value    int64
}

func (i Int32Const) String() string {
	return fmt.Sprintf("%s = i32 %d", i.register, i.Value)
}

func (i *Int32Const) Register() Register {
	return i.register
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

type Load struct {
	register   Register
	Source     Register
	TargetType Type
}

func (i *Load) Register() Register {
	return i.register
}

func (i Load) String() string {
	return fmt.Sprintf("%s = load %s %s", i.register, i.TargetType, i.Source)
}

type Store struct {
	Target    Register
	Value     Register
	ValueType Type
}

func (s *Store) Register() Register {
	return NoneRegister
}

func (s Store) String() string {
	return fmt.Sprintf("store %s %s, %s", s.ValueType, s.Value, s.Target)
}

type SignedInt64AddWithOverflow struct {
	register Register
	Lhs      Register
	Rhs      Register
}

func (i *SignedInt64AddWithOverflow) Register() Register {
	return i.register
}

func (i SignedInt64AddWithOverflow) String() string {
	return fmt.Sprintf("%s = iaddo i64 %s, i64 %s", i.register, i.Lhs, i.Rhs)
}

type IntCompOp string

const (
	IntCompOpEQ IntCompOp = "eq"
)

type IntCompare struct {
	register Register
	Lhs      Register
	Rhs      Register
	Op       IntCompOp
	IntType  BuiltInType
}

func (i IntCompare) Register() Register {
	return i.register
}

func (i IntCompare) String() string {
	return fmt.Sprintf("%s = icmp %s %s %s, %s", i.register, i.Op, i.IntType, i.Lhs, i.Rhs)
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
	if inst.register.Type != NoneType {
		assign = fmt.Sprintf("%s = ", inst.register)
	}
	prefix := ""
	if inst.IsIndirect {
		prefix = "i"
	}
	return fmt.Sprintf("%s%scall %s %s(%s)", assign, prefix, inst.FunctionType.Result, inst.Callee, args)
}

func (inst *Call) Register() Register {
	return inst.register
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
	loopBlock *Block
	exitBlock *Block
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
	definedFunctions    *map[typed.TypeId]DefinedFunction
	dataLayout          DataLayout
}

func (g *generator) enterScope() {
	g.symbolTable = &symbolTable{symbols: make(map[ast.Ident]Register), parent: g.symbolTable}
}

func (g *generator) exitScope() {
	g.symbolTable = g.symbolTable.parent
}

func (g *generator) enterLoop(loopBlock *Block, exitBlock *Block) {
	g.loopScopes = append(g.loopScopes, loopScope{loopBlock: loopBlock, exitBlock: exitBlock})
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

func (g *generator) updateRegisterConstraints(symbolTableBefore map[ast.Ident]Register) {
	for symbol, regBefore := range symbolTableBefore {
		regNow := g.symbolTable.mustLookup(symbol)
		if regNow.Id != regBefore.Id {
			g.registerConstraints.add(regNow, regBefore)
		}
	}
}

func (g *generator) isDefinedFunction(id typed.TypeId) (DefinedFunction, bool) {
	res, ok := (*g.definedFunctions)[id]
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
	g.append(&Int64Const{
		register: g.nextRegister(Int64Type),
		Value:    expr.Value,
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
	if definedFunc, ok := g.isDefinedFunction(typedTy.Id()); ok {
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
		mallocFuncType := g.declaredTypes.MustLookup(typed.BuiltInUnsafeMallocFunction).(*FunctionType)
		g.append(&Int64Const{
			register: sizeReg,
			Value:    int64(g.dataLayout.SizeOf(structType)),
		}, nil)
		mallocSymbol := g.typeInfo.MustLookupSymbol(typed.BuiltInUnsafeMallocFunction.Id())
		g.append(&Call{
			register:     mallocReg,
			Callee:       DefinedFunction{Id: typed.BuiltInUnsafeMallocFunction.Id(), FQN: mallocSymbol.FQN()},
			FunctionType: mallocFuncType,
			Args:         []Register{sizeReg},
		}, expr)
		for i, callArg := range expr.Args {
			fieldType := structType.Fields[i]
			switch fieldType.(type) {
			case BuiltInType, *PointerType:
			default:
				return errors.Errorf("only BuiltInType and PointerType can be stored in struct fields, got %q", fieldType)
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
				Target:    fieldPtrReg,
				Value:     fieldValueReg,
				ValueType: fieldType,
			}, nil)
		}
		g.registerByNodeId[expr.Id()] = mallocReg
	case *typed.FunctionType:
		var callee Callee
		if definedFunc, ok := g.isDefinedFunction(calleeType.Id()); ok {
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
		ty := g.typeInfo.MustLookup(expr.Callee)
		funcType := g.declaredTypes.MustLookup(ty).(*FunctionType)
		var reg Register = NoneRegister
		if funcType.Result != NoneType {
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
		ty := g.typeInfo.MustLookup(expr)
		if ty != typed.Int64Type {
			// For now we only support 64 bit integers.
			return errors.Errorf("add expression must be of type Int64Type, got %s", ty)
		}
		g.append(&SignedInt64AddWithOverflow{register: g.nextRegister(Int64Type), Lhs: lhs, Rhs: rhs}, expr)
	case ast.OpEquality:
		// For now, we only know how to compare 64 and 1 bit integers.
		ty := g.lookupType(expr.Lhs).(BuiltInType)
		if ty != Int64Type && ty != Int1Type {
			return errors.Errorf("type of lhs is not a supported int type, but %s", ty)
		}
		g.append(&IntCompare{register: g.nextRegister(Int1Type), Op: IntCompOpEQ, IntType: ty, Lhs: lhs, Rhs: rhs}, expr)
	default:
		return errors.Errorf("unsupported binary operator: %s", expr.Op)
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
	g.updateRegisterConstraints(symbolTableBeforeBodies)
	if expr.FalseBody != nil {
		g.currentBlock = falseBlock
		if err := g.VisitBlockExpression(expr.FalseBody, w); err != nil {
			return err
		}
		g.updateRegisterConstraints(symbolTableBeforeBodies)
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
		fieldIndex, found := structType.FindFieldIndex(*stmt.Field, stmt.Span())
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
			Target:    getPtrReg,
			Value:     reg,
			ValueType: fieldType,
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
	g.enterLoop(loopStartBlock, exitBlock)
	// In order to add the register constraints we need to first take a snapshot
	// of the current symbol table.
	symbolTableBeforeBody := g.symbolTable.copy()
	if err := w.WalkNode(stmt.Body); err != nil {
		return err
	}
	g.updateRegisterConstraints(symbolTableBeforeBody)
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

type DeclaredTypes struct {
	Types map[typed.TypeId]Type
}

func (dt *DeclaredTypes) MustLookup(ty typed.Type) Type {
	switch ty {
	case typed.NoneType:
		return NoneType
	case typed.StrType:
		return StrType
	case typed.BoolType:
		return Int1Type
	case typed.Int64Type:
		return Int64Type
	}
	switch ty := ty.(type) {
	case *typed.StructType, *typed.FunctionType:
		if res, found := dt.Types[ty.Id()]; found {
			return res
		}
		dt.declare(ty)
		return dt.Types[ty.Id()]
	case *typed.TypeParam:
		return NoneType
	}
	panic(fmt.Sprintf("type not found for %T", ty))
}

func (dt *DeclaredTypes) declare(ty typed.Type) {
	if typeDecl, ok := ty.(*typed.DeclaredType); ok {
		ty = typeDecl.Type
	}
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
	typeInfo *lower.SpecializedTypeInfo,
) *FunctionDefinition {
	params := []FunctionParam{}
	for i, tyParam := range functionType.Params {
		if callArgFuncType, ok := tyParam.Type.(*typed.FunctionType); ok {
			declareFunction(declaredTypes, rootSymbolTable, callArgFuncType, typeInfo)
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
		Id:       functionType.Id(),
		Type:     FunctionType{Params: params, Result: result},
		TypeInfo: typeInfo,
	}
	return res
}

func GenerateIR(lowered *lower.LoweredAST, dataLayout DataLayout) (*Module, error) {
	declaredTypes := &DeclaredTypes{Types: make(map[typed.TypeId]Type)}
	rootSymbolTable := symbolTable{symbols: make(map[ast.Ident]Register)}
	funcSpecs := lowered.FuncSpecs
	funcDefs := []*FunctionDefinition{}
	definedFunctions := make(map[typed.TypeId]DefinedFunction)
	var main *FunctionDefinition
	// First forward declare all functions.
	for _, funcSpec := range funcSpecs {
		funcType := funcSpec.Specialized
		funcDef := declareFunction(declaredTypes, &rootSymbolTable, funcType, funcSpec.TypeInfo)
		funcDefs = append(funcDefs, funcDef)
		if funcSpec.IsMain {
			main = funcDef
		}
		declaredTypes.declare(funcType)
		fqn := funcSpec.TypeInfo.MustLookupSymbol(funcType.Id()).FQN()
		definedFunctions[funcDef.Id] = DefinedFunction{Id: funcDef.Id, FQN: fqn}
	}
	// Declare builtin functions.
	declareBuiltInFunction := func(f *typed.FunctionType) {
		declaredTypes.declare(f)
		definedFunctions[f.Id()] = DefinedFunction{Id: f.Id()}
	}
	declareBuiltInFunction(typed.BuiltInPrintFunction)
	declareBuiltInFunction(typed.BuiltInPrintIntFunction)
	declareBuiltInFunction(typed.BuiltInPrintBoolFunction)
	declareBuiltInFunction(typed.BuiltInUnsafeMallocFunction)
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
			definedFunctions:    &definedFunctions,
			dataLayout:          dataLayout,
		}
		// Make function parameters visible.
		for _, param := range funcSpec.Specialized.Params {
			ty := declaredTypes.MustLookup(param.Type)
			reg := gen.nextRegister(ty)
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
		funcDef.RegisterConstraints = gen.registerConstraints
	}
	return &Module{
		DataLayout:    dataLayout,
		Functions:     funcDefs,
		Constants:     constants,
		DeclaredTypes: declaredTypes,
		Main:          main,
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
