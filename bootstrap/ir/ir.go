package ir

import (
	"fmt"
	"slices"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
	"github.com/pkg/errors"
)

type Type interface {
	String() string
	Size() int
}

type BuiltInType string

const (
	UnitType  BuiltInType = "unit"
	BoolType  BuiltInType = "i1"
	Int8Type  BuiltInType = "i8"
	Int32Type BuiltInType = "i32"
	Int64Type BuiltInType = "i64"
)

func (t BuiltInType) String() string {
	return string(t)
}

func (t BuiltInType) Size() int {
	switch t {
	case UnitType:
		return 0
	case BoolType:
		return 1
	case Int8Type:
		return 1
	case Int32Type:
		return 4
	case Int64Type:
		return 8
	default:
		panic(fmt.Sprintf("Unknown basic type: %s", t))
	}
}

type PointerType struct {
	ElementType Type
}

func (t PointerType) String() string {
	return fmt.Sprintf("%s*", t.ElementType)
}

func (t PointerType) Size() int {
	return 8
}

type StructType struct {
	Name   ast.TypeIdent
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
	return fmt.Sprintf("struct %s {%s}", t.Name, fields)
}

func (t StructType) Size() int {
	size := 0
	for _, field := range t.Fields {
		size += field.Size()
	}
	return size
}

var StrType = &StructType{Name: "Str", Fields: []Type{Int64Type, &PointerType{Int8Type}}}

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

func (ir *Block) String() string {
	s := ""
	for _, inst := range ir.Instructions {
		s += fmt.Sprintf("\n    %s", inst)
	}
	s += fmt.Sprintf("\n    %s", ir.Terminator)
	return fmt.Sprintf("%s:%s\n    -- block_result = %s", ir.Id, s, ir.Result)
}

type Terminator interface {
	String() string
	Targets() []*Block
}

type Jump struct {
	Target *Block
}

func (ir *Jump) String() string {
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

func (ir *CondBranch) String() string {
	return fmt.Sprintf("condbr i1 %s, %s, %s", ir.Condition, ir.TrueBlock.Id, ir.FalseBlock.Id)
}

func (ir *CondBranch) Targets() []*Block {
	return []*Block{ir.TrueBlock, ir.FalseBlock}
}

type Return struct{}

func (ir *Return) String() string {
	return "ret"
}

func (ir *Return) Targets() []*Block {
	return []*Block{}
}

type FunctionArg struct {
	Type     Type
	Register Register
}

type Function struct {
	Name                ast.Ident
	ReturnType          Type
	Args                []FunctionArg
	Entry               *Block
	RegisterConstraints RegisterConstraints
}

func (t *Function) String() string {
	args := ""
	for _, arg := range t.Args {
		if len(args) > 0 {
			args += ", "
		}
		args += fmt.Sprintf("%s %s", arg.Type, arg.Register)
	}
	return fmt.Sprintf("@declare %s %s(%s)", t.ReturnType, t.Name, args)
}

type Module struct {
	Functions     []*Function
	Constants     []*StrConst
	DeclaredTypes *DeclaredTypes
}

type Register string

func (r Register) String() string {
	return string(r)
}

func (r Register) IsConstant() bool {
	return string(r)[0] == '_'
}

func (r Register) IsUnit() bool {
	return string(r) == "()"
}

const UnitRegister Register = "()"

type Instruction interface {
	String() string
	Register() Register
}

type StrConst struct {
	register Register
	Value    string
}

func (i *StrConst) String() string {
	return fmt.Sprintf("%s = Str %q", i.register, i.Value)
}

func (i *StrConst) Register() Register {
	return i.register
}

type Int64Const struct {
	register Register
	Value    int64
}

func (i *Int64Const) String() string {
	return fmt.Sprintf("%s = i64 %d", i.register, i.Value)
}

func (i *Int64Const) Register() Register {
	return i.register
}

type Int32Const struct {
	register Register
	Value    int64
}

func (i *Int32Const) String() string {
	return fmt.Sprintf("%s = i32 %d", i.register, i.Value)
}

func (i *Int32Const) Register() Register {
	return i.register
}

type BoolConst struct {
	register Register
	Value    int
}

func (i *BoolConst) String() string {
	return fmt.Sprintf("%s = i1 %d", i.register, i.Value)
}

func (i *BoolConst) Register() Register {
	return i.register
}

type GetPointer struct {
	register   Register
	Source     Register
	SourceType Type
	FieldIndex int
}

func (i *GetPointer) String() string {
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

func (i *Load) String() string {
	return fmt.Sprintf("%s = load %s %s", i.register, i.TargetType, i.Source)
}

type Store struct {
	Target    Register
	Value     Register
	ValueType Type
}

func (s *Store) Register() Register {
	return UnitRegister
}

func (s *Store) String() string {
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

func (i *SignedInt64AddWithOverflow) String() string {
	return fmt.Sprintf("%s = iaddo i64 %s, i64 %s", i.register, i.Lhs, i.Rhs)
}

type Int64CompOp string

const (
	Int64CompOpEQ Int64CompOp = "eq"
)

type Int64Compare struct {
	register Register
	Lhs      Register
	Rhs      Register
	Op       Int64CompOp
}

func (i *Int64Compare) Register() Register {
	return i.register
}

func (i *Int64Compare) String() string {
	return fmt.Sprintf("%s = icmp %s i64 %s, %s", i.register, i.Op, i.Lhs, i.Rhs)
}

type Call struct {
	register Register
	Function *Function
	Args     []Register
}

func (inst *Call) String() string {
	args := ""
	for i, reg := range inst.Args {
		arg := inst.Function.Args[i]
		if len(args) > 0 {
			args += ", "
		}
		args += fmt.Sprintf("%s %s", arg.Type, reg)
	}
	return fmt.Sprintf("%s = call %s %s (%s)", inst.register, inst.Function.ReturnType, inst.Function.Name, args)
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
	%3 = condbr i1 %2, true_block, else_block

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

func (r *RegisterConstraints) String() string {
	s := ""
	for _, c := range r.constraints {
		if len(s) > 0 {
			s += ", "
		}
		s += "["
		for i, reg := range *c {
			if i > 0 {
				s += ", "
			}
			s += reg.String()
		}
		s += "]"
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

func (s *symbolTable) lookup(name ast.Ident) Register {
	table := s
	for table != nil {
		if reg, found := table.symbols[name]; found {
			return reg
		}
		table = table.parent
	}
	panic(fmt.Sprintf("undeclared symbol: %s", name))
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
	typeInfo            *typed.TypeInfo
	registerByNodeId    map[ast.NodeId]Register
	symbolTable         *symbolTable
	globalConstants     *[]*StrConst
	registerIndex       int
	blockIndex          int
	functions           map[ast.Ident]*Function
	declaredTypes       *DeclaredTypes
	registerConstraints RegisterConstraints
	loopScopes          []loopScope
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

func (g *generator) nextRegister() Register {
	g.registerIndex++
	return Register(fmt.Sprintf("%%%d", g.registerIndex))
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
		regNow := g.symbolTable.lookup(symbol)
		if regNow != regBefore {
			g.registerConstraints.add(regNow, regBefore)
		}
	}
}

func (g *generator) VisitStringLiteralExpression(expr *ast.StringLiteralExpression) error {
	reg := Register(fmt.Sprintf("_const_%d", len(*g.globalConstants)))
	*g.globalConstants = append(*g.globalConstants, &StrConst{register: reg, Value: expr.Value})
	g.append(&GetPointer{
		register:   g.nextRegister(),
		Source:     reg,
		SourceType: StrType,
		FieldIndex: 0,
	}, expr)
	return nil
}

func (g *generator) VisitIntLiteralExpression(expr *ast.IntLiteralExpression) error {
	g.append(&Int64Const{
		register: g.nextRegister(),
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
		register: g.nextRegister(),
		Value:    value,
	}, expr)
	return nil
}

func (g *generator) VisitReferenceExpression(expr ast.ReferenceExpression) error {
	switch expr := expr.(type) {
	case *ast.IdentExpression:
		if _, found := g.functions[expr.Ident]; found {
			return nil
		}
		reg := g.symbolTable.lookup(expr.Ident)
		g.registerByNodeId[expr.Id()] = reg
	case *ast.TypeIdentExpression:
	default:
		panic(fmt.Sprintf("VisitReferenceExpression not implemented for expression type: %T", expr))
	}
	return nil
}

func (g *generator) VisitCallExpression(expr *ast.CallExpression, w ast.Walker) error {
	if err := w.WalkCallExpression(expr); err != nil {
		return err
	}
	funcType := g.typeInfo.MustLookup(expr.Callee).(*typed.FunctionType)
	function, ok := g.functions[funcType.Name]
	if !ok {
		panic(fmt.Sprintf("Unknown function: %s", funcType.Name))
	}
	args := []Register{}
	for _, arg := range expr.Args {
		args = append(args, g.lookupRegisterByNode(arg))
	}
	var reg Register = UnitRegister
	if function.ReturnType != UnitType {
		reg = g.nextRegister()
	}
	g.append(&Call{
		register: reg,
		Function: function,
		Args:     args,
	}, expr)
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
		g.append(&SignedInt64AddWithOverflow{register: g.nextRegister(), Lhs: lhs, Rhs: rhs}, expr)
	case ast.OpEquality:
		// For now, we only know how to compare 64 bit integers.
		lhsType := g.typeInfo.MustLookup(expr.Lhs)
		if lhsType != typed.Int64Type {
			return errors.Errorf("type of lhs is not Int64Type, but %s", lhsType)
		}
		rhsType := g.typeInfo.MustLookup(expr.Rhs)
		if rhsType != typed.Int64Type {
			return errors.Errorf("type of rhs is not Int64Type, but %s", rhsType)
		}
		g.append(&Int64Compare{register: g.nextRegister(), Op: Int64CompOpEQ, Lhs: lhs, Rhs: rhs}, expr)
	default:
		return errors.Errorf("unsupported binary operator: %s", expr.Op)
	}
	return nil
}

func (g *generator) VisitIfExpression(expr *ast.IfExpression, w ast.Walker) error {
	condBlock := g.newBlock(g.currentBlock)
	g.currentBlock.Terminator = &Jump{Target: condBlock}
	g.currentBlock.Result = UnitRegister
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
	// Remember we need to constraint registers that point to the same symbol/value
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
	condBlock.Result = UnitRegister
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
	fieldIndex := slices.IndexFunc(
		irSourceType.Fields, func(field typed.StructField) bool { return field.Name == expr.Field },
	)
	if fieldIndex == -1 {
		if _, err := irSourceType.FindMethod(expr.Field); err == nil {
			return nil
		}
		return errors.Errorf("field %q not found in struct %q", expr.Field, irSourceType.Name)
	}
	fieldType := sourceType.Fields[fieldIndex]
	getPtrReg := g.nextRegister()
	g.append(&GetPointer{
		register:   getPtrReg,
		Source:     source,
		SourceType: sourceType,
		FieldIndex: fieldIndex,
	}, expr.Target)
	reg := g.nextRegister()
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
	lastExpr := expr.Nodes[len(expr.Nodes)-1]
	reg, found := g.registerByNodeId[lastExpr.Id()]
	if !found {
		reg = UnitRegister
	}
	g.currentBlock.Result = reg
	g.registerByNodeId[expr.Id()] = reg
	return nil
}

func (g *generator) VisitStructInitExpression(expr *ast.StructInitExpression, w ast.Walker) error {
	if err := w.WalkStructInitExpression(expr); err != nil {
		return err
	}
	structType := g.lookupType(expr).(*StructType)
	sizeReg := g.nextRegister()
	mallocReg := g.nextRegister()
	g.append(&Int64Const{
		register: sizeReg,
		Value:    int64(structType.Size()),
	}, nil)
	g.append(&Call{
		register: mallocReg,
		Function: g.functions["__unsafe_malloc"],
		Args:     []Register{sizeReg},
	}, expr)
	for i, astField := range expr.Fields {
		fieldType := structType.Fields[i]
		switch fieldType.(type) {
		case BuiltInType, *PointerType:
		default:
			return errors.Errorf("only BuiltInType and PointerType can be stored in struct fields, got %q", fieldType)
		}
		fieldValueReg := g.lookupRegisterByNode(astField.Value)
		fieldPtrReg := g.nextRegister()
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
		getPtrReg := g.nextRegister()
		sourceReg := g.symbolTable.lookup(stmt.Variable.Ident)
		structType := g.typeInfo.MustLookup(stmt.Variable).(*typed.StructType)
		sourceType := g.lookupType(stmt.Variable).(*StructType)
		fieldIndex, err := structType.FindFieldIndex(*stmt.Field)
		fieldType := sourceType.Fields[fieldIndex]
		if err != nil {
			return err
		}
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
	g.currentBlock.Result = UnitRegister
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
	g.currentBlock.Result = UnitRegister
	g.currentBlock = exitBlock
	g.registerByNodeId[stmt.Id()] = UnitRegister
	return nil
}

func (g *generator) VisitBreakStatement(stmt *ast.BreakStatement) error {
	loopScope := g.loopScope()
	g.currentBlock.Terminator = &Jump{Target: loopScope.exitBlock}
	g.currentBlock.Result = UnitRegister
	g.currentBlock = g.newBlock(nil)
	return nil
}

func (g *generator) VisitContinueStatement(stmt *ast.ContinueStatement) error {
	loopScope := g.loopScope()
	g.currentBlock.Terminator = &Jump{Target: loopScope.loopBlock}
	g.currentBlock.Result = UnitRegister
	g.currentBlock = g.newBlock(nil)
	return nil
}

type DeclaredTypes struct {
	Types map[ast.TypeIdent]Type
}

func (dt *DeclaredTypes) MustLookup(ty typed.Type) Type {
	switch ty {
	case typed.UnitType:
		return UnitType
	case typed.StrType:
		return StrType
	case typed.Int64Type:
		return Int64Type
	}
	switch ty := ty.(type) {
	case *typed.StructType:
		return dt.Types[ty.Name]
	default:
		panic(fmt.Sprintf("type not found for %T", ty))
	}
}

func (dt *DeclaredTypes) declare(ty typed.Type) {
	typeDecl, ok := ty.(*typed.DeclaredType)
	if !ok {
		panic(fmt.Sprintf("cannot declare type %T", ty))
	}
	switch ty := typeDecl.Type.(type) {
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
		structType := &StructType{Name: ty.Name, Fields: fieldTypes}
		dt.Types[structType.Name] = structType
	default:
		panic(fmt.Sprintf("cannot declare type %T", ty))
	}
}

func GenerateIR(module *ast.Module, typeInfo *typed.TypeInfo) (*Module, error) {
	functionDefinitions := []*ast.FunctionDefinition{}
	declaredTypes := &DeclaredTypes{Types: make(map[ast.TypeIdent]Type)}
	for _, node := range module.Nodes {
		switch node := node.(type) {
		case *ast.FunctionDefinition:
			functionDefinitions = append(functionDefinitions, node)
		case *ast.ImplDefinition:
			functionDefinitions = append(functionDefinitions, node.Methods...)
		case *ast.StructTypeDeclaration:
			ty := typeInfo.MustLookup(node)
			declaredTypes.declare(ty)
		default:
			return nil, errors.Errorf("cannot generate IR for node type: %T", node)
		}
	}
	functions := []*Function{}
	// First forward declare all functions.
	for _, functionDef := range functionDefinitions {
		funcType := typeInfo.MustLookupDeclaredType(functionDef).Type.(typed.CallableType)
		args := []FunctionArg{}
		for i, arg := range funcType.CallArgTypes() {
			argType := declaredTypes.MustLookup(arg.Type)
			irArg := FunctionArg{
				Type:     argType,
				Register: Register(fmt.Sprintf("%%%d", (i + 1))),
			}
			args = append(args, irArg)
		}
		returnType := declaredTypes.MustLookup(funcType.CallReturnType())
		f := Function{
			Name:       ast.Ident(funcType.TypeName()),
			ReturnType: returnType,
			Args:       args,
		}
		functions = append(functions, &f)
	}
	functionByName := make(map[ast.Ident]*Function)
	for _, f := range functions {
		functionByName[f.Name] = f
	}
	// Declare builtin functions.
	functionByName["print"] = &Function{
		Name:       "print",
		ReturnType: UnitType,
		Args: []FunctionArg{
			FunctionArg{PointerType{StrType}, Register("%1")},
		},
	}
	functionByName["print_int"] = &Function{
		Name:       "print_int",
		ReturnType: UnitType,
		Args: []FunctionArg{
			FunctionArg{PointerType{Int8Type}, Register("%1")},
			FunctionArg{Int64Type, Register("%2")},
		},
	}
	functionByName["__unsafe_malloc"] = &Function{
		Name:       "__unsafe_malloc",
		ReturnType: PointerType{Int8Type},
		Args: []FunctionArg{
			FunctionArg{Int64Type, Register("%1")},
		},
	}
	constants := []*StrConst{}
	// Generate code for each function.
	for i, function := range functions {
		definition := functionDefinitions[i]
		gen := &generator{
			DefaultVisitor:      ast.DefaultVisitor{},
			typeInfo:            typeInfo,
			registerByNodeId:    make(map[ast.NodeId]Register),
			functions:           functionByName,
			symbolTable:         &symbolTable{symbols: make(map[ast.Ident]Register)},
			globalConstants:     &constants,
			declaredTypes:       declaredTypes,
			registerConstraints: RegisterConstraints{},
			loopScopes:          []loopScope{},
		}
		// Make function arguments visible.
		for _, arg := range definition.Decl.Args {
			gen.symbolTable.declare(arg.Name, gen.nextRegister())
		}
		block := gen.newBlock()
		gen.currentBlock = block
		walker := &ast.DefaultWalker{Visitor: gen}
		if err := walker.WalkNode(definition.Body); err != nil {
			return nil, err
		}
		if gen.currentBlock.Terminator != nil {
			return nil, errors.Errorf("expecting the last block to not have a terminator, but got: %s", block.Terminator)
		}
		gen.currentBlock.Terminator = &Return{}
		function.Entry = block
		function.RegisterConstraints = gen.registerConstraints
	}
	return &Module{Functions: functions, Constants: constants, DeclaredTypes: declaredTypes}, nil
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
