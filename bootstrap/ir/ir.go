package ir

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
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
	Name   string
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
	return fmt.Sprintf("%s:%s", ir.Id, s)
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
	Name       string
	ReturnType Type
	Args       []FunctionArg
	Entry      *Block
	Definition *ast.FunctionDefinition
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
	Functions []*Function
	Constants []*StrConst
	Types     []*StructType
}

type Register string

func (r Register) String() string {
	return string(r)
}

func (r Register) IsConstant() bool {
	return string(r)[0] == '_'
}

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
	Type       Type
	FieldIndex int
}

func (i *GetPointer) String() string {
	return fmt.Sprintf("%s = getptr %s, %s, %d", i.register, i.Type, i.Source, i.FieldIndex)
}

func (i *GetPointer) Register() Register {
	return i.register
}

type Load struct {
	register  Register
	Source    Register
	FieldType Type
}

func (i *Load) Register() Register {
	return i.register
}

func (i *Load) String() string {
	return fmt.Sprintf("%s = load %s, %s", i.register, i.Source, i.FieldType)
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

type symbolTable struct {
	symbols map[string]Register
	parent  *symbolTable
}

func (s *symbolTable) lookup(name string) (Register, bool) {
	reg, found := s.symbols[name]
	if !found && s.parent != nil {
		return s.parent.lookup(name)
	}
	return reg, found
}

type generator struct {
	ast.DefaultASTVisitor
	currentBlock     *Block
	typeByNodeId     map[ast.NodeId]typed.Type
	registerByNodeId map[ast.NodeId]Register
	symbolTable      *symbolTable
	globalConstants  *[]*StrConst
	registerIndex    int
	blockIndex       int
	functions        map[string]*Function
	declaredTypes    map[string]*StructType
}

func (g *generator) enterScope() {
	g.symbolTable = &symbolTable{symbols: make(map[string]Register), parent: g.symbolTable}
}

func (g *generator) exitScope() {
	g.symbolTable = g.symbolTable.parent
}

func (g *generator) setSymbol(name string, reg Register) {
	g.symbolTable.symbols[name] = reg
}

func (g *generator) getSymbol(name string) (Register, bool) {
	return g.symbolTable.lookup(name)
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

func (g *generator) typeOf(node ast.Node) typed.Type {
	ty, found := g.typeByNodeId[node.Id()]
	if !found {
		panic(fmt.Sprintf("Type of node %s should have been determined by the type-checker", node))
	}
	return ty
}

func (g *generator) newBlock(predecessors ...*Block) *Block {
	g.blockIndex += 1
	block := &Block{Id: BlockId(g.blockIndex)}
	block.Predecessors = append(block.Predecessors, predecessors...)
	return block
}

func (g *generator) VisitStringLiteralExpression(expr *ast.StringLiteralExpression) error {
	reg := Register(fmt.Sprintf("_const_%d", len(*g.globalConstants)))
	*g.globalConstants = append(*g.globalConstants, &StrConst{register: reg, Value: expr.Value})
	g.append(&GetPointer{
		register:   g.nextRegister(),
		Source:     reg,
		Type:       StrType,
		FieldIndex: 0,
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

func (g *generator) VisitIdentExpression(expr *ast.IdentExpression) error {
	if _, found := g.functions[expr.Name]; found {
		return nil
	}
	reg, found := g.getSymbol(expr.Name)
	if !found {
		return fmt.Errorf("unknown symbol: %s", expr.Name)
	}
	g.registerByNodeId[expr.Id()] = reg
	return nil
}

func (g *generator) VisitCallExpression(expr *ast.CallExpression, w ast.ASTWalker) error {
	if err := w.WalkCallExpression(expr); err != nil {
		return err
	}
	funcType := g.typeOf(expr.Callee).(*typed.FunctionType)
	function, ok := g.functions[funcType.Name]
	if !ok {
		panic(fmt.Sprintf("Unknown function: %s", funcType.Name))
	}
	if function.Name == "print" {
		arg0reg := g.lookupRegisterByNode(expr.Args[0])
		stdOutReg := g.nextRegister()
		strPtrReg := g.nextRegister()
		strPtrLoadReg := g.nextRegister()
		strLenReg := g.nextRegister()
		g.append(&Int32Const{
			register: stdOutReg,
			Value:    1,
		}, nil)
		g.append(&GetPointer{
			register:   strPtrReg,
			Source:     arg0reg,
			FieldIndex: 1,
			Type:       StrType,
		}, nil)
		g.append(&Load{
			register:  strPtrLoadReg,
			Source:    strPtrReg,
			FieldType: &PointerType{Int8Type},
		}, nil)
		g.append(&Load{
			register:  strLenReg,
			Source:    arg0reg,
			FieldType: Int64Type,
		}, nil)
		g.append(&Call{
			register: g.nextRegister(),
			Function: function,
			Args:     []Register{stdOutReg, strPtrLoadReg, strLenReg},
		}, nil)
	} else {
		args := []Register{}
		for _, arg := range expr.Args {
			args = append(args, g.lookupRegisterByNode(arg))
		}
		g.append(&Call{
			register: g.nextRegister(),
			Function: function,
			Args:     args,
		}, nil)
	}
	return nil
}

func (g *generator) VisitIfExpression(expr *ast.IfExpression, w ast.ASTWalker) error {
	condBlock := g.newBlock(g.currentBlock)
	g.currentBlock.Terminator = &Jump{Target: condBlock}
	g.currentBlock = condBlock
	if err := w.WalkNode(expr.Condition); err != nil {
		return err
	}
	trueBlock := g.newBlock(condBlock)
	mergeBlock := g.newBlock(condBlock, trueBlock)
	trueBlock.Terminator = &Jump{Target: mergeBlock}
	condBlock.Terminator = &CondBranch{
		Condition:  g.registerByNodeId[expr.Condition.Id()],
		TrueBlock:  trueBlock,
		FalseBlock: mergeBlock,
	}
	g.currentBlock = trueBlock
	if err := w.WalkBlockExpression(expr.TrueBody); err != nil {
		return err
	}
	g.currentBlock = mergeBlock
	return nil
}

func GenerateIR(module *ast.Module, typeMap map[ast.NodeId]typed.Type) (*Module, error) {
	functionDefinitions := []*ast.FunctionDefinition{}
	for _, node := range module.Nodes {
		switch node := node.(type) {
		case *ast.FunctionDefinition:
			functionDefinitions = append(functionDefinitions, node)
		default:
			return nil, fmt.Errorf("cannot generate IR for node type: %T", node)
		}
	}
	functions := []*Function{}
	declaredTypes := make(map[string]*StructType)
	// Declare built-in types.
	declaredTypes[StrType.Name] = StrType
	// First forward declare all functions.
	for _, fd := range functionDefinitions {
		args := []FunctionArg{}
		for i, arg := range fd.Args {
			argType, found := declaredTypes[arg.Type]
			if !found {
				return nil, fmt.Errorf("type %s not found for argument %s", arg.Type, arg.Name)
			}
			irArg := FunctionArg{
				Type:     argType,
				Register: Register(fmt.Sprintf("%%%d", (i + 1))),
			}
			args = append(args, irArg)
		}
		f := Function{
			Name:       fd.Name,
			ReturnType: UnitType,
			Args:       args,
			Definition: fd,
		}
		functions = append(functions, &f)
	}
	functionByName := make(map[string]*Function)
	for _, f := range functions {
		functionByName[f.Name] = f
	}
	// Declare builtin functions.
	functionByName["print"] = &Function{
		Name:       "print",
		ReturnType: Int64Type,
		Args: []FunctionArg{
			FunctionArg{Int32Type, Register("%1")},
			FunctionArg{PointerType{Int8Type}, Register("%2")},
			FunctionArg{Int64Type, Register("%3")},
		},
	}
	constants := []*StrConst{}
	// Generate code for each function.
	for _, function := range functions {
		gen := &generator{
			DefaultASTVisitor: ast.DefaultASTVisitor{},
			typeByNodeId:      typeMap,
			registerByNodeId:  make(map[ast.NodeId]Register),
			functions:         functionByName,
			symbolTable:       &symbolTable{symbols: make(map[string]Register)},
			globalConstants:   &constants,
			declaredTypes:     declaredTypes,
		}
		// Make function arguments visible.
		for _, arg := range function.Definition.Args {
			gen.setSymbol(arg.Name, gen.nextRegister())
		}
		block := gen.newBlock()
		gen.currentBlock = block
		walker := &ast.DefaultASTWalker{Visitor: gen}
		if err := walker.WalkNode(function.Definition.Body); err != nil {
			return nil, err
		}
		if gen.currentBlock.Terminator != nil {
			return nil, fmt.Errorf("expecting the last block to not have a terminator, but got: %s", block.Terminator)
		}
		gen.currentBlock.Terminator = &Return{}
		function.Entry = block
	}
	types := []*StructType{}
	for _, ty := range declaredTypes {
		types = append(types, ty)
	}
	return &Module{Functions: functions, Constants: constants, Types: types}, nil
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
		blocks = append(blocks, block.Terminator.Targets()...)
	}
	return nil
}
