package ir

import (
	"fmt"

	"github.com/flunderpero/klar/bootstrap/ast"
	"github.com/flunderpero/klar/bootstrap/typed"
)

type IRType interface {
	String() string
	Size() int
}

type IRBasicType string

const (
	IRUnit  IRBasicType = "unit"
	IRBool  IRBasicType = "i1"
	IRInt8  IRBasicType = "i8"
	IRInt32 IRBasicType = "i32"
	IRInt64 IRBasicType = "i64"
)

func (t IRBasicType) String() string {
	return string(t)
}

func (t IRBasicType) Size() int {
	switch t {
	case IRUnit:
		return 0
	case IRBool:
		return 1
	case IRInt8:
		return 1
	case IRInt32:
		return 4
	case IRInt64:
		return 8
	default:
		panic(fmt.Sprintf("Unknown basic type: %s", t))
	}
}

type IRPointerType struct {
	ElementType IRType
}

func (t IRPointerType) String() string {
	return fmt.Sprintf("%s*", t.ElementType)
}

func (t IRPointerType) Size() int {
	return 8
}

type IRStructType struct {
	Name   string
	Fields []IRType
}

func (t IRStructType) String() string {
	fields := ""
	for _, field := range t.Fields {
		if len(fields) > 0 {
			fields += ", "
		}
		fields += field.String()
	}
	return fmt.Sprintf("struct %s {%s}", t.Name, fields)
}

func (t IRStructType) Size() int {
	size := 0
	for _, field := range t.Fields {
		size += field.Size()
	}
	return size
}

var IRStrType = &IRStructType{Name: "Str", Fields: []IRType{IRInt64, &IRPointerType{IRInt8}}}

type IRBlockId int

func (b IRBlockId) String() string {
	return fmt.Sprintf("block_%d", b)
}

type IRBlock struct {
	Id           IRBlockId
	Instructions []IRInstruction
	Terminator   IRTerminator
	Predecessors []*IRBlock
}

func (ir *IRBlock) append(instruction IRInstruction) {
	ir.Instructions = append(ir.Instructions, instruction)
}

func (ir *IRBlock) String() string {
	s := ""
	for _, inst := range ir.Instructions {
		s += fmt.Sprintf("\n    %s", inst)
	}
	s += fmt.Sprintf("\n    %s", ir.Terminator)
	return fmt.Sprintf("%s:%s", ir.Id, s)
}

type IRTerminator interface {
	String() string
	Targets() []*IRBlock
}

type IRJump struct {
	Target *IRBlock
}

func (ir *IRJump) String() string {
	return fmt.Sprintf("jmp %s", ir.Target.Id)
}

func (ir *IRJump) Targets() []*IRBlock {
	return []*IRBlock{ir.Target}
}

type IRCondBranch struct {
	Condition  IRRegister
	TrueBlock  *IRBlock
	FalseBlock *IRBlock
}

func (ir *IRCondBranch) String() string {
	return fmt.Sprintf("condbr i1 %s, %s, %s", ir.Condition, ir.TrueBlock.Id, ir.FalseBlock.Id)
}

func (ir *IRCondBranch) Targets() []*IRBlock {
	return []*IRBlock{ir.TrueBlock, ir.FalseBlock}
}

type IRReturn struct{}

func (ir *IRReturn) String() string {
	return "ret"
}

func (ir *IRReturn) Targets() []*IRBlock {
	return []*IRBlock{}
}

type IRFunctionArg struct {
	Type     IRType
	Register IRRegister
}

type IRFunction struct {
	Name       string
	ReturnType IRType
	Args       []IRFunctionArg
	Entry      *IRBlock
	Definition *ast.FunctionDefinition
}

func (t *IRFunction) String() string {
	args := ""
	for _, arg := range t.Args {
		if len(args) > 0 {
			args += ", "
		}
		args += fmt.Sprintf("%s %s", arg.Type, arg.Register)
	}
	return fmt.Sprintf("@declare %s %s(%s)", t.ReturnType, t.Name, args)
}

type IRModule struct {
	Functions []*IRFunction
	Constants []*IRStringConst
	Types     []*IRStructType
}

type IRRegister string

func (r IRRegister) String() string {
	return string(r)
}

func (r IRRegister) IsConstant() bool {
	return string(r)[0] == '_'
}

type IRInstruction interface {
	String() string
	Register() IRRegister
}

type IRStringConst struct {
	register IRRegister
	Value    string
}

func (i *IRStringConst) String() string {
	return fmt.Sprintf("%s = String %q", i.register, i.Value)
}

func (i *IRStringConst) Register() IRRegister {
	return i.register
}

type IRInt64Const struct {
	register IRRegister
	Value    int64
}

func (i *IRInt64Const) String() string {
	return fmt.Sprintf("%s = i64 %d", i.register, i.Value)
}

func (i *IRInt64Const) Register() IRRegister {
	return i.register
}

type IRInt32Const struct {
	register IRRegister
	Value    int64
}

func (i *IRInt32Const) String() string {
	return fmt.Sprintf("%s = i32 %d", i.register, i.Value)
}

func (i *IRInt32Const) Register() IRRegister {
	return i.register
}

type IRBoolConst struct {
	register IRRegister
	Value    int
}

func (i *IRBoolConst) String() string {
	return fmt.Sprintf("%s = i1 %d", i.register, i.Value)
}

func (i *IRBoolConst) Register() IRRegister {
	return i.register
}

type IRGetPtr struct {
	register   IRRegister
	Source     IRRegister
	Type       IRType
	FieldIndex int
}

func (i *IRGetPtr) String() string {
	return fmt.Sprintf("%s = getptr %s, %s, %d", i.register, i.Type, i.Source, i.FieldIndex)
}

func (i *IRGetPtr) Register() IRRegister {
	return i.register
}

type IRLoad struct {
	register  IRRegister
	Source    IRRegister
	FieldType IRType
}

func (i *IRLoad) Register() IRRegister {
	return i.register
}

func (i *IRLoad) String() string {
	return fmt.Sprintf("%s = load %s, %s", i.register, i.Source, i.FieldType)
}

type IRCall struct {
	register IRRegister
	Function *IRFunction
	Args     []IRRegister
}

func (inst *IRCall) String() string {
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

func (inst *IRCall) Register() IRRegister {
	return inst.register
}

type SymbolTable struct {
	symbols map[string]IRRegister
	parent  *SymbolTable
}

func (s *SymbolTable) lookup(name string) (IRRegister, bool) {
	reg, found := s.symbols[name]
	if !found && s.parent != nil {
		return s.parent.lookup(name)
	}
	return reg, found
}

type IRGenerator struct {
	ast.DefaultASTVisitor
	currentBlock     *IRBlock
	typeByNodeId     map[ast.NodeId]typed.Type
	registerByNodeId map[ast.NodeId]IRRegister
	symbolTable      *SymbolTable
	globalConstants  *[]*IRStringConst
	registerIndex    int
	blockIndex       int
	functions        map[string]*IRFunction
	declaredTypes    map[string]*IRStructType
}

func (g *IRGenerator) EnterScope() {
	g.symbolTable = &SymbolTable{symbols: make(map[string]IRRegister), parent: g.symbolTable}
}

func (g *IRGenerator) ExitScope() {
	g.symbolTable = g.symbolTable.parent
}

func (g *IRGenerator) SetSymbol(name string, reg IRRegister) {
	g.symbolTable.symbols[name] = reg
}

func (g *IRGenerator) GetSymbol(name string) (IRRegister, bool) {
	return g.symbolTable.lookup(name)
}

func (g *IRGenerator) NextRegister() IRRegister {
	g.registerIndex++
	return IRRegister(fmt.Sprintf("%%%d", g.registerIndex))
}

func (g *IRGenerator) Append(instruction IRInstruction, node ast.Node) {
	g.currentBlock.append(instruction)
	if node != nil {
		g.registerByNodeId[node.Id()] = instruction.Register()
	}
}

func (g *IRGenerator) LookupRegisterByNode(node ast.Node) IRRegister {
	reg, ok := g.registerByNodeId[node.Id()]
	if !ok {
		panic(fmt.Sprintf("No register found for node %s", node))
	}
	return reg
}

func (g *IRGenerator) VisitStringLiteralExpression(expr *ast.StringLiteralExpression) error {
	reg := IRRegister(fmt.Sprintf("_const_%d", len(*g.globalConstants)))
	*g.globalConstants = append(*g.globalConstants, &IRStringConst{register: reg, Value: expr.Value})
	g.Append(&IRGetPtr{
		register:   g.NextRegister(),
		Source:     reg,
		Type:       IRStrType,
		FieldIndex: 0,
	}, expr)
	return nil
}

func (g *IRGenerator) TypeOf(node ast.Node) typed.Type {
	ty, found := g.typeByNodeId[node.Id()]
	if !found {
		panic(fmt.Sprintf("Type of node %s should have been determined by the type-checker", node))
	}
	return ty
}

func (g *IRGenerator) NewBlock(predecessors ...*IRBlock) *IRBlock {
	g.blockIndex += 1
	block := &IRBlock{Id: IRBlockId(g.blockIndex)}
	block.Predecessors = append(block.Predecessors, predecessors...)
	return block
}

func (g *IRGenerator) VisitBoolLiteralExpression(expr *ast.BoolLiteralExpression) error {
	value := 0
	if expr.Value {
		value = 1
	}
	g.Append(&IRBoolConst{
		register: g.NextRegister(),
		Value:    value,
	}, expr)
	return nil
}

func (g *IRGenerator) VisitIdentExpression(expr *ast.IdentExpression) error {
	if _, found := g.functions[expr.Name]; found {
		return nil
	}
	reg, found := g.GetSymbol(expr.Name)
	if !found {
		return fmt.Errorf("unknown symbol: %s", expr.Name)
	}
	g.registerByNodeId[expr.Id()] = reg
	return nil
}

func (g *IRGenerator) VisitCallExpression(expr *ast.CallExpression, w ast.ASTWalker) error {
	if err := w.WalkCallExpression(expr); err != nil {
		return err
	}
	funcType := g.TypeOf(expr.Callee).(*typed.FunctionType)
	function, ok := g.functions[funcType.Name]
	if !ok {
		panic(fmt.Sprintf("Unknown function: %s", funcType.Name))
	}
	if function.Name == "print" {
		arg0reg := g.LookupRegisterByNode(expr.Args[0])
		stdOutReg := g.NextRegister()
		strPtrReg := g.NextRegister()
		strPtrLoadReg := g.NextRegister()
		strLenReg := g.NextRegister()
		g.Append(&IRInt32Const{
			register: stdOutReg,
			Value:    1,
		}, nil)
		g.Append(&IRGetPtr{
			register:   strPtrReg,
			Source:     arg0reg,
			FieldIndex: 1,
			Type:       IRStrType,
		}, nil)
		g.Append(&IRLoad{
			register:  strPtrLoadReg,
			Source:    strPtrReg,
			FieldType: &IRPointerType{IRInt8},
		}, nil)
		g.Append(&IRLoad{
			register:  strLenReg,
			Source:    arg0reg,
			FieldType: IRInt64,
		}, nil)
		g.Append(&IRCall{
			register: g.NextRegister(),
			Function: function,
			Args:     []IRRegister{stdOutReg, strPtrLoadReg, strLenReg},
		}, nil)
	} else {
		args := []IRRegister{}
		for _, arg := range expr.Args {
			args = append(args, g.LookupRegisterByNode(arg))
		}
		g.Append(&IRCall{
			register: g.NextRegister(),
			Function: function,
			Args:     args,
		}, nil)
	}
	return nil
}

func (g *IRGenerator) VisitIfExpression(expr *ast.IfExpression, w ast.ASTWalker) error {
	condBlock := g.NewBlock(g.currentBlock)
	g.currentBlock.Terminator = &IRJump{Target: condBlock}
	g.currentBlock = condBlock
	if err := w.WalkNode(expr.Condition); err != nil {
		return err
	}
	trueBlock := g.NewBlock(condBlock)
	mergeBlock := g.NewBlock(condBlock, trueBlock)
	trueBlock.Terminator = &IRJump{Target: mergeBlock}
	condBlock.Terminator = &IRCondBranch{
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

func GenerateIR(module *ast.Module, typeMap map[ast.NodeId]typed.Type) (*IRModule, error) {
	functionDefinitions := []*ast.FunctionDefinition{}
	for _, node := range module.Nodes {
		switch node := node.(type) {
		case *ast.FunctionDefinition:
			functionDefinitions = append(functionDefinitions, node)
		default:
			return nil, fmt.Errorf("cannot generate IR for node type: %T", node)
		}
	}
	functions := []*IRFunction{}
	declaredTypes := make(map[string]*IRStructType)
	// Declare built-in types.
	declaredTypes[IRStrType.Name] = IRStrType
	// First forward declare all functions.
	for _, fd := range functionDefinitions {
		args := []IRFunctionArg{}
		for i, arg := range fd.Args {
			argType, found := declaredTypes[arg.Type]
			if !found {
				return nil, fmt.Errorf("type %s not found for argument %s", arg.Type, arg.Name)
			}
			irArg := IRFunctionArg{
				Type:     argType,
				Register: IRRegister(fmt.Sprintf("%%%d", (i + 1))),
			}
			args = append(args, irArg)
		}
		f := IRFunction{
			Name:       fd.Name,
			ReturnType: IRUnit,
			Args:       args,
			Definition: fd,
		}
		functions = append(functions, &f)
	}
	functionByName := make(map[string]*IRFunction)
	for _, f := range functions {
		functionByName[f.Name] = f
	}
	// Declare builtin functions.
	functionByName["print"] = &IRFunction{
		Name:       "print",
		ReturnType: IRInt64,
		Args: []IRFunctionArg{
			IRFunctionArg{IRInt32, IRRegister("%1")},
			IRFunctionArg{IRPointerType{IRInt8}, IRRegister("%2")},
			IRFunctionArg{IRInt64, IRRegister("%3")},
		},
	}
	constants := []*IRStringConst{}
	// Generate code for each function.
	for _, function := range functions {
		gen := &IRGenerator{
			DefaultASTVisitor: ast.DefaultASTVisitor{},
			typeByNodeId:      typeMap,
			registerByNodeId:  make(map[ast.NodeId]IRRegister),
			functions:         functionByName,
			symbolTable:       &SymbolTable{symbols: make(map[string]IRRegister)},
			globalConstants:   &constants,
			declaredTypes:     declaredTypes,
		}
		// Make function arguments visible.
		for _, arg := range function.Definition.Args {
			gen.SetSymbol(arg.Name, gen.NextRegister())
		}
		block := gen.NewBlock()
		gen.currentBlock = block
		walker := &ast.DefaultASTWalker{Visitor: gen}
		if err := walker.WalkNode(function.Definition.Body); err != nil {
			return nil, err
		}
		if gen.currentBlock.Terminator != nil {
			return nil, fmt.Errorf("expecting the last block to not have a terminator, but got: %s", block.Terminator)
		}
		gen.currentBlock.Terminator = &IRReturn{}
		function.Entry = block
	}
	types := []*IRStructType{}
	for _, ty := range declaredTypes {
		types = append(types, ty)
	}
	return &IRModule{Functions: functions, Constants: constants, Types: types}, nil
}

// Walk the given block and call `visitor` for each block we discover in the graph
// of reachable blocks. It is guaranteed that each unique block is only visited once.
func WalkBlock(block *IRBlock, visit func(block *IRBlock) error) error {
	visited := make(map[IRBlockId]bool)
	blocks := []*IRBlock{block}
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
