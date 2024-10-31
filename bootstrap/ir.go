package main

import (
	"fmt"
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

type IRFunction struct {
	Name       string
	ReturnType IRType
	ArgTypes   []IRType
	Entry      *IRBlock
	Definition *FunctionDefinition
}

func (t *IRFunction) String() string {
	args := ""
	for _, argType := range t.ArgTypes {
		if len(args) > 0 {
			args += ", "
		}
		args += argType.String()
	}
	return fmt.Sprintf("@declare %s %s (%v)", t.ReturnType, t.Name, args)
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
		ty := inst.Function.ArgTypes[i]
		if len(args) > 0 {
			args += ", "
		}
		args += fmt.Sprintf("%s %s", ty, reg)
	}
	return fmt.Sprintf("%s = call %s %s (%s)", inst.register, inst.Function.ReturnType, inst.Function.Name, args)
}

func (inst *IRCall) Register() IRRegister {
	return inst.register
}

type IRGenerator struct {
	DefaultASTVisitor
	currentBlock          *IRBlock
	typeByNodeId          map[NodeId]Type
	registerByNodeId      map[NodeId]IRRegister
	instructionByRegister map[IRRegister]IRInstruction
	functions             map[string]*IRFunction
	globalConstants       *[]*IRStringConst
	registerIndex         int
	blockIndex            int
	declaredTypes         map[string]*IRStructType
}

func (g *IRGenerator) NextRegister() IRRegister {
	g.registerIndex++
	return IRRegister(fmt.Sprintf("%%%d", g.registerIndex))
}

func (g *IRGenerator) Append(instruction IRInstruction, node Node) {
	g.instructionByRegister[instruction.Register()] = instruction
	g.currentBlock.append(instruction)
	if node != nil {
		g.registerByNodeId[node.Id()] = instruction.Register()
	}
}

func (g *IRGenerator) LookupInstruction(register IRRegister) IRInstruction {
	instruction, ok := g.instructionByRegister[register]
	if !ok {
		panic(fmt.Sprintf("No instruction found for register %s", register))
	}
	return instruction
}

func (g *IRGenerator) LookupInstructionByNode(node Node) IRInstruction {
	reg, ok := g.registerByNodeId[node.Id()]
	if !ok {
		panic(fmt.Sprintf("No register found for node %s", node))
	}
	return g.LookupInstruction(reg)
}

func (g *IRGenerator) VisitStringLiteralExpression(expr *StringLiteralExpression) error {
	reg := IRRegister(fmt.Sprintf("_const_%d", len(*g.globalConstants)))
	*g.globalConstants = append(*g.globalConstants, &IRStringConst{register: reg, Value: expr.Value})
	g.Append(&IRGetPtr{
		register: g.NextRegister(),
		Source:   reg,
	}, expr)
	return nil
}

func (g *IRGenerator) TypeOf(node Node) Type {
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

func (g *IRGenerator) VisitBoolLiteralExpression(expr *BoolLiteralExpression) error {
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

func (g *IRGenerator) VisitCallExpression(expr *CallExpression, w ASTWalker) error {
	if err := w.WalkCallExpression(expr); err != nil {
		return err
	}
	funcType := g.TypeOf(expr.Callee).(*FunctionType)
	function, ok := g.functions[funcType.Name]
	if !ok {
		panic(fmt.Sprintf("Unknown function: %s", funcType.Name))
	}
	if function.Name == "print" {
		arg0 := g.LookupInstructionByNode(expr.Args[0]).(*IRGetPtr)
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
			Source:     arg0.Register(),
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
			Source:    arg0.Register(),
			FieldType: IRInt64,
		}, nil)
		g.Append(&IRCall{
			register: g.NextRegister(),
			Function: function,
			Args:     []IRRegister{stdOutReg, strPtrLoadReg, strLenReg},
		}, nil)
	} else {
		g.Append(&IRCall{
			register: g.NextRegister(),
			Function: function,
			Args:     []IRRegister{},
		}, nil)
	}
	return nil
}

func (g *IRGenerator) VisitIfExpression(expr *IfExpression, w ASTWalker) error {
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

func GenerateIR(module *Module, typeMap map[NodeId]Type) (*IRModule, error) {
	functionDefinitions := []*FunctionDefinition{}
	for _, node := range module.Nodes {
		switch node := node.(type) {
		case *FunctionDefinition:
			functionDefinitions = append(functionDefinitions, node)
		default:
			return nil, fmt.Errorf("cannot generate IR for node type: %T", node)
		}
	}
	functions := []*IRFunction{}
	// First forward declare all functions.
	for _, fd := range functionDefinitions {
		f := IRFunction{
			Name:       fd.Name,
			ReturnType: IRUnit,
			ArgTypes:   []IRType{},
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
		ArgTypes:   []IRType{IRInt32, &IRPointerType{IRInt8}, IRInt64},
	}
	constants := []*IRStringConst{}
	declaredTypes := make(map[string]*IRStructType)
	// Declare built-in types.
	declaredTypes[IRStrType.Name] = IRStrType
	// Generate code for each function.
	for _, function := range functions {
		gen := &IRGenerator{
			DefaultASTVisitor:     DefaultASTVisitor{},
			typeByNodeId:          typeMap,
			registerByNodeId:      make(map[NodeId]IRRegister),
			instructionByRegister: make(map[IRRegister]IRInstruction),
			functions:             functionByName,
			globalConstants:       &constants,
			declaredTypes:         declaredTypes,
		}
		block := gen.NewBlock()
		gen.currentBlock = block
		walker := &DefaultASTWalker{Visitor: gen}
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
