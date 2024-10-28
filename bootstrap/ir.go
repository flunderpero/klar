package main

import (
	"fmt"
)

type BasicType int

const (
	Void BasicType = iota
	Int8
	Int32
	Int64
	String
	Ptr
)

type IRType interface {
	String() string
}

type IRBasicType struct {
	ty BasicType
}

func (t *IRBasicType) String() string {
	switch t.ty {
	case Void:
		return "void"
	case Int8:
		return "i8"
	case Int32:
		return "i32"
	case Int64:
		return "i64"
	case String:
		return "string"
	case Ptr:
		return "ptr"
	default:
		panic(fmt.Sprintf("Unknown basic type: %d", t.ty))
	}
}

type IRFunctionType struct {
	Name       string
	ReturnType IRType
	ArgTypes   []IRType
}

func (t *IRFunctionType) String() string {
	return fmt.Sprintf("%s(%v)%s", t.Name, t.ArgTypes, t.ReturnType)
}

type IRRegister int

func (r IRRegister) String() string {
	return fmt.Sprintf("%%%d", r)
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

type IRGetPtr struct {
	register IRRegister
	Source   IRRegister
}

func (i *IRGetPtr) String() string {
	return fmt.Sprintf("%s = getptr %s", i.register, i.Source)
}

func (i *IRGetPtr) Register() IRRegister {
	return i.register
}

type IRCall struct {
	register IRRegister
	Function IRFunctionType
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
	EmptyASTVisitor
	walker                DepthFirstASTWalker
	instructions          []IRInstruction
	typeByNodeId          map[NodeId]Type
	registerByNodeId      map[NodeId]IRRegister
	instructionByRegister map[IRRegister]int
	functions             map[string]IRFunctionType
	registerIndex         int
}

func (g *IRGenerator) NextRegister() IRRegister {
	g.registerIndex++
	return IRRegister(g.registerIndex)
}

func (g *IRGenerator) Append(instruction IRInstruction, node Node) {
	g.instructionByRegister[instruction.Register()] = len(g.instructions)
	g.instructions = append(g.instructions, instruction)
	if node != nil {
		g.registerByNodeId[node.Id()] = instruction.Register()
	}
}

func (g *IRGenerator) LookupInstruction(register IRRegister) IRInstruction {
	index, ok := g.instructionByRegister[register]
	if !ok {
		panic(fmt.Sprintf("No instruction found for register %s", register))
	}
	if index < 0 || index >= len(g.instructions) {
		panic(fmt.Sprintf("Instruction index %d out of bounds", index))
	}
	return g.instructions[index]
}

func (g *IRGenerator) LookupInstructionByNode(node Node) IRInstruction {
	reg, ok := g.registerByNodeId[node.Id()]
	if !ok {
		panic(fmt.Sprintf("No register found for node %s", node))
	}
	return g.LookupInstruction(reg)
}

func (g *IRGenerator) VisitStringLiteralExpression(expr *StringLiteralExpression) error {
	g.Append(&IRStringConst{
		register: g.NextRegister(),
		Value:    expr.Value,
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

func (g *IRGenerator) VisitCallExpression(expr *CallExpression) error {
	funcType := g.TypeOf(expr.Callee).(*FunctionType)
	if funcType.Name != "print" {
		return fmt.Errorf("unknown function: %s", funcType.Name)
	}
	function, ok := g.functions[funcType.Name]
	if !ok {
		panic(fmt.Sprintf("Unknown function: %s", funcType.Name))
	}
	arg0 := g.LookupInstructionByNode(expr.Args[0]).(*IRStringConst)
	stdOutReg := g.NextRegister()
	strPtrReg := g.NextRegister()
	strLenReg := g.NextRegister()
	g.Append(&IRInt32Const{
		register: stdOutReg,
		Value:    1,
	}, nil)
	g.Append(&IRGetPtr{
		register: strPtrReg,
		Source:   arg0.Register(),
	}, nil)
	g.Append(&IRInt64Const{
		register: strLenReg,
		Value:    int64(len(arg0.Value)),
	}, nil)
	g.Append(&IRCall{
		register: g.NextRegister(),
		Function: function,
		Args:     []IRRegister{stdOutReg, strPtrReg, strLenReg},
	}, nil)
	return nil
}

func GenerateIR(node Node, typeMap map[NodeId]Type) ([]IRInstruction, error) {
	gen := &IRGenerator{
		walker:                DepthFirstASTWalker{},
		instructions:          []IRInstruction{},
		typeByNodeId:          typeMap,
		registerByNodeId:      make(map[NodeId]IRRegister),
		instructionByRegister: make(map[IRRegister]int),
		functions:             make(map[string]IRFunctionType),
	}
	// Declare builtin functions.
	gen.functions["print"] = IRFunctionType{
		Name:       "print",
		ReturnType: &IRBasicType{Int64},
		ArgTypes:   []IRType{&IRBasicType{Int32}, &IRBasicType{Ptr}, &IRBasicType{Int64}},
	}
	gen.walker.Visitor = gen
	if err := gen.walker.WalkNode(node); err != nil {
		return nil, err
	}
	return gen.instructions, nil
}
