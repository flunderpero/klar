package ast

import (
	"fmt"
	"slices"
	"strconv"
	"strings"

	"github.com/flunderpero/klar/bootstrap/token"
)

type NodeId int

type Node interface {
	String() string
	Id() NodeId
}

type node struct {
	id NodeId
}

func (n *node) Id() NodeId {
	return n.id
}

type AnyIdent interface {
	IdentString()
}

type TypeIdent string

func (ident TypeIdent) IdentString() string {
	return string(ident)
}

type Ident string

func (ident Ident) IdentString() string {
	return string(ident)
}

type Expression interface {
	String() string
	Id() NodeId
}

type AnyIdentExpression interface {
	String() string
	IdentString() string
	Id() NodeId
}

type TypeIdentExpression struct {
	node
	Ident TypeIdent
}

func (expr *TypeIdentExpression) String() string {
	return fmt.Sprintf("TypeIdentExpression(%s)", expr.Ident)
}

func (expr *TypeIdentExpression) IdentString() string {
	return expr.Ident.IdentString()
}

type IdentExpression struct {
	node
	Ident Ident
}

func NewIdentExpression(ident Ident, id NodeId) *IdentExpression {
	return &IdentExpression{node: node{id: id}, Ident: ident}
}

func (expr *IdentExpression) String() string {
	return fmt.Sprintf("IdentExpression(%s)", expr.Ident)
}

func (expr *IdentExpression) IdentString() string {
	return expr.Ident.IdentString()
}

type StringLiteralExpression struct {
	node
	Value string
}

func (expr *StringLiteralExpression) String() string {
	return fmt.Sprintf("StringLiteralExpression(%q)", expr.Value)
}

type IntLiteralExpression struct {
	node
	Value int64
}

func (expr *IntLiteralExpression) String() string {
	return fmt.Sprintf("IntLiteralExpression(%d)", expr.Value)
}

type BoolLiteralExpression struct {
	node
	Value bool
}

func (expr *BoolLiteralExpression) String() string {
	return fmt.Sprintf("BoolLiteralExpression(%s)", strconv.FormatBool(expr.Value))
}

type MemberExpression struct {
	node
	Target Expression
	Field  Ident
}

func (expr *MemberExpression) String() string {
	return fmt.Sprintf("MemberExpression(%s, %s)", expr.Target, expr.Field)
}

type BinaryOperator string

const (
	OpAdd      BinaryOperator = "+"
	OpEquality BinaryOperator = "=="
)

type BinaryExpression struct {
	node
	Lhs Expression
	Rhs Expression
	Op  BinaryOperator
}

func (expr *BinaryExpression) String() string {
	return fmt.Sprintf("BinaryExpression(%s, %s, %s)", expr.Op, expr.Lhs, expr.Rhs)
}

type StructInitField struct {
	Name  Ident
	Value Expression
}

func (f *StructInitField) String() string {
	return fmt.Sprintf("StructInitField(%s, %s)", f.Name, f.Value)
}

type StructInitExpression struct {
	node
	TypeIdent TypeIdent
	Fields    []StructInitField
}

func (s *StructInitExpression) String() string {
	fields := ""
	for _, field := range s.Fields {
		fields += "\n    "
		fields += strings.ReplaceAll(field.String(), "\n", "\n    ")
	}
	return fmt.Sprintf("StructInitExpression(\n    %s%s\n)", s.TypeIdent, fields)
}

type CallExpression struct {
	node
	Callee Expression
	Args   []Expression
}

func (expr *CallExpression) String() string {
	return fmt.Sprintf("CallExpression(%s, %v)", expr.Callee, expr.Args)
}

type BlockExpression struct {
	node
	Nodes []Node
}

func (expr *BlockExpression) String() string {
	s := ""
	for _, node := range expr.Nodes {
		nodeStr := "\n    " + strings.ReplaceAll(node.String(), "\n", "\n    ")
		s += nodeStr
	}
	return fmt.Sprintf("BlockExpression{%s\n}", s)
}

type IfExpression struct {
	node
	Condition Expression
	TrueBody  *BlockExpression
	FalseBody *BlockExpression
}

func (expr *IfExpression) String() string {
	condition := strings.ReplaceAll(expr.Condition.String(), "\n", "\n    ")
	trueBody := strings.ReplaceAll(expr.TrueBody.String(), "\n", "\n    ")
	if expr.FalseBody != nil {
		falseBody := strings.ReplaceAll(expr.FalseBody.String(), "\n", "\n    ")
		return fmt.Sprintf("IfExpression(\n    %s\n    %s\n    %s\n)", condition, trueBody, falseBody)
	} else {
		return fmt.Sprintf("IfExpression(\n    %s\n    %s\n)", condition, trueBody)
	}

}

type LoopStatement struct {
	node
	Body *BlockExpression
}

func (l *LoopStatement) String() string {
	return fmt.Sprintf("LoopStatement(%s)", l.Body)
}

type BreakStatement struct {
	node
}

func (b *BreakStatement) String() string {
	return "BreakStatement()"
}

type ContinueStatement struct {
	node
}

func (b *ContinueStatement) String() string {
	return "ContinueStatement()"
}

type AssignmentStatement struct {
	node
	Variable *IdentExpression
	// This is optional but we cannot express this in Go.
	Field *Ident
	Rhs   Expression
}

func (a *AssignmentStatement) String() string {
	if a.IsAssignToMember() {
		return fmt.Sprintf("AssignmentStatement(%s, %s, %s)", a.Variable, *a.Field, a.Rhs)
	}
	return fmt.Sprintf("AssignmentStatement(%s, %s)", a.Variable, a.Rhs)
}

func (a *AssignmentStatement) IsAssignToMember() bool {
	return a.Field != nil
}

type Module struct {
	node
	Name  Ident
	Nodes []Node
}

func (m *Module) String() string {
	nodes := ""
	for _, node := range m.Nodes {
		nodes += "\n    "
		nodes += strings.ReplaceAll(node.String(), "\n", "\n    ")
	}
	return fmt.Sprintf("Module(\n    %s%s\n)", m.Name, nodes)
}

type StructTypeField struct {
	Name Ident
	Type TypeIdent
}

func (f *StructTypeField) String() string {
	return fmt.Sprintf("StructTypeField(%s, %s)", f.Name, f.Type)
}

type StructTypeDeclaration struct {
	node
	Name   TypeIdent
	Fields []StructTypeField
}

func (st *StructTypeDeclaration) String() string {
	fields := ""
	for _, field := range st.Fields {
		fields += "\n    "
		fields += strings.ReplaceAll(field.String(), "\n", "\n    ")
	}
	return fmt.Sprintf("StructTypeDeclaration(\n    %s%s\n)", st.Name, fields)
}

func (st *StructTypeDeclaration) FindField(name Ident) (*StructTypeField, error) {
	index := slices.IndexFunc(st.Fields, func(field StructTypeField) bool { return field.Name == name })
	if index < 0 {
		return nil, fmt.Errorf("field %q not found in struct %q", name, st)
	}
	return &st.Fields[index], nil
}

type FunctionArg struct {
	Name Ident
	Type TypeIdent
}

func (f *FunctionArg) String() string {
	return fmt.Sprintf("FunctionArg(%s, %s)", f.Name, f.Type)
}

type FunctionDeclaration struct {
	node
	Name       Ident
	Args       []FunctionArg
	ReturnType TypeIdent
}

func (f *FunctionDeclaration) String() string {
	args := ""
	for _, arg := range f.Args {
		if args != "" {
			args += ", "
		}
		args += arg.String()
	}
	return fmt.Sprintf("FunctionDeclaration(%s(%s) %s)", f.Name, args, f.ReturnType)
}

type FunctionDefinition struct {
	node
	Decl *FunctionDeclaration
	Body *BlockExpression
}

func (f *FunctionDefinition) String() string {
	body := strings.ReplaceAll(f.Body.String(), "\n", "\n    ")
	return fmt.Sprintf("FunctionDefinition(\n    %s\n    %s\n)", f.Decl, body)
}

type ImplDefinition struct {
	node
	Target  TypeIdent
	Methods []*FunctionDefinition
	Trait   TypeIdent // optional
}

func (impl *ImplDefinition) ImplementsTrait() bool {
	return impl.Trait != ""
}

func (impl *ImplDefinition) String() string {
	functions := ""
	for _, function := range impl.Methods {
		functions += "\n    "
		functions += strings.ReplaceAll(function.String(), "\n", "\n    ")
	}
	target := string(impl.Target)
	if impl.ImplementsTrait() {
		target = fmt.Sprintf("%s for %s", impl.Trait, target)
	}
	return fmt.Sprintf("ImplDefinition(\n    %s    %s\n)", target, functions)
}

type TraitDeclaration struct {
	node
	Name        TypeIdent
	MethodDecls []*FunctionDeclaration
}

func (trait *TraitDeclaration) String() string {
	methods := ""
	for _, decl := range trait.MethodDecls {
		methods += "\n    "
		methods += strings.ReplaceAll(decl.String(), "\n", "\n    ")
	}
	return fmt.Sprintf("TraitDeclaration(\n    %s    %s\n)", trait.Name, methods)
}

type VariableDefinition struct {
	node
	Name    Ident
	Mutable bool
	Value   Expression
}

func (v *VariableDefinition) String() string {
	value := strings.ReplaceAll(v.Value.String(), "\n", "\n    ")
	return fmt.Sprintf("VariableDefinition(\n    %s, \n    mutable=%t, \n    %s\n)", v.Name, v.Mutable, value)
}

type Parser struct {
	tokens []token.Token
	index  int
	nodeId NodeId
}

func (p *Parser) newNode() node {
	p.nodeId = p.nodeId + 1
	return node{p.nodeId}
}

func (p *Parser) consume(kind token.TokenKind) (token.Token, error) {
	t := p.tokens[p.index]
	if t.Kind != kind {
		return token.Token{}, fmt.Errorf("Expected token kind %s, got %s", kind, t.Kind)
	}
	p.index = p.index + 1
	return t, nil
}

func (p *Parser) consumeAny() token.Token {
	p.index = p.index + 1
	return p.tokens[p.index-1]
}

func (p *Parser) peek() token.Token {
	return p.tokens[p.index]
}

func (p *Parser) parseCallExpression(callee Expression) (*CallExpression, error) {
	if _, err := p.consume(token.LParen); err != nil {
		return nil, err
	}
	args := []Expression{}
	done := false
	for p.index < len(p.tokens) && !done {
		t := p.peek()
		switch t.Kind {
		case token.RParen:
			p.consumeAny()
			done = true
		case token.Comma:
			p.consumeAny()
		default:
			arg, err := p.parseExpression()
			if err != nil {
				return nil, fmt.Errorf("failed to parse call argument: %v", err)
			}
			args = append(args, arg)
		}
	}
	return &CallExpression{node: p.newNode(), Callee: callee, Args: args}, nil
}

func (p *Parser) parseBlockExpression() (*BlockExpression, error) {
	if _, err := p.consume(token.LCurly); err != nil {
		return nil, err
	}
	var nodes []Node
	for p.index < len(p.tokens) {
		t := p.peek()
		if t.Kind == token.RCurly {
			p.consumeAny()
			break
		}
		node, err := p.ParseNode()
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, node)
	}
	return &BlockExpression{node: p.newNode(), Nodes: nodes}, nil
}

func (p *Parser) parseIfExpression() (*IfExpression, error) {
	if _, err := p.consume(token.If); err != nil {
		return nil, err
	}
	condition, err := p.parseExpression()
	if err != nil {
		return nil, fmt.Errorf("failed to parse condition: %v", err)
	}
	trueBody, err := p.parseBlockExpression()
	if err != nil {
		return nil, fmt.Errorf("failed to parse `true` branch body: %v", err)
	}
	var falseBody *BlockExpression
	if p.peek().Kind == token.Else {
		p.consumeAny()
		falseBody, err = p.parseBlockExpression()
		if err != nil {
			return nil, fmt.Errorf("failed to parse `false` branch body: %v", err)
		}
	}
	return &IfExpression{node: p.newNode(), Condition: condition, TrueBody: trueBody, FalseBody: falseBody}, nil
}

func (p *Parser) parseStructInitExpression(typeIdent TypeIdent) (*StructInitExpression, error) {
	if _, err := p.consume(token.LParen); err != nil {
		return nil, err
	}
	fields := []StructInitField{}
	expectComma := false
	for p.index < len(p.tokens) {
		t := p.peek()
		switch t.Kind {
		case token.RParen:
			p.consumeAny()
			return &StructInitExpression{node: p.newNode(), TypeIdent: typeIdent, Fields: fields}, nil
		case token.Comma:
			if !expectComma {
				return nil, fmt.Errorf("unexpected token: %s", t)
			}
			p.consumeAny()
			expectComma = false
		case token.Ident:
			p.consumeAny()
			fieldName := Ident(t.Value)
			if _, err := p.consume(token.Equal); err != nil {
				return nil, err
			}
			fieldValue, err := p.parseExpression()
			if err != nil {
				return nil, err
			}
			expectComma = true
			field := StructInitField{Name: fieldName, Value: fieldValue}
			fields = append(fields, field)
		default:
			return nil, fmt.Errorf("unexpected token: %s", t)
		}
	}
	return nil, fmt.Errorf("unexpected end of file while parsing struct init")
}

func (p *Parser) parseFunctionDeclaration(acceptSelfParameter bool) (*FunctionDeclaration, error) {
	if _, err := p.consume(token.Fn); err != nil {
		return nil, err
	}
	nameToken, err := p.consume(token.Ident)
	if err != nil {
		return nil, err
	}
	_, err = p.consume(token.LParen)
	if err != nil {
		return nil, err
	}
	args := []FunctionArg{}
	for p.index < len(p.tokens) {
		t := p.peek()
		if t.Kind == token.RParen {
			p.consumeAny()
			break
		}
		argNameToken := p.consumeAny()
		if argNameToken.Kind == token.Ident {
			argName := Ident(argNameToken.Value)
			argTypeToken, err := p.consume(token.TypeIdent)
			if err != nil {
				return nil, err
			}
			argType := TypeIdent(argTypeToken.Value)
			arg := FunctionArg{Name: argName, Type: argType}
			args = append(args, arg)
		} else if argNameToken.Kind == token.Self {
			if !acceptSelfParameter {
				return nil, fmt.Errorf("self parameter not allowed here")
			}
			if len(args) > 0 {
				return nil, fmt.Errorf("self parameter must be the first parameter")
			}
			arg := FunctionArg{Name: Ident("self"), Type: TypeIdent("Self")}
			args = append(args, arg)
		}
		t = p.peek()
		if t.Kind == token.RParen {
			p.consumeAny()
			break
		}
		if t.Kind != token.Comma {
			return nil, fmt.Errorf("expected comma or close paren, got %s", t)
		}
		p.consumeAny()
	}
	t := p.peek()
	returnType := TypeIdent("()")
	if t.Kind == token.TypeIdent {
		p.consumeAny()
		returnType = TypeIdent(t.Value)

	}
	return &FunctionDeclaration{
		node: p.newNode(), Name: Ident(nameToken.Value), Args: args, ReturnType: returnType,
	}, nil
}

func (p *Parser) parseFunctionDefinition(acceptSelfParameter bool) (*FunctionDefinition, error) {
	decl, err := p.parseFunctionDeclaration(acceptSelfParameter)
	if err != nil {
		return nil, err
	}
	body, err := p.parseBlockExpression()
	if err != nil {
		return nil, err
	}
	return &FunctionDefinition{node: p.newNode(), Decl: decl, Body: body}, nil
}

func (p *Parser) parseVariableDefinition() (*VariableDefinition, error) {
	var mutable bool
	switch p.consumeAny().Kind {
	case token.Mut:
		mutable = true
	case token.Let:
		mutable = false
	default:
		return nil, fmt.Errorf("expected mut keyword")
	}
	identToken, err := p.consume(token.Ident)
	if err != nil {
		return nil, err
	}
	if _, err = p.consume(token.Equal); err != nil {
		return nil, err
	}
	value, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	return &VariableDefinition{node: p.newNode(), Name: Ident(identToken.Value), Value: value, Mutable: mutable}, nil
}

func (p *Parser) parseAssignmentStatement(lhs Expression) (*AssignmentStatement, error) {
	rhs, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	switch lhs := lhs.(type) {
	case *IdentExpression:
		return &AssignmentStatement{node: p.newNode(), Variable: lhs, Field: nil, Rhs: rhs}, nil
	case *MemberExpression:
		switch variable := lhs.Target.(type) {
		case *IdentExpression:
			return &AssignmentStatement{node: p.newNode(), Variable: variable, Field: &lhs.Field, Rhs: rhs}, nil
		}
	}
	return nil, fmt.Errorf("expected identifier or member expression with identifier as target, got %s", lhs)
}

func (p *Parser) parseExpression() (Expression, error) {
	return p.parseBinaryExpression(0)
}

// Parse an expression as the left-hand-side and look at the token after it.
// If that token signals that the expression is part of a binary expression (i.e. is `+`, `==`, etc.),
// parse the right-hand-side and return a `BinaryExpression`.
//
// Parsing a binary expression takes operator precedence into account. That is some operators have
// a higher precedence than others, i.e. `a + b * c` should be parsed as `a + (b * c)`
// and not as `(a + b) * c`.
func (p *Parser) parseBinaryExpression(minPrecedence int) (Expression, error) {
	lhs, err := p.parseExpressionWithPostfix()
	if err != nil {
		return nil, err
	}
	// Technically, the AssignmentStatement is not an expression but we parse it here anyway
	// because it fits here very well.
	if p.peek().Kind == token.Equal {
		p.consumeAny()
		return p.parseAssignmentStatement(lhs)
	}
	precedences := map[token.TokenKind]int{
		token.Plus:       2,
		token.EqualEqual: 1,
	}
	ops := map[token.TokenKind]BinaryOperator{
		token.Plus:       OpAdd,
		token.EqualEqual: OpEquality,
	}
	for {
		op := p.peek()
		precedence, isOp := precedences[op.Kind]
		if !isOp || precedence <= minPrecedence {
			break
		}
		p.consumeAny()
		rhs, err := p.parseExpressionWithPostfix()
		if err != nil {
			return nil, err
		}
		lhs = &BinaryExpression{node: p.newNode(), Op: ops[op.Kind], Lhs: lhs, Rhs: rhs}
	}
	return lhs, nil
}

// Parse an expression and then look at the next token to determine whether it's a
// member expression or call expression.
// Even though syntactically possible we forbid some expressions from being callable
// or the lhs of a member expression like the if expression. You would have to use
// parenthesis around those expressions to call them or use them as a member expression.
func (p *Parser) parseExpressionWithPostfix() (Expression, error) {
	expr, err := p.parsePrimaryExpression()
	if err != nil {
		return nil, err
	}
	for p.index < len(p.tokens) {
		_, is_forbidden_expression := expr.(*IfExpression)
		if !is_forbidden_expression {
			_, is_forbidden_expression = expr.(*BlockExpression)
		}
		switch p.peek().Kind {
		case token.Dot:
			if is_forbidden_expression {
				return nil, fmt.Errorf("block and if expressions cannot be used as member expressions")
			}
			p.consumeAny()
			field := p.consumeAny()
			if field.Kind != token.Ident {
				return nil, fmt.Errorf("expected identifier after '.', got %s", field)
			}
			expr = &MemberExpression{node: p.newNode(), Target: expr, Field: Ident(field.Value)}
		case token.LParen:
			if is_forbidden_expression {
				return nil, fmt.Errorf("block and if expressions cannot be called")
			}
			expr, err = p.parseCallExpression(expr)
			if err != nil {
				return nil, err
			}
		default:
			return expr, nil
		}
	}
	panic("unreachable")
}

func (p *Parser) parsePrimaryExpression() (Expression, error) {
	t := p.peek()
	switch t.Kind {
	case token.Ident:
		p.consumeAny()
		return &IdentExpression{node: p.newNode(), Ident: Ident(t.Value)}, nil
	case token.TypeIdent:
		p.consumeAny()
		ident := TypeIdent(t.Value)
		switch p.peek().Kind {
		case token.LParen:
			return p.parseStructInitExpression(ident)
		}
		expr := &TypeIdentExpression{node: p.newNode(), Ident: ident}
		return expr, nil
	case token.Self:
		p.consumeAny()
		return &IdentExpression{node: p.newNode(), Ident: Ident("self")}, nil
	case token.Str:
		p.consumeAny()
		return &StringLiteralExpression{node: p.newNode(), Value: t.Value}, nil
	case token.Int:
		p.consumeAny()
		value, err := strconv.ParseInt(t.Value, 10, 64)
		if err != nil {
			return nil, fmt.Errorf("failed to parse int literal: %v", err)
		}
		return &IntLiteralExpression{node: p.newNode(), Value: value}, nil
	case token.True:
		p.consumeAny()
		return &BoolLiteralExpression{node: p.newNode(), Value: true}, nil
	case token.False:
		p.consumeAny()
		return &BoolLiteralExpression{node: p.newNode(), Value: false}, nil
	case token.LCurly:
		return p.parseBlockExpression()
	case token.If:
		return p.parseIfExpression()
	default:
		return nil, fmt.Errorf("expected expression, got token: %s", t)
	}

}

func (p *Parser) parseLoopStatement() (*LoopStatement, error) {
	if _, err := p.consume(token.Loop); err != nil {
		return nil, err
	}
	body, err := p.parseBlockExpression()
	if err != nil {
		return nil, err
	}
	return &LoopStatement{node: p.newNode(), Body: body}, nil
}

func (p *Parser) parseStructDeclaration() (*StructTypeDeclaration, error) {
	if _, err := p.consume(token.Struct); err != nil {
		return nil, err
	}
	identToken, err := p.consume(token.TypeIdent)
	if err != nil {
		return nil, err
	}
	if _, err = p.consume(token.LCurly); err != nil {
		return nil, err
	}
	fields := []StructTypeField{}
	for p.index < len(p.tokens) {
		t := p.peek()
		switch t.Kind {
		case token.RCurly:
			p.consumeAny()
			return &StructTypeDeclaration{node: p.newNode(), Name: TypeIdent(identToken.Value), Fields: fields}, nil
		case token.Ident:
			p.consumeAny()
			fieldName := t.Value
			typeToken, err := p.consume(token.TypeIdent)
			if err != nil {
				return nil, err
			}
			field := StructTypeField{Name: Ident(fieldName), Type: TypeIdent(typeToken.Value)}
			fields = append(fields, field)
		default:
			return nil, fmt.Errorf("unexpected token: %s", t)
		}
	}
	return nil, fmt.Errorf("unexpected end of file while parsing struct")
}

func (p *Parser) parseImplDefinition() (*ImplDefinition, error) {
	if _, err := p.consume(token.Impl); err != nil {
		return nil, err
	}
	targetIdentToken, err := p.consume(token.TypeIdent)
	if err != nil {
		return nil, err
	}
	target := TypeIdent(targetIdentToken.Value)
	var trait TypeIdent = ""
	if p.peek().Kind == token.For {
		p.consumeAny()
		traitIdentToken, err := p.consume(token.TypeIdent)
		if err != nil {
			return nil, err
		}
		trait = target
		target = TypeIdent(traitIdentToken.Value)
	}
	if _, err = p.consume(token.LCurly); err != nil {
		return nil, err
	}
	functions := []*FunctionDefinition{}
	for p.index < len(p.tokens) {
		t := p.peek()
		switch t.Kind {
		case token.RCurly:
			p.consumeAny()
			return &ImplDefinition{node: p.newNode(), Trait: trait, Target: target, Methods: functions}, nil
		case token.Fn:
			function, err := p.parseFunctionDefinition(true)
			if err != nil {
				return nil, err
			}
			functions = append(functions, function)
		default:
			return nil, fmt.Errorf("unexpected token: %s", t)
		}
	}
	return nil, fmt.Errorf("unexpected end of file while parsing impl")
}

func (p *Parser) parseTraitDeclaration() (*TraitDeclaration, error) {
	if _, err := p.consume(token.Trait); err != nil {
		return nil, err
	}
	typeIdentToken, err := p.consume(token.TypeIdent)
	if err != nil {
		return nil, err
	}
	if _, err = p.consume(token.LCurly); err != nil {
		return nil, err
	}
	methodDecls := []*FunctionDeclaration{}
	for p.index < len(p.tokens) {
		t := p.peek()
		switch t.Kind {
		case token.RCurly:
			p.consumeAny()
			return &TraitDeclaration{
				node:        p.newNode(),
				Name:        TypeIdent(typeIdentToken.Value),
				MethodDecls: methodDecls,
			}, nil
		case token.Fn:
			decl, err := p.parseFunctionDeclaration(true)
			if err != nil {
				return nil, err
			}
			methodDecls = append(methodDecls, decl)
		default:
			return nil, fmt.Errorf("unexpected token: %s", t)
		}
	}
	return nil, fmt.Errorf("unexpected end of file while parsing trait")

}

var EOF = fmt.Errorf("EOF")

func (p *Parser) ParseNode() (Node, error) {
	for p.index < len(p.tokens) {
		t := p.peek()
		switch t.Kind {
		case token.EOF:
			return nil, EOF
		case token.Fn:
			return p.parseFunctionDefinition(false)
		case token.Mut, token.Let:
			return p.parseVariableDefinition()
		case token.Loop:
			return p.parseLoopStatement()
		case token.Break:
			p.consumeAny()
			return &BreakStatement{node: p.newNode()}, nil
		case token.Continue:
			p.consumeAny()
			return &ContinueStatement{node: p.newNode()}, nil
		case token.Struct:
			return p.parseStructDeclaration()
		case token.Impl:
			return p.parseImplDefinition()
		case token.Trait:
			return p.parseTraitDeclaration()
		case token.Ident, token.TypeIdent, token.LCurly, token.If, token.True, token.False, token.Str, token.Int, token.Self:
			expr, err := p.parseExpression()
			if err != nil {
				return nil, err
			}
			return expr, nil
		default:
			return nil, fmt.Errorf("unexpected token: %s", t)
		}
	}
	return nil, fmt.Errorf("unexpected end of file")
}

func (p *Parser) Parse(moduleName Ident) (*Module, error) {
	nodes := []Node{}
	for p.index < len(p.tokens) {
		node, err := p.ParseNode()
		if err == EOF {
			if len(nodes) == 0 {
				return nil, fmt.Errorf("expected at least one AST node")
			}
			return &Module{node: p.newNode(), Name: moduleName, Nodes: nodes}, nil
		}
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, node)
	}
	return nil, fmt.Errorf("unexpected end of file")
}

func Parse(tokens []token.Token, moduleName Ident) (*Module, error) {
	p := Parser{tokens: tokens, index: 0, nodeId: 0}
	return p.Parse(moduleName)
}
