package ast

import (
	"fmt"
	"slices"
	"strconv"

	"github.com/flunderpero/klar/bootstrap/base"
	"github.com/flunderpero/klar/bootstrap/token"
	"github.com/pkg/errors"
)

type NodeId int

func (id NodeId) String() string {
	return fmt.Sprintf("node%d", id)
}

func (id NodeId) IdMarker() {}

type Node interface {
	String() string
	Id() NodeId
	Span() token.Span
}

type node struct {
	id   NodeId
	span token.Span
}

func (n *node) Id() NodeId {
	return n.id
}

func (n *node) Span() token.Span {
	return n.span
}

type Ident string

type TypeParam struct {
	node
	Name Ident
}

func (t TypeParam) String() string {
	return fmt.Sprintf("TypeParam %q", t.Name)
}

func (t TypeParam) TypeName() string {
	return string(t.Name)
}

func typeParamsString(params []TypeParam) string {
	s := ""
	if len(params) > 0 {
		s = fmt.Sprintf("\n(TypeParams)%s", base.IndentSlice(params, 1))
	}
	return s
}

func typeArgsString(args []Type) string {
	s := ""
	if len(args) > 0 {
		s = fmt.Sprintf("\n(TypeArgs)%s", base.IndentSlice(args, 1))
	}
	return s
}

type Type interface {
	Node
	TypeName() string
}

type SimpleType struct {
	node
	Name Ident
}

func (t SimpleType) String() string {
	return fmt.Sprintf("SimpleType %q", t.Name)
}

func (t SimpleType) TypeName() string {
	return string(t.Name)
}

func NewSimpleType(name Ident, id NodeId, span token.Span) *SimpleType {
	return &SimpleType{node: node{id: id, span: span}, Name: name}
}

type FunctionType struct {
	node
	TypeParams []TypeParam
	ArgTypes   []Type
	ReturnType Type
}

func (t FunctionType) String() string {
	return fmt.Sprintf(
		"FunctionType\n%s%s\n%s",
		base.IndentString(typeParamsString(t.TypeParams), 1), base.IndentSlice(t.ArgTypes, 1), base.Indent(t.ReturnType, 1))
}

func (t FunctionType) TypeName() string {
	argTypes := ""
	for i, argType := range t.ArgTypes {
		if i > 0 {
			argTypes += ","
		}
		argTypes += argType.TypeName()
	}
	return fmt.Sprintf("fn(%s)%s", argTypes, t.ReturnType.TypeName())
}

func (ty Ident) String() string {
	return string(ty)
}

type Expression interface {
	Node
}

type IdentExpression struct {
	node
	Ident Ident
}

func NewIdentExpression(ident Ident, id NodeId, span token.Span) *IdentExpression {
	return &IdentExpression{node: node{id: id, span: span}, Ident: ident}
}

func (expr *IdentExpression) String() string {
	return fmt.Sprintf("IdentExpression %q", expr.Ident)
}

type StringLiteralExpression struct {
	node
	Value string
}

func (expr *StringLiteralExpression) String() string {
	return fmt.Sprintf("StringLiteralExpression %q", expr.Value)
}

type IntLiteralExpression struct {
	node
	Value int64
}

func (expr *IntLiteralExpression) String() string {
	return fmt.Sprintf("IntLiteralExpression \"%d\"", expr.Value)
}

type BoolLiteralExpression struct {
	node
	Value bool
}

func (expr *BoolLiteralExpression) String() string {
	return fmt.Sprintf("BoolLiteralExpression \"%t\"", expr.Value)
}

type MemberExpression struct {
	node
	Target Expression
	Field  Ident
}

func (expr *MemberExpression) String() string {
	return fmt.Sprintf("MemberExpression\n%s\n%s", base.Indent(expr.Target, 1), base.Indent(expr.Field, 1))
}

type BinaryOperator string

const (
	OpAdd      BinaryOperator = "+"
	OpEquality BinaryOperator = "=="
)

func (op BinaryOperator) String() string {
	return string(op)
}

type BinaryExpression struct {
	node
	Lhs Expression
	Rhs Expression
	Op  BinaryOperator
}

func (expr *BinaryExpression) String() string {
	return fmt.Sprintf(
		"BinaryExpression\n%s\n%s\n%s", base.Indent(expr.Lhs, 1), base.Indent(expr.Op, 1), base.Indent(expr.Rhs, 1))
}

type StructInitField struct {
	Name  Ident
	Value Expression
	Span  token.Span
}

func (f StructInitField) String() string {
	return fmt.Sprintf("%s\n%s", f.Name, base.Indent(f.Value, 1))
}

type StructInitExpression struct {
	node
	Ident  Ident
	Fields []StructInitField
}

func (s *StructInitExpression) String() string {
	return fmt.Sprintf("StructInitExpression\n%s%s", base.Indent(s.Ident, 1), base.IndentSlice(s.Fields, 1))
}

type CallExpression struct {
	node
	TypeArgs []Type
	Callee   Expression
	Args     []Expression
}

func (expr *CallExpression) String() string {
	return fmt.Sprintf(
		"CallExpression\n%s%s%s",
		base.Indent(expr.Callee, 1), base.IndentString(typeArgsString(expr.TypeArgs), 1), base.IndentSlice(expr.Args, 1))
}

type BlockExpression struct {
	node
	Nodes []Node
}

func (expr BlockExpression) String() string {
	return fmt.Sprintf("BlockExpression%s", base.IndentSlice(expr.Nodes, 1))
}

type IfExpression struct {
	node
	Condition Expression
	TrueBody  *BlockExpression
	FalseBody *BlockExpression
}

func (expr *IfExpression) String() string {
	if expr.FalseBody != nil {
		return fmt.Sprintf(
			"IfExpression\n%s\n%s\n%s",
			base.Indent(expr.Condition, 1),
			base.Indent(expr.TrueBody, 1),
			base.Indent(expr.FalseBody, 1))
	}
	return fmt.Sprintf("IfExpression\n%s\n%s", base.Indent(expr.Condition, 1), base.Indent(expr.TrueBody, 1))

}

type LoopStatement struct {
	node
	Body *BlockExpression
}

func (l *LoopStatement) String() string {
	return fmt.Sprintf("LoopStatement\n%s", base.Indent(l.Body, 1))
}

type BreakStatement struct {
	node
}

func (b *BreakStatement) String() string {
	return "BreakStatement"
}

type ContinueStatement struct {
	node
}

func (b *ContinueStatement) String() string {
	return "ContinueStatement"
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
		return fmt.Sprintf(
			"AssignmentStatement\n%s\n%s\n%s", base.Indent(a.Variable, 1), base.Indent(*a.Field, 1), base.Indent(a.Rhs, 1))
	}
	return fmt.Sprintf("AssignmentStatement\n%s\n%s", base.Indent(a.Variable, 1), base.Indent(a.Rhs, 1))
}

func (a *AssignmentStatement) IsAssignToMember() bool {
	return a.Field != nil
}

type Module struct {
	node
	Name  Ident
	Nodes []Node
}

func (m Module) String() string {
	return fmt.Sprintf("Module\n%s%s", base.Indent(m.Name, 1), base.IndentSlice(m.Nodes, 1))
}

type StructTypeField struct {
	Name Ident
	Type Type
	Span token.Span
}

func (f StructTypeField) String() string {
	return fmt.Sprintf("%s\n%s", f.Name, base.Indent(f.Type, 1))
}

type StructTypeDeclaration struct {
	node
	Name   Ident
	Fields []StructTypeField
}

func (st StructTypeDeclaration) String() string {
	return fmt.Sprintf("StructTypeDeclaration\n%s%s", base.Indent(st.Name, 1), base.IndentSlice(st.Fields, 1))
}

func (st *StructTypeDeclaration) FindField(name Ident) (*StructTypeField, error) {
	index := slices.IndexFunc(st.Fields, func(field StructTypeField) bool { return field.Name == name })
	if index < 0 {
		return nil, errors.Errorf("field %q not found in struct %q", name, st.Name)
	}
	return &st.Fields[index], nil
}

type FunctionArg struct {
	Name Ident
	Type Type
	Span token.Span
}

func (f FunctionArg) String() string {
	return fmt.Sprintf("%s\n%s", f.Name, base.Indent(f.Type, 1))
}

type FunctionDeclaration struct {
	node
	Name       Ident
	TypeParams []TypeParam
	Args       []FunctionArg
	ReturnType Type
}

func (f FunctionDeclaration) String() string {
	return fmt.Sprintf(
		"FunctionDeclaration\n%s%s%s\n%s",
		base.Indent(f.Name, 1),
		base.IndentString(typeParamsString(f.TypeParams), 1),
		base.IndentSlice(f.Args, 1),
		base.Indent(f.ReturnType, 1))
}

type FunctionDefinition struct {
	node
	Decl *FunctionDeclaration
	Body *BlockExpression
}

func (f *FunctionDefinition) String() string {
	return fmt.Sprintf("FunctionDefinition\n%s\n%s", base.Indent(f.Decl, 1), base.Indent(f.Body, 1))
}

type ImplDefinition struct {
	node
	Target  Ident
	Methods []*FunctionDefinition
	Trait   Ident // optional
}

func (impl *ImplDefinition) ImplementsTrait() bool {
	return impl.Trait != ""
}

func (impl ImplDefinition) String() string {
	return fmt.Sprintf("ImplDefinition\n%s%s)", base.Indent(impl.Target, 1), base.IndentSlice(impl.Methods, 1))
}

type TraitDeclaration struct {
	node
	Name        Ident
	MethodDecls []*FunctionDeclaration
}

func (trait *TraitDeclaration) String() string {
	return fmt.Sprintf("TraitDeclaration\n%s%s)", base.Indent(trait.Name, 1), base.IndentSlice(trait.MethodDecls, 1))
}

type VariableDefinition struct {
	node
	Name    Ident
	Mutable bool
	Value   Expression
}

func (v *VariableDefinition) String() string {
	mutable := "(immutable)"
	if v.Mutable {
		mutable = "(mutable)"
	}
	return fmt.Sprintf(
		"VariableDefinition\n%s\n%s\n%s", base.IndentString(mutable, 1), base.Indent(v.Name, 1), base.Indent(v.Value, 1))
}

type Parser struct {
	tokens []token.Token
	index  int
	nodeId NodeId
}

func (p *Parser) spanToHere(from token.Span) token.Span {
	span := from
	span.End = p.tokens[p.index-1].Span.End
	return span
}

func (p *Parser) newNode(from token.Span) node {
	p.nodeId = p.nodeId + 1
	return node{id: p.nodeId, span: p.spanToHere(from)}
}

func (p *Parser) consume(kind token.TokenKind) (token.Token, error) {
	t := p.tokens[p.index]
	if t.Kind != kind {
		return token.Token{}, errors.Errorf("%s: expected token of kind %q, got: %s", t.Span, kind, t.Kind)
	}
	p.index = p.index + 1
	return t, nil
}

func (p *Parser) consumeAny() token.Token {
	p.index = p.index + 1
	return p.tokens[p.index-1]
}

func (p *Parser) peek() token.Token {
	for p.tokens[p.index].Kind == token.LineComment {
		p.index += 1
	}
	if p.index >= len(p.tokens) {
		return token.Token{Kind: token.EOF}
	}
	return p.tokens[p.index]
}

func (p *Parser) peek1() token.Token {
	add := 1
	for p.tokens[p.index+add].Kind == token.LineComment {
		add += 1
	}
	if p.index+add >= len(p.tokens) {
		return token.Token{Kind: token.EOF}
	}
	return p.tokens[p.index+add]
}

func (p *Parser) span() token.Span {
	span := p.tokens[p.index].Span
	return span
}

func (p *Parser) parseTypeArgs() ([]Type, error) {
	if p.peek().Kind != token.LAngle {
		return nil, nil
	}
	p.consumeAny()
	args := []Type{}
	done := false
	for p.index < len(p.tokens) && !done {
		arg, err := p.parseType()
		if err != nil {
			return nil, err
		}
		args = append(args, arg)
		switch p.peek().Kind {
		case token.Comma:
			p.consumeAny()
		case token.RAngle:
			p.consumeAny()
			done = true
		default:
			return nil, errors.Errorf("expected comma or close angle bracket, got %s", p.peek())
		}
	}
	return args, nil
}

func (p *Parser) parseCallExpression(callee Expression) (*CallExpression, error) {
	typeArgs, err := p.parseTypeArgs()
	if err != nil {
		return nil, err
	}
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
				return nil, errors.Errorf("failed to parse call argument: %v", err)
			}
			args = append(args, arg)
		}
	}
	return &CallExpression{node: p.newNode(callee.Span()), Callee: callee, TypeArgs: typeArgs, Args: args}, nil
}

func (p *Parser) parseBlockExpression() (*BlockExpression, error) {
	from := p.span()
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
	return &BlockExpression{node: p.newNode(from), Nodes: nodes}, nil
}

func (p *Parser) parseIfExpression() (*IfExpression, error) {
	from := p.span()
	if _, err := p.consume(token.If); err != nil {
		return nil, err
	}
	condition, err := p.parseExpression()
	if err != nil {
		return nil, errors.Errorf("failed to parse condition: %v", err)
	}
	trueBody, err := p.parseBlockExpression()
	if err != nil {
		return nil, errors.Errorf("failed to parse `true` branch body: %v", err)
	}
	var falseBody *BlockExpression
	if p.peek().Kind == token.Else {
		p.consumeAny()
		falseBody, err = p.parseBlockExpression()
		if err != nil {
			return nil, errors.Errorf("failed to parse `false` branch body: %v", err)
		}
	}
	return &IfExpression{node: p.newNode(from), Condition: condition, TrueBody: trueBody, FalseBody: falseBody}, nil
}

func (p *Parser) parseStructInitExpression(typeIdent Ident, from token.Span) (*StructInitExpression, error) {
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
			return &StructInitExpression{node: p.newNode(from), Ident: typeIdent, Fields: fields}, nil
		case token.Comma:
			if !expectComma {
				return nil, errors.Errorf("unexpected token: %s", t)
			}
			p.consumeAny()
			expectComma = false
		case token.Ident:
			from := p.span()
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
			field := StructInitField{Name: fieldName, Value: fieldValue, Span: p.spanToHere(from)}
			fields = append(fields, field)
		default:
			return nil, errors.Errorf("unexpected token: %s", t)
		}
	}
	return nil, errors.Errorf("unexpected end of file while parsing struct init")
}

func (p *Parser) parseFunctionType() (*FunctionType, error) {
	from := p.span()
	if _, err := p.consume(token.Fn); err != nil {
		return nil, err
	}
	if _, err := p.consume(token.LParen); err != nil {
		return nil, err
	}
	argTypes := []Type{}
	for p.index < len(p.tokens) {
		if p.peek().Kind == token.RParen {
			break
		}
		argType, err := p.parseType()
		if err != nil {
			return nil, err
		}
		argTypes = append(argTypes, argType)
		t := p.peek()
		if t.Kind == token.RParen {
			p.consumeAny()
			break
		}
		if _, err := p.consume(token.Comma); err != nil {
			return nil, err
		}
	}
	returnTypeSpan := p.span()
	returnType, err := p.tryParseType(&SimpleType{node: p.newNode(returnTypeSpan), Name: "None"})
	if err != nil {
		return nil, err
	}
	return &FunctionType{node: p.newNode(from), ArgTypes: argTypes, ReturnType: returnType}, nil
}

func (p *Parser) parseType() (Type, error) {
	t := p.peek()
	switch t.Kind {
	case token.TypeIdent:
		p.consumeAny()
		return &SimpleType{node: p.newNode(p.span()), Name: Ident(t.Value)}, nil
	case token.Fn:
		return p.parseFunctionType()
	}
	return nil, errors.Errorf("%s: expected type, got %s", t.Span, t)
}

func (p *Parser) tryParseType(defaultValue Type) (Type, error) {
	t := p.peek()
	switch t.Kind {
	case token.TypeIdent:
		return p.parseType()
	case token.Fn:
		if p.peek1().Kind == token.LParen {
			return p.parseType()
		}
	}
	return defaultValue, nil
}

func (p *Parser) parseTypeParams() ([]TypeParam, error) {
	if p.peek().Kind != token.LAngle {
		return nil, nil
	}
	p.consumeAny()
	params := []TypeParam{}
	done := false
	for p.index < len(p.tokens) && !done {
		typeIdent, err := p.consume(token.TypeIdent)
		if err != nil {
			return nil, err
		}
		param := TypeParam{node: p.newNode(typeIdent.Span), Name: Ident(typeIdent.Value)}
		params = append(params, param)
		switch p.peek().Kind {
		case token.Comma:
			p.consumeAny()
		case token.RAngle:
			p.consumeAny()
			done = true
		default:
			return nil, errors.Errorf("expected comma or close angle bracket, got %s", p.peek())
		}
	}
	return params, nil
}

func (p *Parser) parseFunctionDeclaration(acceptSelfParameter bool) (*FunctionDeclaration, error) {
	from := p.span()
	if _, err := p.consume(token.Fn); err != nil {
		return nil, err
	}
	nameToken, err := p.consume(token.Ident)
	if err != nil {
		return nil, err
	}
	typeParams, err := p.parseTypeParams()
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
		from := p.span()
		argNameToken := p.consumeAny()
		if argNameToken.Kind == token.Ident {
			argName := Ident(argNameToken.Value)
			argType, err := p.parseType()
			if err != nil {
				return nil, err
			}
			arg := FunctionArg{Name: argName, Type: argType}
			args = append(args, arg)
		} else if argNameToken.Kind == token.Self {
			if !acceptSelfParameter {
				return nil, errors.Errorf("self parameter not allowed here")
			}
			if len(args) > 0 {
				return nil, errors.Errorf("self parameter must be the first parameter")
			}
			selfType := &SimpleType{node: p.newNode(from), Name: "Self"}
			arg := FunctionArg{Name: Ident("self"), Type: selfType, Span: p.spanToHere(from)}
			args = append(args, arg)
		}
		t = p.peek()
		if t.Kind == token.RParen {
			p.consumeAny()
			break
		}
		if t.Kind != token.Comma {
			return nil, errors.Errorf("expected comma or close paren, got %s", t)
		}
		p.consumeAny()
	}
	returnType, err := p.tryParseType(&SimpleType{node: p.newNode(from), Name: "None"})
	if err != nil {
		return nil, err
	}
	return &FunctionDeclaration{
		node: p.newNode(from), TypeParams: typeParams, Name: Ident(nameToken.Value), Args: args, ReturnType: returnType,
	}, nil
}

func (p *Parser) parseFunctionDefinition(acceptSelfParameter bool) (*FunctionDefinition, error) {
	from := p.span()
	decl, err := p.parseFunctionDeclaration(acceptSelfParameter)
	if err != nil {
		return nil, err
	}
	body, err := p.parseBlockExpression()
	if err != nil {
		return nil, err
	}
	return &FunctionDefinition{node: p.newNode(from), Decl: decl, Body: body}, nil
}

func (p *Parser) parseVariableDefinition() (*VariableDefinition, error) {
	from := p.span()
	var mutable bool
	switch p.consumeAny().Kind {
	case token.Mut:
		mutable = true
	case token.Let:
		mutable = false
	default:
		return nil, errors.Errorf("expected mut keyword")
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
	return &VariableDefinition{node: p.newNode(from), Name: Ident(identToken.Value), Value: value, Mutable: mutable}, nil
}

func (p *Parser) parseAssignmentStatement(lhs Expression) (*AssignmentStatement, error) {
	from := p.span()
	rhs, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	switch lhs := lhs.(type) {
	case *IdentExpression:
		return &AssignmentStatement{node: p.newNode(from), Variable: lhs, Field: nil, Rhs: rhs}, nil
	case *MemberExpression:
		switch variable := lhs.Target.(type) {
		case *IdentExpression:
			return &AssignmentStatement{node: p.newNode(from), Variable: variable, Field: &lhs.Field, Rhs: rhs}, nil
		}
	}
	return nil, errors.Errorf("expected identifier or member expression with identifier as target, got %s", lhs)
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
	from := p.span()
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
		lhs = &BinaryExpression{node: p.newNode(from), Op: ops[op.Kind], Lhs: lhs, Rhs: rhs}
	}
	return lhs, nil
}

// Parse an expression and then look at the next token to determine whether it's a
// member expression or call expression.
// Even though syntactically possible we forbid some expressions from being callable
// or the lhs of a member expression like the if expression. You would have to use
// parenthesis around those expressions to call them or use them as a member expression.
func (p *Parser) parseExpressionWithPostfix() (Expression, error) {
	from := p.span()
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
				return nil, errors.Errorf("block and if expressions cannot be used as member expressions")
			}
			p.consumeAny()
			field := p.consumeAny()
			if field.Kind != token.Ident {
				return nil, errors.Errorf("expected identifier after '.', got %s", field)
			}
			expr = &MemberExpression{node: p.newNode(from), Target: expr, Field: Ident(field.Value)}
		case token.LParen, token.LAngle:
			if is_forbidden_expression {
				return nil, errors.Errorf("block and if expressions cannot be called")
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
	from := p.span()
	t := p.peek()
	switch t.Kind {
	case token.Ident:
		p.consumeAny()
		return &IdentExpression{node: p.newNode(from), Ident: Ident(t.Value)}, nil
	case token.TypeIdent:
		p.consumeAny()
		ident := Ident(t.Value)
		switch p.peek().Kind {
		case token.LParen:
			return p.parseStructInitExpression(ident, from)
		}
		return &IdentExpression{node: p.newNode(from), Ident: ident}, nil
	case token.Self:
		p.consumeAny()
		return &IdentExpression{node: p.newNode(from), Ident: Ident("self")}, nil
	case token.Str:
		p.consumeAny()
		return &StringLiteralExpression{node: p.newNode(from), Value: t.Value}, nil
	case token.Int:
		p.consumeAny()
		value, err := strconv.ParseInt(t.Value, 10, 64)
		if err != nil {
			return nil, errors.Errorf("failed to parse int literal: %v", err)
		}
		return &IntLiteralExpression{node: p.newNode(from), Value: value}, nil
	case token.True:
		p.consumeAny()
		return &BoolLiteralExpression{node: p.newNode(from), Value: true}, nil
	case token.False:
		p.consumeAny()
		return &BoolLiteralExpression{node: p.newNode(from), Value: false}, nil
	case token.LCurly:
		return p.parseBlockExpression()
	case token.If:
		return p.parseIfExpression()
	default:
		return nil, errors.Errorf("expected expression, got token: %s", t)
	}

}

func (p *Parser) parseLoopStatement() (*LoopStatement, error) {
	from := p.span()
	if _, err := p.consume(token.Loop); err != nil {
		return nil, err
	}
	body, err := p.parseBlockExpression()
	if err != nil {
		return nil, err
	}
	return &LoopStatement{node: p.newNode(from), Body: body}, nil
}

func (p *Parser) parseStructDeclaration() (*StructTypeDeclaration, error) {
	from := p.span()
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
			return &StructTypeDeclaration{node: p.newNode(from), Name: Ident(identToken.Value), Fields: fields}, nil
		case token.Ident:
			from := p.span()
			p.consumeAny()
			fieldName := t.Value
			fieldType, err := p.parseType()
			if err != nil {
				return nil, err
			}
			field := StructTypeField{Name: Ident(fieldName), Type: fieldType, Span: p.spanToHere(from)}
			fields = append(fields, field)
		default:
			return nil, errors.Errorf("unexpected token: %s", t)
		}
	}
	return nil, errors.Errorf("unexpected end of file while parsing struct")
}

func (p *Parser) parseImplDefinition() (*ImplDefinition, error) {
	from := p.span()
	if _, err := p.consume(token.Impl); err != nil {
		return nil, err
	}
	targetIdentToken, err := p.consume(token.TypeIdent)
	if err != nil {
		return nil, err
	}
	target := Ident(targetIdentToken.Value)
	var trait Ident = ""
	if p.peek().Kind == token.For {
		p.consumeAny()
		traitIdentToken, err := p.consume(token.TypeIdent)
		if err != nil {
			return nil, err
		}
		trait = target
		target = Ident(traitIdentToken.Value)
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
			return &ImplDefinition{node: p.newNode(from), Trait: trait, Target: target, Methods: functions}, nil
		case token.Fn:
			function, err := p.parseFunctionDefinition(true)
			if err != nil {
				return nil, err
			}
			functions = append(functions, function)
		default:
			return nil, errors.Errorf("unexpected token: %s", t)
		}
	}
	return nil, errors.Errorf("unexpected end of file while parsing impl")
}

func (p *Parser) parseTraitDeclaration() (*TraitDeclaration, error) {
	from := p.span()
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
				node:        p.newNode(from),
				Name:        Ident(typeIdentToken.Value),
				MethodDecls: methodDecls,
			}, nil
		case token.Fn:
			decl, err := p.parseFunctionDeclaration(true)
			if err != nil {
				return nil, err
			}
			methodDecls = append(methodDecls, decl)
		default:
			return nil, errors.Errorf("unexpected token: %s", t)
		}
	}
	return nil, errors.Errorf("unexpected end of file while parsing trait")
}

var EOF = errors.Errorf("EOF")

func (p *Parser) ParseNode() (Node, error) {
	from := p.span()
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
			return &BreakStatement{node: p.newNode(from)}, nil
		case token.Continue:
			p.consumeAny()
			return &ContinueStatement{node: p.newNode(from)}, nil
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
			return nil, errors.Errorf("unexpected token: %s", t)
		}
	}
	return nil, errors.Errorf("unexpected end of file")
}

func (p *Parser) Parse(moduleName Ident) (*Module, error) {
	from := p.span()
	nodes := []Node{}
	for p.index < len(p.tokens) {
		node, err := p.ParseNode()
		if err == EOF {
			if len(nodes) == 0 {
				return nil, errors.Errorf("expected at least one AST node")
			}
			return &Module{node: p.newNode(from), Name: moduleName, Nodes: nodes}, nil
		}
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, node)
	}
	return nil, errors.Errorf("unexpected end of file")
}

func Parse(tokens []token.Token, moduleName Ident) (*Module, error) {
	p := Parser{tokens: tokens, index: 0, nodeId: 0}
	return p.Parse(moduleName)
}
