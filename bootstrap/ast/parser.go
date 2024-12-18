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

type NodeCreator struct {
	nextNodeId int
}

func NewNodeCreator() *NodeCreator {
	return &NodeCreator{nextNodeId: 0}
}

func (self *NodeCreator) newNodeBase(span token.Span) nodeBase {
	self.nextNodeId++
	return nodeBase{id: NodeId(self.nextNodeId), span: span}
}

func (self *NodeCreator) NewIdentExpression(ident Ident, span token.Span) *IdentExpression {
	return &IdentExpression{nodeBase: self.newNodeBase(span), Ident: ident}
}

func (self *NodeCreator) NewCallExpression(callee Expression, args []CallArg, span token.Span) *CallExpression {
	return &CallExpression{nodeBase: self.newNodeBase(span), Callee: callee, Args: args}
}

type nodeBase struct {
	id   NodeId
	span token.Span
}

func (n *nodeBase) Id() NodeId {
	return n.id
}

func (n *nodeBase) Span() token.Span {
	return n.span
}

type Ident string

type TypeParam struct {
	nodeBase
	Name       Ident
	TraitBound Type
}

func (t TypeParam) String() string {
	traitBound := ""
	if t.TraitBound != nil {
		traitBound = fmt.Sprintf("\n(TraitBound)\n%s", base.Indent(t.TraitBound, 1))
	}
	return fmt.Sprintf("TypeParam %q%s", t.Name, traitBound)
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
	nodeBase
	Name     Ident
	TypeArgs []Type
}

func (t SimpleType) String() string {
	return fmt.Sprintf("SimpleType %q%s", t.Name, base.IndentString(typeArgsString(t.TypeArgs), 1))
}

func (t SimpleType) TypeName() string {
	return string(t.Name)
}

type TupleType struct {
	nodeBase
	Values []Type
}

func (t TupleType) String() string {
	return fmt.Sprintf("TupleType%s", base.IndentSlice(t.Values, 1))
}

func (t TupleType) TypeName() string {
	values := ""
	for i, value := range t.Values {
		if i > 0 {
			values += ","
		}
		values += value.TypeName()
	}
	return fmt.Sprintf("(%s)", values)
}

type FunctionType struct {
	nodeBase
	TypeParams []TypeParam
	Params     []Type
	Result     Type
}

func (t FunctionType) String() string {
	return fmt.Sprintf(
		"FunctionType%s%s\n%s",
		base.IndentString(typeParamsString(t.TypeParams), 1), base.IndentSlice(t.Params, 1), base.Indent(t.Result, 1))
}

func (t FunctionType) TypeName() string {
	params := ""
	for i, param := range t.Params {
		if i > 0 {
			params += ","
		}
		params += param.TypeName()
	}
	return fmt.Sprintf("fn(%s)%s", params, t.Result.TypeName())
}

func (ty Ident) String() string {
	return string(ty)
}

type Expression interface {
	Node
}

type IdentExpression struct {
	nodeBase
	Ident    Ident
	TypeArgs []Type
}

func (expr *IdentExpression) String() string {
	return fmt.Sprintf("IdentExpression %q%s", expr.Ident, base.IndentString(typeArgsString(expr.TypeArgs), 1))
}

type StringLiteralExpression struct {
	nodeBase
	Value string
}

func (expr *StringLiteralExpression) String() string {
	return fmt.Sprintf("StringLiteralExpression %q", expr.Value)
}

type CharLiteralExpression struct {
	nodeBase
	Value uint32
}

func (expr *CharLiteralExpression) String() string {
	return fmt.Sprintf("CharLiteralExpression '%s'", string(rune(expr.Value)))
}

type IntLiteralExpression struct {
	nodeBase
	Int64    int64
	UInt64   uint64
	IsUInt64 bool
}

func (expr *IntLiteralExpression) String() string {
	var value string
	if expr.IsUInt64 {
		value = fmt.Sprintf("%d", expr.UInt64)
	} else {
		value = fmt.Sprintf("%d", expr.Int64)
	}
	return fmt.Sprintf("IntLiteralExpression %q", value)
}

type BoolLiteralExpression struct {
	nodeBase
	Value bool
}

func (expr *BoolLiteralExpression) String() string {
	return fmt.Sprintf("BoolLiteralExpression \"%t\"", expr.Value)
}

type TupleLiteralExpression struct {
	nodeBase
	Values []Expression
}

func (self *TupleLiteralExpression) String() string {
	return fmt.Sprintf("TupleLiteralExpression%s", base.IndentSlice(self.Values, 1))
}

type MemberExpressionField string

func (self MemberExpressionField) String() string {
	return string(self)
}

func (self MemberExpressionField) IsIndex() bool {
	_, err := strconv.Atoi(string(self))
	return err == nil
}

func (self MemberExpressionField) AsIndex() int {
	index, err := strconv.Atoi(string(self))
	if err != nil {
		panic(fmt.Sprintf("expected index, got identifier %q", self))
	}
	return index
}

func (self MemberExpressionField) AsIdent() Ident {
	if self.IsIndex() {
		panic(fmt.Sprintf("expected identifier, got index %q", self))
	}
	return Ident(self)
}

type MemberExpression struct {
	nodeBase
	Target   Expression
	Field    MemberExpressionField
	TypeArgs []Type
}

func (expr *MemberExpression) String() string {
	return fmt.Sprintf("MemberExpression\n%s\n%s%s", base.Indent(expr.Target, 1), base.Indent(expr.Field, 1), base.IndentString(typeArgsString(expr.TypeArgs), 1))
}

type BinaryOperator string

const (
	OpAdd                BinaryOperator = "+"
	OpMultiply           BinaryOperator = "*"
	OpDivide             BinaryOperator = "/"
	OpModulo             BinaryOperator = "%"
	OpEqual              BinaryOperator = "=="
	OpLessThan           BinaryOperator = "<"
	OpLessThanOrEqual    BinaryOperator = "<="
	OpGreaterThan        BinaryOperator = ">"
	OpGreaterThanOrEqual BinaryOperator = ">="
	OpNotEqual           BinaryOperator = "!="
	OpAnd                BinaryOperator = "and"
	OpOr                 BinaryOperator = "or"
)

func (op BinaryOperator) String() string {
	return string(op)
}

type BinaryExpression struct {
	nodeBase
	Lhs Expression
	Rhs Expression
	Op  BinaryOperator
}

func (expr *BinaryExpression) String() string {
	return fmt.Sprintf(
		"BinaryExpression\n%s\n%s\n%s", base.Indent(expr.Lhs, 1), base.Indent(expr.Op, 1), base.Indent(expr.Rhs, 1))
}

type UnaryOperator string

const (
	OpNot UnaryOperator = "not"
)

func (op UnaryOperator) String() string {
	return string(op)
}

type UnaryExpression struct {
	nodeBase
	Value Expression
	Op    UnaryOperator
}

func (expr *UnaryExpression) String() string {
	return fmt.Sprintf("UnaryExpression\n%s\n%s", base.Indent(expr.Op, 1), base.Indent(expr.Value, 1))
}

type CallArg struct {
	// Optional, maybe set to "".
	Name  Ident
	Value Expression
	Span  token.Span
}

func (f CallArg) String() string {
	if f.Name != "" {
		return fmt.Sprintf("%s\n%s", f.Name, base.Indent(f.Value, 1))
	}
	return f.Value.String()
}

type CallExpression struct {
	nodeBase
	Callee Expression
	Args   []CallArg
}

func (expr *CallExpression) String() string {
	return fmt.Sprintf("CallExpression\n%s%s", base.Indent(expr.Callee, 1), base.IndentSlice(expr.Args, 1))
}

type BlockExpression struct {
	nodeBase
	Nodes []Node
}

func (expr BlockExpression) String() string {
	return fmt.Sprintf("BlockExpression%s", base.IndentSlice(expr.Nodes, 1))
}

type IfExpression struct {
	nodeBase
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
	nodeBase
	Body *BlockExpression
}

func (l *LoopStatement) String() string {
	return fmt.Sprintf("LoopStatement\n%s", base.Indent(l.Body, 1))
}

type BreakStatement struct {
	nodeBase
}

func (b *BreakStatement) String() string {
	return "BreakStatement"
}

type ContinueStatement struct {
	nodeBase
}

func (b *ContinueStatement) String() string {
	return "ContinueStatement"
}

type AssignmentStatement struct {
	nodeBase
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
	nodeBase
	Name  Ident
	Nodes []Node
}

func (m Module) String() string {
	return fmt.Sprintf("Module\n%s%s", base.Indent(m.Name, 1), base.IndentSlice(m.Nodes, 1))
}

type NamedVariant struct {
	Name Ident
	Type Type
}

func (self NamedVariant) String() string {
	return fmt.Sprintf("NamedVariant\n%s\n%s", base.Indent(self.Name, 1), base.Indent(self.Type, 1))
}

type UnionVariantKind int

const (
	UnionVariantKindNamed UnionVariantKind = 1
	UnionVariantKindType  UnionVariantKind = 2
)

type UnionVariant struct {
	Kind  UnionVariantKind
	Named NamedVariant
	Type  Type
}

func (self UnionVariant) String() string {
	if self.Kind == UnionVariantKindType {
		return fmt.Sprintf("UnionVariant\n%s", base.Indent(self.Type, 1))
	}
	return fmt.Sprintf("UnionVariant\n%s", base.Indent(self.Named, 1))
}

type UnionTypeDeclaration struct {
	nodeBase
	Name       Ident
	TypeParams []TypeParam
	Variants   []UnionVariant
}

func (self UnionTypeDeclaration) String() string {
	name := ""
	if self.Name.String() != "" {
		name = fmt.Sprintf("\n%s", base.Indent(self.Name, 1))
	}
	return fmt.Sprintf("UnionTypeDeclaration%s%s%s",
		name,
		base.IndentString(typeParamsString(self.TypeParams), 1),
		base.IndentSlice(self.Variants, 1))
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
	nodeBase
	Name       Ident
	TypeParams []TypeParam
	Fields     []StructTypeField
}

func (st StructTypeDeclaration) String() string {
	return fmt.Sprintf(
		"StructTypeDeclaration%s\n%s%s",
		base.IndentString(typeParamsString(st.TypeParams), 1),
		base.Indent(st.Name, 1),
		base.IndentSlice(st.Fields, 1))
}

func (st *StructTypeDeclaration) FindField(name Ident) (*StructTypeField, error) {
	index := slices.IndexFunc(st.Fields, func(field StructTypeField) bool { return field.Name == name })
	if index < 0 {
		return nil, errors.Errorf("field %q not found in struct %q", name, st.Name)
	}
	return &st.Fields[index], nil
}

type FunctionParam struct {
	Name Ident
	Type Type
	Span token.Span
}

func (f FunctionParam) String() string {
	return fmt.Sprintf("%s\n%s", f.Name, base.Indent(f.Type, 1))
}

type FunctionDeclaration struct {
	nodeBase
	Name       Ident
	TypeParams []TypeParam
	Params     []FunctionParam
	Result     Type
}

func (f FunctionDeclaration) String() string {
	return fmt.Sprintf(
		"FunctionDeclaration\n%s%s%s\n%s",
		base.Indent(f.Name, 1),
		base.IndentString(typeParamsString(f.TypeParams), 1),
		base.IndentSlice(f.Params, 1),
		base.Indent(f.Result, 1))
}

type FunctionDefinition struct {
	nodeBase
	Decl *FunctionDeclaration
	Body *BlockExpression
}

func (f *FunctionDefinition) String() string {
	return fmt.Sprintf("FunctionDefinition\n%s\n%s", base.Indent(f.Decl, 1), base.Indent(f.Body, 1))
}

type ImplDefinition struct {
	nodeBase
	Target        Ident
	Methods       []*FunctionDefinition
	Trait         Ident // optional
	TraitTypeArgs []Type
}

func (impl *ImplDefinition) ImplementsTrait() bool {
	return impl.Trait != ""
}

func (impl ImplDefinition) String() string {
	trait := ""
	if impl.Trait != "" {
		trait = fmt.Sprintf("\n(Trait)\n%s%s", base.IndentString(impl.Trait.String(), 1), base.IndentString(typeArgsString(impl.TraitTypeArgs), 1))
	}
	return fmt.Sprintf("ImplDefinition\n%s%s%s", base.Indent(impl.Target, 1), base.IndentString(trait, 1), base.IndentSlice(impl.Methods, 1))
}

type TraitDeclaration struct {
	nodeBase
	Name        Ident
	TypeParams  []TypeParam
	MethodDecls []*FunctionDeclaration
}

func (trait *TraitDeclaration) String() string {
	return fmt.Sprintf(
		"TraitDeclaration\n%s%s%s",
		base.Indent(trait.Name, 1),
		base.IndentString(typeParamsString(trait.TypeParams), 1),
		base.IndentSlice(trait.MethodDecls, 1))
}

type VariableDefinition struct {
	nodeBase
	Name Ident
	// Optional
	Type    Type
	Mutable bool
	Value   Expression
}

func (v *VariableDefinition) String() string {
	mutable := "(immutable)"
	if v.Mutable {
		mutable = "(mutable)"
	}
	ty := ""
	if v.Type != nil {
		ty = fmt.Sprintf("\n(Type)\n%s", base.Indent(v.Type, 1))
	}
	return fmt.Sprintf(
		"VariableDefinition\n%s\n%s%s\n%s",
		base.IndentString(mutable, 1), base.Indent(v.Name, 1), base.IndentString(ty, 1), base.Indent(v.Value, 1))
}

type Parser struct {
	tokens      []token.Token
	index       int
	nodeCreator *NodeCreator
}

func (p *Parser) spanToHere(from token.Span) token.Span {
	span := from
	span.End = p.tokens[p.index-1].Span.End
	return span
}

func (p *Parser) newNodeBase(from token.Span) nodeBase {
	return p.nodeCreator.newNodeBase(p.spanToHere(from))
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
	if _, err := p.consume(token.LParen); err != nil {
		return nil, err
	}
	args := []CallArg{}
	done := false
	hasNamedArgs := false
	for p.index < len(p.tokens) && !done {
		t := p.peek()
		switch t.Kind {
		case token.RParen:
			p.consumeAny()
			done = true
		case token.Comma:
			p.consumeAny()
		default:
			var name Ident = ""
			if t.Kind == token.Ident && p.peek1().Kind == token.Equal {
				name = Ident(t.Value)
				p.consumeAny()
				p.consumeAny()
				hasNamedArgs = true
				if slices.ContainsFunc(args, func(arg CallArg) bool { return arg.Name == name }) {
					return nil, errors.Errorf("%s: duplicate argument name %q", t.Span, name)
				}
			} else if hasNamedArgs {
				return nil, errors.Errorf("%s: positional argument after named argument", t.Span)
			}
			value, err := p.parseExpression()
			if err != nil {
				return nil, errors.Wrapf(err, "%s: failed to parse call argument", t.Span)
			}
			arg := CallArg{Name: name, Value: value, Span: p.spanToHere(t.Span)}
			args = append(args, arg)
		}
	}
	return &CallExpression{nodeBase: p.newNodeBase(callee.Span()), Callee: callee, Args: args}, nil
}

func (p *Parser) parseBlockExpression() (*BlockExpression, error) {
	from := p.span()
	t := p.peek()
	switch t.Kind {
	case token.LCurly:
		p.consumeAny()
	case token.FatArrow:
		// Single expression block.
		p.consumeAny()
		expr, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		return &BlockExpression{nodeBase: p.newNodeBase(from), Nodes: []Node{expr}}, nil
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
	return &BlockExpression{nodeBase: p.newNodeBase(from), Nodes: nodes}, nil
}

func (p *Parser) parseIfExpression() (*IfExpression, error) {
	from := p.span()
	if _, err := p.consume(token.If); err != nil {
		return nil, err
	}
	condition, err := p.parseExpression()
	if err != nil {
		return nil, errors.Wrapf(err, "failed to parse condition")
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
	return &IfExpression{nodeBase: p.newNodeBase(from), Condition: condition, TrueBody: trueBody, FalseBody: falseBody}, nil
}

func (p *Parser) parseTupleLiteralExpression() (*TupleLiteralExpression, error) {
	from := p.span()
	if _, err := p.consume(token.LParen); err != nil {
		return nil, err
	}
	values := []Expression{}
	for p.index < len(p.tokens) {
		value, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		values = append(values, value)
		switch p.peek().Kind {
		case token.RParen:
			p.consumeAny()
			return &TupleLiteralExpression{nodeBase: p.newNodeBase(from), Values: values}, nil
		case token.Comma:
			p.consumeAny()
		default:
			return nil, errors.Errorf("expected comma or close paren, got %s", p.peek())
		}
	}
	panic("unexpected end of file while parsing tuple")
}

func (p *Parser) parseFunctionType() (*FunctionType, error) {
	from := p.span()
	if _, err := p.consume(token.LParen); err != nil {
		return nil, err
	}
	if _, err := p.consume(token.Fn); err != nil {
		return nil, err
	}
	if _, err := p.consume(token.LParen); err != nil {
		return nil, err
	}
	params := []Type{}
	for p.index < len(p.tokens) {
		if p.peek().Kind == token.RParen {
			break
		}
		param, err := p.parseType()
		if err != nil {
			return nil, err
		}
		params = append(params, param)
		t := p.peek()
		if t.Kind == token.RParen {
			p.consumeAny()
			break
		}
		if _, err := p.consume(token.Comma); err != nil {
			return nil, err
		}
	}
	resultSpan := p.span()
	result, err := p.tryParseType(&SimpleType{nodeBase: p.newNodeBase(resultSpan), Name: "None"})
	if err != nil {
		return nil, err
	}
	if _, err := p.consume(token.RParen); err != nil {
		return nil, err
	}
	return &FunctionType{nodeBase: p.newNodeBase(from), Params: params, Result: result}, nil
}

func (p *Parser) parseTupleType() (*TupleType, error) {
	from := p.span()
	if _, err := p.consume(token.LParen); err != nil {
		return nil, err
	}
	values := []Type{}
	for p.index < len(p.tokens) {
		if p.peek().Kind == token.RParen {
			break
		}
		value, err := p.parseType()
		if err != nil {
			return nil, err
		}
		values = append(values, value)
		t := p.peek()
		if t.Kind == token.RParen {
			p.consumeAny()
			break
		}
		if _, err := p.consume(token.Comma); err != nil {
			return nil, err
		}
	}
	return &TupleType{nodeBase: p.newNodeBase(from), Values: values}, nil
}

func (p *Parser) parseType() (Type, error) {
	t := p.peek()
	switch t.Kind {
	case token.TypeIdent:
		p.consumeAny()
		typeArgs, err := p.parseTypeArgs()
		if err != nil {
			return nil, err
		}
		return &SimpleType{nodeBase: p.newNodeBase(p.span()), Name: Ident(t.Value), TypeArgs: typeArgs}, nil
	case token.LParen:
		switch p.peek1().Kind {
		case token.Fn:
			return p.parseFunctionType()
		default:
			return p.parseTupleType()
		}
	}
	return nil, errors.Errorf("%s: expected type, got %s", t.Span, t)
}

func (p *Parser) tryParseType(defaultValue Type) (Type, error) {
	t := p.peek()
	switch t.Kind {
	case token.TypeIdent, token.LParen:
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
		var trait_bound Type
		if p.peek().Kind == token.Impl {
			p.consumeAny()
			trait_bound_, err := p.parseType()
			if err != nil {
				return nil, err
			}
			trait_bound = trait_bound_
		}
		param := TypeParam{nodeBase: p.newNodeBase(typeIdent.Span), Name: Ident(typeIdent.Value), TraitBound: trait_bound}
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
	params := []FunctionParam{}
	for p.index < len(p.tokens) {
		t := p.peek()
		if t.Kind == token.RParen {
			p.consumeAny()
			break
		}
		from := p.span()
		paramNameToken := p.consumeAny()
		if paramNameToken.Kind == token.Ident {
			paramName := Ident(paramNameToken.Value)
			paramType, err := p.parseType()
			if err != nil {
				return nil, err
			}
			param := FunctionParam{Name: paramName, Type: paramType}
			params = append(params, param)
		} else if paramNameToken.Kind == token.Self {
			if !acceptSelfParameter {
				return nil, errors.Errorf("self parameter not allowed here")
			}
			if len(params) > 0 {
				return nil, errors.Errorf("self parameter must be the first parameter")
			}
			selfType := &SimpleType{nodeBase: p.newNodeBase(from), Name: "Self"}
			param := FunctionParam{Name: Ident("self"), Type: selfType, Span: p.spanToHere(from)}
			params = append(params, param)
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
	result, err := p.tryParseType(&SimpleType{nodeBase: p.newNodeBase(from), Name: "None"})
	if err != nil {
		return nil, err
	}
	return &FunctionDeclaration{
		nodeBase: p.newNodeBase(from), TypeParams: typeParams, Name: Ident(nameToken.Value), Params: params, Result: result,
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
	return &FunctionDefinition{nodeBase: p.newNodeBase(from), Decl: decl, Body: body}, nil
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
	ty, err := p.tryParseType(nil)
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
	return &VariableDefinition{
		nodeBase: p.newNodeBase(from), Name: Ident(identToken.Value), Type: ty, Value: value, Mutable: mutable}, nil
}

func (p *Parser) parseAssignmentStatement(lhs Expression) (*AssignmentStatement, error) {
	from := p.span()
	rhs, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	switch lhs := lhs.(type) {
	case *IdentExpression:
		return &AssignmentStatement{nodeBase: p.newNodeBase(from), Variable: lhs, Field: nil, Rhs: rhs}, nil
	case *MemberExpression:
		switch variable := lhs.Target.(type) {
		case *IdentExpression:
			if lhs.Field.IsIndex() {
				return nil, errors.Errorf("%s: cannot assign to an index", lhs.span)
			}
			field := lhs.Field.AsIdent()
			return &AssignmentStatement{nodeBase: p.newNodeBase(from), Variable: variable, Field: &field, Rhs: rhs}, nil
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
	switch p.peek().Kind {
	case token.Equal:
		// Technically, the AssignmentStatement is not an expression but we parse it here anyway
		// because it fits here very well.
		p.consumeAny()
		return p.parseAssignmentStatement(lhs)
	case token.FatArrow:
		// This is the start of a single-expression block.
		return lhs, nil
	}
	precedences := map[token.TokenKind]int{
		token.Or:                 1,
		token.And:                2,
		token.EqualEqual:         3,
		token.NotEqual:           3,
		token.LessThanOrEqual:    3,
		token.LAngle:             3,
		token.RAngle:             3,
		token.GreaterThanOrEqual: 3,
		token.Plus:               4,
		token.Star:               5,
		token.Slash:              5,
		token.Percent:            5,
	}
	ops := map[token.TokenKind]BinaryOperator{
		token.Plus:               OpAdd,
		token.Star:               OpMultiply,
		token.Slash:              OpDivide,
		token.Percent:            OpModulo,
		token.NotEqual:           OpNotEqual,
		token.EqualEqual:         OpEqual,
		token.LAngle:             OpLessThan,
		token.RAngle:             OpGreaterThan,
		token.LessThanOrEqual:    OpLessThanOrEqual,
		token.GreaterThanOrEqual: OpGreaterThanOrEqual,
		token.And:                OpAnd,
		token.Or:                 OpOr,
	}
	for {
		op := p.peek()
		precedence, isOp := precedences[op.Kind]
		if !isOp || precedence < minPrecedence {
			break
		}
		p.consumeAny()
		rhs, err := p.parseBinaryExpression(precedence)
		if err != nil {
			return nil, err
		}
		lhs = &BinaryExpression{nodeBase: p.newNodeBase(from), Op: ops[op.Kind], Lhs: lhs, Rhs: rhs}
	}
	return lhs, nil
}

// Parse an expression and then look at the next token to determine whether it's a
// member expression or call expression.
// Even though syntactically possible, we forbid some expressions from being callable
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
			t := p.peek()
			if t.Kind != token.Int && t.Kind != token.Ident && t.Kind != token.TypeIdent {
				return nil, errors.Errorf("expected identifier or integer literal after dot, got %s", t)
			}
			field := p.consumeAny()
			typeArgs, err := p.parseTypeArgs()
			if err != nil {
				return nil, err
			}
			expr = &MemberExpression{
				nodeBase: p.newNodeBase(from), Target: expr, Field: MemberExpressionField(field.Value), TypeArgs: typeArgs}
		case token.LParen:
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

func (p *Parser) parseIdentExpression(token token.Token) (*IdentExpression, error) {
	typeArgs, err := p.parseTypeArgs()
	if err != nil {
		return nil, err
	}
	return &IdentExpression{nodeBase: p.newNodeBase(token.Span), Ident: Ident(token.Value), TypeArgs: typeArgs}, nil
}

func (p *Parser) parseIntLiteralExpression() (*IntLiteralExpression, error) {
	strValue := ""
	if p.peek().Kind == token.Minus {
		p.consumeAny()
		strValue = "-"
	}
	strValue += p.consumeAny().Value
	int64Value, err := strconv.ParseInt(strValue, 10, 64)
	if err != nil {
		uint64Value, err2 := strconv.ParseUint(strValue, 10, 64)
		if err2 != nil {
			return nil, errors.Errorf("failed to parse int literal: %v", err)
		}
		return &IntLiteralExpression{nodeBase: p.newNodeBase(p.span()), UInt64: uint64Value, IsUInt64: true}, nil
	}
	return &IntLiteralExpression{nodeBase: p.newNodeBase(p.span()), Int64: int64Value, IsUInt64: false}, nil
}

func (p *Parser) parseCharLiteralExpression() (*CharLiteralExpression, error) {
	t, err := p.consume(token.Char)
	if err != nil {
		return nil, err
	}
	runes := []rune(t.Value)
	value := uint32(runes[0])
	return &CharLiteralExpression{nodeBase: p.newNodeBase(t.Span), Value: value}, nil
}

func (p *Parser) parsePrimaryExpression() (Expression, error) {
	from := p.span()
	t := p.peek()
	switch t.Kind {
	case token.Ident, token.TypeIdent:
		p.consumeAny()
		return p.parseIdentExpression(t)
	case token.Self:
		p.consumeAny()
		return &IdentExpression{nodeBase: p.newNodeBase(from), Ident: Ident("self")}, nil
	case token.Str:
		p.consumeAny()
		return &StringLiteralExpression{nodeBase: p.newNodeBase(from), Value: t.Value}, nil
	case token.Char:
		return p.parseCharLiteralExpression()
	case token.Int:
		return p.parseIntLiteralExpression()
	case token.Minus:
		t1 := p.peek1()
		if t1.Kind == token.Int {
			return p.parseIntLiteralExpression()
		}
	case token.True:
		p.consumeAny()
		return &BoolLiteralExpression{nodeBase: p.newNodeBase(from), Value: true}, nil
	case token.False:
		p.consumeAny()
		return &BoolLiteralExpression{nodeBase: p.newNodeBase(from), Value: false}, nil
	case token.LCurly:
		return p.parseBlockExpression()
	case token.LParen:
		return p.parseTupleLiteralExpression()
	case token.If:
		return p.parseIfExpression()
	case token.Not:
		p.consumeAny()
		expr, err := p.parsePrimaryExpression()
		if err != nil {
			return nil, err
		}
		return &UnaryExpression{nodeBase: p.newNodeBase(from), Op: OpNot, Value: expr}, nil
	}
	return nil, errors.Errorf("expected expression, got token: %s", t)

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
	return &LoopStatement{nodeBase: p.newNodeBase(from), Body: body}, nil
}

func (p *Parser) parseNamedUnionDeclaration() (*UnionTypeDeclaration, error) {
	from := p.span()
	if _, err := p.consume(token.Union); err != nil {
		return nil, err
	}
	identToken, err := p.consume(token.TypeIdent)
	if err != nil {
		return nil, err
	}
	typeParams, err := p.parseTypeParams()
	if err != nil {
		return nil, err
	}
	if _, err = p.consume(token.Equal); err != nil {
		return nil, err
	}
	res, err := p.parseAnonymousUnionDeclaration()
	if err != nil {
		return nil, err
	}
	res.Name = Ident(identToken.Value)
	res.TypeParams = typeParams
	res.span = p.spanToHere(from)
	return res, nil
}

func (p *Parser) parseAnonymousUnionDeclaration() (*UnionTypeDeclaration, error) {
	from := p.span()
	variants := []UnionVariant{}
	for p.index < len(p.tokens) {
		t := p.peek()
		switch t.Kind {
		case token.Pipe:
			p.consumeAny()
		case token.TypeIdent:
			var variant UnionVariant
			if p.peek1().Kind == token.LParen {
				p.consumeAny()
				tupleType, err := p.parseTupleType()
				if err != nil {
					return nil, err
				}
				variant = UnionVariant{Kind: UnionVariantKindNamed, Named: NamedVariant{Name: Ident(t.Value), Type: tupleType}}
			} else {
				variantType, err := p.parseType()
				if err != nil {
					return nil, err
				}
				variant = UnionVariant{Kind: UnionVariantKindType, Type: variantType}
			}
			variants = append(variants, variant)
			if p.peek().Kind != token.Pipe {
				return &UnionTypeDeclaration{nodeBase: p.newNodeBase(from), Variants: variants}, nil
			}
		}
	}
	panic("unexpected end of file while parsing union")
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
	typeParams, err := p.parseTypeParams()
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
			return &StructTypeDeclaration{
				nodeBase: p.newNodeBase(from), Name: Ident(identToken.Value), Fields: fields, TypeParams: typeParams}, nil
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
	traitTypeArgs, err := p.parseTypeArgs()
	if err != nil {
		return nil, err
	}
	if p.peek().Kind == token.For {
		p.consumeAny()
		traitIdentToken, err := p.consume(token.TypeIdent)
		if err != nil {
			return nil, err
		}
		trait = target
		target = Ident(traitIdentToken.Value)
	} else if len(traitTypeArgs) > 0 {
		return nil, errors.Errorf("%s: type arguments are only allowed when implementing a trait", from)
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
			return &ImplDefinition{
				nodeBase: p.newNodeBase(from), Trait: trait, TraitTypeArgs: traitTypeArgs, Target: target, Methods: functions}, nil
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
	typeParams, err := p.parseTypeParams()
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
				nodeBase:    p.newNodeBase(from),
				Name:        Ident(typeIdentToken.Value),
				TypeParams:  typeParams,
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
			return &BreakStatement{nodeBase: p.newNodeBase(from)}, nil
		case token.Continue:
			p.consumeAny()
			return &ContinueStatement{nodeBase: p.newNodeBase(from)}, nil
		case token.Struct:
			return p.parseStructDeclaration()
		case token.Union:
			return p.parseNamedUnionDeclaration()
		case token.Impl:
			return p.parseImplDefinition()
		case token.Trait:
			return p.parseTraitDeclaration()
		case token.Ident, token.TypeIdent, token.LCurly, token.LParen, token.If, token.True, token.False, token.Str, token.Int, token.Self:
			return p.parseExpression()
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
			return &Module{nodeBase: p.newNodeBase(from), Name: moduleName, Nodes: nodes}, nil
		}
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, node)
	}
	return nil, errors.Errorf("unexpected end of file")
}

func Parse(tokens []token.Token, moduleName Ident, nodeCreator *NodeCreator) (*Module, error) {
	p := Parser{tokens: tokens, index: 0, nodeCreator: nodeCreator}
	return p.Parse(moduleName)
}
