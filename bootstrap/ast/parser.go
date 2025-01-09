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

func (self *NodeCreator) NewSignedIntLiteralExpression(value int64, span token.Span) *IntLiteralExpression {
	return &IntLiteralExpression{nodeBase: self.newNodeBase(span), Int64: value, IsUInt64: false}
}

func (self *NodeCreator) NewBoolLiteralExpression(value bool, span token.Span) *BoolLiteralExpression {
	return &BoolLiteralExpression{nodeBase: self.newNodeBase(span), Value: value}
}

func (self *NodeCreator) NewIfExpression(condition Expression, trueBody *BlockExpression, falseBody *BlockExpression, span token.Span) *IfExpression {
	return &IfExpression{nodeBase: self.newNodeBase(span), Condition: condition, TrueBody: trueBody, FalseBody: falseBody}
}

func (self *NodeCreator) NewMemberExpression(target Expression, field MemberExpressionField, span token.Span) *MemberExpression {
	return &MemberExpression{nodeBase: self.newNodeBase(span), Target: target, Field: field}
}

func (self *NodeCreator) NewBinaryExpression(lhs Expression, op BinaryOperator, rhs Expression, span token.Span) *BinaryExpression {
	return &BinaryExpression{nodeBase: self.newNodeBase(span), Lhs: lhs, Op: op, Rhs: rhs}
}

func (self *NodeCreator) NewBlockExpression(nodes []Node, span token.Span) *BlockExpression {
	return &BlockExpression{nodeBase: self.newNodeBase(span), Nodes: nodes}
}

func (self *NodeCreator) NewVariableDefinition(
	name Ident, ty Type, mutable bool, value Expression, span token.Span) *VariableDefinition {
	return &VariableDefinition{
		nodeBase: self.newNodeBase(span), Name: name, Type: ty, Mutable: mutable, Value: value}
}

func (self *NodeCreator) NewTupleLiterarExpression(values []Expression, span token.Span) *TupleLiteralExpression {
	return &TupleLiteralExpression{nodeBase: self.newNodeBase(span), Values: values}
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

type FunctionTypeParam struct {
	Type    Type
	Mutable bool
}

func (self FunctionTypeParam) String() string {
	mutable := ""
	if self.Mutable {
		mutable = "mut "
	}
	return fmt.Sprintf("%s%s", mutable, self.Type)
}

type FunctionType struct {
	nodeBase
	TypeParams []TypeParam
	Params     []FunctionTypeParam
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
		params += param.String()
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

func (expr IntLiteralExpression) String() string {
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

type ArrayLiteralExpression struct {
	nodeBase
	Values []Expression
}

func (self *ArrayLiteralExpression) String() string {
	return fmt.Sprintf("ArrayLiteralExpression%s", base.IndentSlice(self.Values, 1))
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

type IndexExpression struct {
	nodeBase
	Target Expression
	Index  Expression
}

func (expr *IndexExpression) String() string {
	return fmt.Sprintf("IndexExpression\n%s\n%s", base.Indent(expr.Target, 1), base.Indent(expr.Index, 1))
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
	OpBitwiseAnd         BinaryOperator = "&"
	OpBitwiseOr          BinaryOperator = "|"
	OpBitwiseXor         BinaryOperator = "^"
	OpBitwiseShiftLeft   BinaryOperator = "<<"
	OpBitwiseShiftRight  BinaryOperator = ">>"
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
	OpNot        UnaryOperator = "not"
	OpBitwiseNot UnaryOperator = "~"
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

type MatchExpression struct {
	nodeBase
	Expression Expression
	Arms       []*MatchArm
}

func (self *MatchExpression) String() string {
	return fmt.Sprintf("MatchExpression%s", base.IndentSlice(self.Arms, 1))
}

type MatchArm struct {
	nodeBase
	Pattern MatchPattern
	Alias   *IdentExpression // Optional
	Body    *BlockExpression
}

func (self *MatchArm) String() string {
	alias := ""
	if self.Alias != nil {
		alias = fmt.Sprintf("\n    (Alias)\n%s", base.Indent(self.Alias, 2))
	}
	return fmt.Sprintf("MatchArm\n%s%s\n%s", base.Indent(self.Pattern, 1), alias, base.Indent(self.Body, 1))
}

type MatchPattern interface {
	Node
	String() string
	matchPattern()
}

type matchPatternBase struct {
	nodeBase
}

func (self matchPatternBase) matchPattern() {}

func (self matchPatternBase) Span() token.Span {
	return self.span
}

type UnionTypePattern struct {
	matchPatternBase
	Type         Type  // Optional, can be nil
	NamedVariant Ident // Optional, can be ""
}

func (self UnionTypePattern) String() string {
	ident := ""
	if self.NamedVariant != "" {
		ident = fmt.Sprintf("\n%s", base.Indent(self.NamedVariant, 1))
	}
	ty := ""
	if self.Type != nil {
		ty = fmt.Sprintf("\n%s", base.Indent(self.Type, 1))
	}
	return fmt.Sprintf("UnionTypePattern%s%s", ty, ident)
}

type IntPattern struct {
	matchPatternBase
	Value IntLiteralExpression
}

func (self IntPattern) String() string {
	return fmt.Sprintf("IntPattern\n%s", base.Indent(self.Value, 1))
}

type IntRangePattern struct {
	matchPatternBase
	From        IntLiteralExpression
	To          IntLiteralExpression
	InclusiveTo bool
}

func (self IntRangePattern) String() string {
	inclusive := " (exclusive)"
	if self.InclusiveTo {
		inclusive = " (inclusive)"
	}
	return fmt.Sprintf("IntRangePattern\n%s\n%s%s", base.Indent(self.From, 1), base.Indent(self.To, 1), inclusive)
}

type CharRangePattern struct {
	matchPatternBase
	From        CharLiteralExpression
	To          CharLiteralExpression
	InclusiveTo bool
}

func (self CharRangePattern) String() string {
	inclusive := " (exclusive)"
	if self.InclusiveTo {
		inclusive = " (inclusive)"
	}
	return fmt.Sprintf("CharRangePattern\n%s\n%s%s", base.Indent(&self.From, 1), base.Indent(&self.To, 1), inclusive)
}

type StrPattern struct {
	matchPatternBase
	Value StringLiteralExpression
}

func (self StrPattern) String() string {
	return fmt.Sprintf("StrPattern\n%s", base.Indent(&self.Value, 1))
}

type CharPattern struct {
	matchPatternBase
	Value CharLiteralExpression
}

func (self CharPattern) String() string {
	return fmt.Sprintf("CharPattern\n%s", base.Indent(&self.Value, 1))
}

type WildcardPattern struct {
	matchPatternBase
}

func (self WildcardPattern) String() string {
	return "WildcardPattern"
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

type ReturnStatement struct {
	nodeBase
	Value Expression
}

func (r *ReturnStatement) String() string {
	return fmt.Sprintf("ReturnStatement\n%s", base.Indent(r.Value, 1))
}

type AssignmentStatement struct {
	nodeBase
	// Can either be an IdentExpression, MemberExpression, or IndexExpression.
	Target Expression
	Value  Expression
}

func (a *AssignmentStatement) String() string {
	return fmt.Sprintf("AssignmentStatement\n%s\n%s", base.Indent(a.Target, 1), base.Indent(a.Value, 1))
}

func (a *AssignmentStatement) VariableExpr() *IdentExpression {
	switch target := a.Target.(type) {
	case *IdentExpression:
		return target
	case *MemberExpression:
		return a.Target.(*MemberExpression).Target.(*IdentExpression)
	case *IndexExpression:
		return a.Target.(*IndexExpression).Target.(*IdentExpression)
	default:
		panic(fmt.Sprintf("unexpected target type: %T", a.Target))
	}
}

func (a *AssignmentStatement) Variable() Ident {
	return a.VariableExpr().Ident
}

func (a *AssignmentStatement) IsMemberAssigment() (Ident, bool) {
	if member, ok := a.Target.(*MemberExpression); ok {
		return member.Field.AsIdent(), true
	}
	return "", false
}

func (a *AssignmentStatement) IsIndexAssigment() (*IndexExpression, bool) {
	if index, ok := a.Target.(*IndexExpression); ok {
		return index, true
	}
	return nil, false
}

func (a *AssignmentStatement) IsDirectAssigment() bool {
	_, ok := a.Target.(*IdentExpression)
	return ok
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

type UnionType struct {
	nodeBase
	Variants []UnionVariant
}

func (self UnionType) String() string {
	return fmt.Sprintf("UnionType%s", base.IndentSlice(self.Variants, 1))
}

func (self UnionType) TypeName() string {
	s := ""
	for i, variant := range self.Variants {
		if i > 0 {
			s += "|"
		}
		if variant.Kind == UnionVariantKindNamed {
			s += variant.Named.Name.String()
		} else {
			s += variant.Type.TypeName()
		}
	}
	return s
}

type NamedUnionTypeDeclaration struct {
	nodeBase
	Name       Ident
	TypeParams []TypeParam
	UnionType  UnionType
}

func (self NamedUnionTypeDeclaration) String() string {
	return fmt.Sprintf("NamedUnionTypeDeclaration\n%s%s%s", base.Indent(self.Name, 1), base.IndentString(typeParamsString(self.TypeParams), 1), base.Indent(self.UnionType, 1))
}

type StructTypeField struct {
	Name    Ident
	Type    Type
	Mutable bool
	Span    token.Span
}

func (f StructTypeField) String() string {
	mutable := ""
	if f.Mutable {
		mutable = "\n    (mutable)"
	}
	return fmt.Sprintf("%s%s\n%s", f.Name, mutable, base.Indent(f.Type, 1))
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
	Name    Ident
	Type    Type
	Mutable bool
	Span    token.Span
}

func (f FunctionParam) String() string {
	mutable := ""
	if f.Mutable {
		mutable = "\n    (mutable)"
	}
	return fmt.Sprintf("%s%s\n%s", f.Name, mutable, base.Indent(f.Type, 1))
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
	mutable := ""
	if v.Mutable {
		mutable = "\n    (mutable)"
	}
	ty := ""
	if v.Type != nil {
		ty = fmt.Sprintf("\n(Type)\n%s", base.Indent(v.Type, 1))
	}
	return fmt.Sprintf(
		"VariableDefinition\n%s%s%s\n%s",
		base.Indent(v.Name, 1), mutable, base.IndentString(ty, 1), base.Indent(v.Value, 1))
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

func (p *Parser) isStartOfTypeParamOrArgList(tokenBefore token.Token) bool {
	t := p.peek()
	if t.Kind != token.LAngle {
		return false
	}
	if !tokenBefore.Immediate(t) {
		// The opening `<` must immediately follow the previous token, i.e. `Foo <Int>` is invalid.
		return false
	}
	t1 := p.peek1()
	if t1.Kind == token.LAngle {
		// This is actually a bitwise shift left.
		return false
	}
	if !t.Immediate(t1) {
		// The next token must follow immediately, i.e. no whitespace is allowed
		// between the opening `<` and the next token.
		return false
	}
	return true
}

func (p *Parser) parseTypeArgs(tokenBefore token.Token) ([]Type, error) {
	if !p.isStartOfTypeParamOrArgList(tokenBefore) {
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

func (p *Parser) parseMatchExpression() (*MatchExpression, error) {
	from := p.span()
	if _, err := p.consume(token.Match); err != nil {
		return nil, err
	}
	expression, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	if _, err := p.consume(token.LCurly); err != nil {
		return nil, err
	}
	arms := []*MatchArm{}
	for {
		switch p.peek().Kind {
		case token.RCurly:
			p.consumeAny()
			if len(arms) == 0 {
				return nil, errors.Errorf("match expression must have at least one arm")
			}
			return &MatchExpression{nodeBase: p.newNodeBase(from), Expression: expression, Arms: arms}, nil
		case token.Case:
			arm, err := p.parseMatchArm()
			if err != nil {
				return nil, err
			}
			arms = append(arms, arm)
		default:
			return nil, errors.Errorf("expected case or close curly, got %s", p.peek())
		}
	}

}

func (p *Parser) parseMatchArm() (*MatchArm, error) {
	from := p.span()
	if _, err := p.consume(token.Case); err != nil {
		return nil, err
	}
	pattern, err := p.parseMatchPattern()
	if err != nil {
		return nil, err
	}
	var alias *IdentExpression = nil
	if p.peek().Kind == token.As {
		p.consumeAny()
		identExpression, err := p.parseIdentExpression(p.consumeAny())
		if err != nil {
			return nil, err
		}
		alias = identExpression
	}
	body, err := p.parseBlockExpression()
	if err != nil {
		return nil, err
	}
	return &MatchArm{nodeBase: p.newNodeBase(from), Pattern: pattern, Alias: alias, Body: body}, nil
}

func (p *Parser) matchPatternBase(from token.Span) matchPatternBase {
	return matchPatternBase{nodeBase: p.newNodeBase(from)}
}

func (p *Parser) parseMatchPattern() (MatchPattern, error) {
	from := p.span()
	t := p.peek()
	switch t.Kind {
	case token.Underscore:
		p.consumeAny()
		return &WildcardPattern{matchPatternBase: p.matchPatternBase(from)}, nil
	case token.Dot:
		p.consumeAny()
		namedVariant_, err := p.consume(token.TypeIdent)
		if err != nil {
			return nil, err
		}
		namedVariant := Ident(namedVariant_.Value)
		return &UnionTypePattern{matchPatternBase: p.matchPatternBase(from), Type: nil, NamedVariant: namedVariant}, nil
	case token.TypeIdent:
		ty, err := p.parseType()
		if err != nil {
			return nil, err
		}
		var namedVariant Ident = ""
		if p.peek().Kind == token.Dot {
			p.consumeAny()
			namedVariant_, err := p.consume(token.TypeIdent)
			if err != nil {
				return nil, err
			}
			namedVariant = Ident(namedVariant_.Value)
		}
		return &UnionTypePattern{matchPatternBase: p.matchPatternBase(from), Type: ty, NamedVariant: namedVariant}, nil
	case token.Int:
		literal, err := p.parseIntLiteralExpression()
		if err != nil {
			return nil, err
		}
		t := p.peek()
		if t.Kind == token.ClosedRange || t.Kind == token.HalfOpenRange {
			p.consumeAny()
			to, err := p.parseIntLiteralExpression()
			if err != nil {
				return nil, err
			}
			return &IntRangePattern{
				matchPatternBase: p.matchPatternBase(from), From: *literal, To: *to, InclusiveTo: t.Kind == token.ClosedRange}, nil
		}
		return &IntPattern{matchPatternBase: p.matchPatternBase(from), Value: *literal}, nil
	case token.Str:
		literal, err := p.parseStringLiteralExpression()
		if err != nil {
			return nil, err
		}
		return &StrPattern{matchPatternBase: p.matchPatternBase(from), Value: *literal}, nil
	case token.Char:
		literal, err := p.parseCharLiteralExpression()
		if err != nil {
			return nil, err
		}
		t := p.peek()
		if t.Kind == token.ClosedRange || t.Kind == token.HalfOpenRange {
			p.consumeAny()
			to, err := p.parseCharLiteralExpression()
			if err != nil {
				return nil, err
			}
			return &CharRangePattern{
				matchPatternBase: p.matchPatternBase(from), From: *literal, To: *to, InclusiveTo: t.Kind == token.ClosedRange}, nil
		}
		return &CharPattern{matchPatternBase: p.matchPatternBase(from), Value: *literal}, nil
	}
	return nil, errors.Errorf("expected a pattern but got %q", t)
}

func (p *Parser) parseTupleLiteralExpression() (*TupleLiteralExpression, error) {
	from := p.span()
	if _, err := p.consume(token.LParen); err != nil {
		return nil, err
	}
	values, err := p.parseCommaSeparatedExpressionList(token.RParen)
	if err != nil {
		return nil, err
	}
	if len(values) == 0 {
		return nil, errors.Errorf("expected at least one value in tuple")
	}
	return &TupleLiteralExpression{nodeBase: p.newNodeBase(from), Values: values}, nil
}

func (p *Parser) parseArrayLiteralExpression() (*ArrayLiteralExpression, error) {
	from := p.span()
	if _, err := p.consume(token.LBracket); err != nil {
		return nil, err
	}
	values, err := p.parseCommaSeparatedExpressionList(token.RBracket)
	if err != nil {
		return nil, err
	}
	return &ArrayLiteralExpression{nodeBase: p.newNodeBase(from), Values: values}, nil
}

func (p *Parser) parseCommaSeparatedExpressionList(closingTokenKind token.TokenKind) ([]Expression, error) {
	expressions := []Expression{}
	done := false
	for p.index < len(p.tokens) && !done {
		expr, err := p.parseExpression()
		if err != nil {
			return nil, err
		}
		expressions = append(expressions, expr)
		switch p.peek().Kind {
		case token.Comma:
			p.consumeAny()
		case closingTokenKind:
			p.consumeAny()
			done = true
		default:
			return nil, errors.Errorf("expected comma or closing token (%s), got %s", closingTokenKind, p.peek())
		}
	}
	return expressions, nil
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
	params := []FunctionTypeParam{}
	for p.index < len(p.tokens) {
		if p.peek().Kind == token.RParen {
			break
		}
		mutable := false
		if p.peek().Kind == token.Mut {
			p.consumeAny()
			mutable = true
		}
		paramType, err := p.parseType()
		if err != nil {
			return nil, err
		}
		params = append(params, FunctionTypeParam{Type: paramType, Mutable: mutable})
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
			p.consumeAny()
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
	return p.parseType_(true)
}

func (p *Parser) parseType_(parseAnonymousUnionType bool) (Type, error) {
	t := p.peek()
	switch t.Kind {
	case token.TypeIdent:
		beforeTypeArgs := p.consumeAny()
		typeArgs, err := p.parseTypeArgs(beforeTypeArgs)
		if err != nil {
			return nil, err
		}
		if p.peek().Kind == token.Pipe && parseAnonymousUnionType {
			simpleType := &SimpleType{nodeBase: p.newNodeBase(p.span()), Name: Ident(t.Value)}
			unionType, err := p.parseAnonymousUnionType()
			if err != nil {
				return nil, err
			}
			unionType.Variants = append(
				[]UnionVariant{{Kind: UnionVariantKindType, Type: simpleType}}, unionType.Variants...)
			return unionType, nil
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

func (p *Parser) parseTypeParams(tokenBefore token.Token) ([]TypeParam, error) {
	if !p.isStartOfTypeParamOrArgList(tokenBefore) {
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
	typeParams, err := p.parseTypeParams(nameToken)
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
		mutable := false
		if t.Kind == token.Mut {
			mutable = true
			p.consumeAny()
		}
		from := p.span()
		paramNameToken := p.consumeAny()
		if paramNameToken.Kind == token.Ident {
			paramName := Ident(paramNameToken.Value)
			paramType, err := p.parseType()
			if err != nil {
				return nil, err
			}
			param := FunctionParam{Name: paramName, Type: paramType, Mutable: mutable, Span: p.spanToHere(t.Span)}
			params = append(params, param)
		} else if paramNameToken.Kind == token.Self {
			if !acceptSelfParameter {
				return nil, errors.Errorf("self parameter not allowed here")
			}
			if len(params) > 0 {
				return nil, errors.Errorf("self parameter must be the first parameter")
			}
			selfType := &SimpleType{nodeBase: p.newNodeBase(from), Name: "Self"}
			param := FunctionParam{Name: Ident("self"), Type: selfType, Mutable: mutable, Span: p.spanToHere(t.Span)}
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
		return &AssignmentStatement{nodeBase: p.newNodeBase(from), Target: lhs, Value: rhs}, nil
	case *MemberExpression:
		if _, ok := lhs.Target.(*IdentExpression); !ok {
			return nil, errors.Errorf("%s: cannot assign to a member expression that does not target an identifier", lhs.span)
		}
		if lhs.Field.IsIndex() {
			return nil, errors.Errorf("%s: cannot assign to an index", lhs.span)
		}
		return &AssignmentStatement{nodeBase: p.newNodeBase(from), Target: lhs, Value: rhs}, nil
	case *IndexExpression:
		if _, ok := lhs.Target.(*IdentExpression); !ok {
			return nil, errors.Errorf("%s: cannot assign to an index expression that does not target an identifier", lhs.span)
		}
		return &AssignmentStatement{nodeBase: p.newNodeBase(from), Target: lhs, Value: rhs}, nil
	}
	return nil, errors.Errorf("expected IdentExpression, MemberExpression, or IndexExpression, got %s", lhs)
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
	precedences := map[BinaryOperator]int{
		OpOr:                 1,
		OpAnd:                2,
		OpEqual:              3,
		OpNotEqual:           3,
		OpLessThanOrEqual:    3,
		OpLessThan:           3,
		OpGreaterThan:        3,
		OpGreaterThanOrEqual: 3,
		OpBitwiseOr:          4,
		OpBitwiseXor:         5,
		OpBitwiseAnd:         6,
		OpBitwiseShiftLeft:   7,
		OpBitwiseShiftRight:  7,
		OpAdd:                8,
		OpMultiply:           9,
		OpDivide:             9,
		OpModulo:             9,
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
		token.Pipe:               OpBitwiseOr,
		token.BitwiseXor:         OpBitwiseXor,
		token.BitwiseAnd:         OpBitwiseAnd,
	}
	for {
		t := p.peek()
		op, isOp := ops[t.Kind]
		if !isOp {
			break
		}
		if t.Kind == token.LAngle {
			t1 := p.peek1()
			if t.Immediate(t1) {
				op = OpBitwiseShiftLeft
			}
		}
		if t.Kind == token.RAngle {
			t1 := p.peek1()
			if t.Immediate(t1) {
				op = OpBitwiseShiftRight
			}
		}
		precedence, ok := precedences[op]
		if !ok {
			panic(fmt.Sprintf("precedence not defined for operator %s", op))
		}
		if precedence < minPrecedence {
			break
		}
		p.consumeAny()
		if op == OpBitwiseShiftLeft || op == OpBitwiseShiftRight {
			p.consumeAny()
		}
		rhs, err := p.parseBinaryExpression(precedence)
		if err != nil {
			return nil, err
		}
		lhs = &BinaryExpression{nodeBase: p.newNodeBase(from), Op: op, Lhs: lhs, Rhs: rhs}
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
			typeArgs, err := p.parseTypeArgs(field)
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
		case token.LBracket:
			if is_forbidden_expression {
				return nil, errors.Errorf("block and if expressions cannot be indexed")
			}
			p.consumeAny()
			indexExpr, err := p.parseExpression()
			if err != nil {
				return nil, err
			}
			expr = &IndexExpression{nodeBase: p.newNodeBase(from), Target: expr, Index: indexExpr}
			if _, err := p.consume(token.RBracket); err != nil {
				return nil, err
			}
		default:
			return expr, nil
		}
	}
	panic("unreachable")
}

func (p *Parser) parseIdentExpression(token token.Token) (*IdentExpression, error) {
	typeArgs, err := p.parseTypeArgs(token)
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

func (p *Parser) parseStringLiteralExpression() (*StringLiteralExpression, error) {
	t, err := p.consume(token.Str)
	if err != nil {
		return nil, err
	}
	return &StringLiteralExpression{nodeBase: p.newNodeBase(t.Span), Value: t.Value}, nil
}

func (p *Parser) parseUnaryExpression() (*UnaryExpression, error) {
	t := p.consumeAny()
	var op UnaryOperator
	switch t.Kind {
	case token.Not:
		op = OpNot
	case token.BitwiseNot:
		op = OpBitwiseNot
	default:
		return nil, errors.Errorf("unexpected unary operator: %s", t)
	}
	expr, err := p.parsePrimaryExpression()
	if err != nil {
		return nil, err
	}
	return &UnaryExpression{nodeBase: p.newNodeBase(t.Span), Op: op, Value: expr}, nil
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
		return p.parseStringLiteralExpression()
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
	case token.LBracket:
		return p.parseArrayLiteralExpression()
	case token.If:
		return p.parseIfExpression()
	case token.Match:
		return p.parseMatchExpression()
	case token.Not, token.BitwiseNot:
		return p.parseUnaryExpression()
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

func (p *Parser) parseNamedUnionDeclaration() (*NamedUnionTypeDeclaration, error) {
	from := p.span()
	if _, err := p.consume(token.Union); err != nil {
		return nil, err
	}
	nameToken, err := p.consume(token.TypeIdent)
	if err != nil {
		return nil, err
	}
	typeParams, err := p.parseTypeParams(nameToken)
	if err != nil {
		return nil, err
	}
	if _, err = p.consume(token.Equal); err != nil {
		return nil, err
	}
	variants := []UnionVariant{}
	for p.index < len(p.tokens) {
		t := p.peek()
		switch t.Kind {
		case token.Pipe:
			p.consumeAny()
			continue
		case token.Dot:
			p.consumeAny()
			ident, err := p.consume(token.TypeIdent)
			if err != nil {
				return nil, err
			}
			var tupleType *TupleType
			if p.peek().Kind == token.LParen {
				tupleType, err = p.parseTupleType()
				if err != nil {
					return nil, err
				}
			} else {
				tupleType = &TupleType{}
			}
			variant := UnionVariant{Kind: UnionVariantKindNamed, Named: NamedVariant{Name: Ident(ident.Value), Type: tupleType}}
			variants = append(variants, variant)
		case token.TypeIdent:
			variantType, err := p.parseType_(false)
			if err != nil {
				return nil, err
			}
			variant := UnionVariant{Kind: UnionVariantKindType, Type: variantType}
			variants = append(variants, variant)
		default:
			return nil, errors.Errorf("unexpected token while parsing named union type: %s", t)
		}
		if p.peek().Kind != token.Pipe {
			break
		}
	}
	unionType := &UnionType{nodeBase: p.newNodeBase(from), Variants: variants}
	return &NamedUnionTypeDeclaration{
		nodeBase: p.newNodeBase(from), Name: Ident(nameToken.Value), TypeParams: typeParams, UnionType: *unionType}, nil
}

func (p *Parser) parseAnonymousUnionType() (*UnionType, error) {
	from := p.span()
	variants := []UnionVariant{}
	for p.index < len(p.tokens) {
		t := p.peek()
		switch t.Kind {
		case token.Pipe:
			p.consumeAny()
			continue
		case token.TypeIdent:
			variantType, err := p.parseType_(false)
			if err != nil {
				return nil, err
			}
			variant := UnionVariant{Kind: UnionVariantKindType, Type: variantType}
			variants = append(variants, variant)
		default:
			return nil, errors.Errorf("unexpected token while parsing anonymous union type: %s", t)
		}
		if p.peek().Kind != token.Pipe {
			break
		}
	}
	return &UnionType{nodeBase: p.newNodeBase(from), Variants: variants}, nil
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
	typeParams, err := p.parseTypeParams(identToken)
	if err != nil {
		return nil, err
	}
	if _, err = p.consume(token.LCurly); err != nil {
		return nil, err
	}
	fields := []StructTypeField{}
	for p.index < len(p.tokens) {
		mutable := false
		if p.peek().Kind == token.Mut {
			mutable = true
			p.consumeAny()
		}
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
			field := StructTypeField{Name: Ident(fieldName), Type: fieldType, Mutable: mutable, Span: p.spanToHere(from)}
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
	traitTypeArgs, err := p.parseTypeArgs(targetIdentToken)
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
	typeParams, err := p.parseTypeParams(typeIdentToken)
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

func (p *Parser) parseReturnStatement() (*ReturnStatement, error) {
	from := p.span()
	if _, err := p.consume(token.Return); err != nil {
		return nil, err
	}
	value, err := p.parseExpression()
	if err != nil {
		return nil, err
	}
	return &ReturnStatement{nodeBase: p.newNodeBase(from), Value: value}, nil
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
		case token.Return:
			return p.parseReturnStatement()
		case token.Struct:
			return p.parseStructDeclaration()
		case token.Union:
			return p.parseNamedUnionDeclaration()
		case token.Impl:
			return p.parseImplDefinition()
		case token.Trait:
			return p.parseTraitDeclaration()
		case token.Ident, token.TypeIdent, token.LCurly, token.LParen, token.If, token.Match, token.True, token.False, token.Str, token.Char, token.Int, token.Self:
			return p.parseExpression()
		default:
			return nil, errors.Errorf("%s: unexpected token: %s", t.Span, t)
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
