package ast

import (
	"fmt"
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

type Expression interface {
	String() string
	Id() NodeId
}

type IdentExpression struct {
	node
	Name string
}

func (expr *IdentExpression) String() string {
	return fmt.Sprintf("IdentExpression(%s)", expr.Name)
}

type StringLiteralExpression struct {
	node
	Value string
}

func (expr *StringLiteralExpression) String() string {
	return fmt.Sprintf("StringLiteralExpression(%q)", expr.Value)
}

type BoolLiteralExpression struct {
	node
	Value bool
}

func (expr *BoolLiteralExpression) String() string {
	return fmt.Sprintf("BoolLiteralExpression(%s)", strconv.FormatBool(expr.Value))
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
}

func (expr *IfExpression) String() string {
	condition := strings.ReplaceAll(expr.Condition.String(), "\n", "\n    ")
	trueBody := strings.ReplaceAll(expr.TrueBody.String(), "\n", "\n    ")
	return fmt.Sprintf("IfExpression(\n    %s\n    %s\n)", condition, trueBody)
}

type Module struct {
	node
	Nodes []Node
}

func (m *Module) String() string {
	nodes := ""
	for _, node := range m.Nodes {
		nodes += "\n    "
		nodes += strings.ReplaceAll(node.String(), "\n", "\n    ")
	}
	return fmt.Sprintf("Module(%s\n)", nodes)
}

type FunctionArg struct {
	node
	Name string
	Type string
}

func (f *FunctionArg) String() string {
	return fmt.Sprintf("FunctionArg(%s, %s)", f.Name, f.Type)
}

type FunctionDefinition struct {
	node
	Name string
	Args []FunctionArg
	Body *BlockExpression
}

func (f *FunctionDefinition) String() string {
	body := strings.ReplaceAll(f.Body.String(), "\n", "\n    ")
	args := ""
	for _, arg := range f.Args {
		if args != "" {
			args += ", "
		}
		args += arg.String()
	}
	return fmt.Sprintf("FunctionDefinition(\n    %s(%s)\n    %s\n)", f.Name, args, body)
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
	return &IfExpression{node: p.newNode(), Condition: condition, TrueBody: trueBody}, nil
}

func (p *Parser) parseFunctionDefinition() (*FunctionDefinition, error) {
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
		argNameToken, err := p.consume(token.Ident)
		if err != nil {
			return nil, err
		}
		argTypeToken, err := p.consume(token.Ident)
		if err != nil {
			return nil, err
		}
		arg := FunctionArg{node: p.newNode(), Name: argNameToken.Value, Type: argTypeToken.Value}
		args = append(args, arg)
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
	body, err := p.parseBlockExpression()
	if err != nil {
		return nil, err
	}
	return &FunctionDefinition{node: p.newNode(), Name: nameToken.Value, Args: args, Body: body}, nil
}

func (p *Parser) parseExpression() (Expression, error) {
	t := p.peek()
	switch t.Kind {
	case token.Ident:
		if _, err := p.consume(token.Ident); err != nil {
			return nil, err
		}
		expr := &IdentExpression{node: p.newNode(), Name: t.Value}
		if p.peek().Kind == token.LParen {
			expr, err := p.parseCallExpression(expr)
			if err != nil {
				return nil, err
			}
			return expr, nil
		}
		return expr, nil
	case token.Str:
		p.consumeAny()
		return &StringLiteralExpression{node: p.newNode(), Value: t.Value}, nil
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

var EOF = fmt.Errorf("EOF")

func (p *Parser) ParseNode() (Node, error) {
	for p.index < len(p.tokens) {
		t := p.peek()
		switch t.Kind {
		case token.EOF:
			return nil, EOF
		case token.Fn:
			return p.parseFunctionDefinition()
		case token.Ident, token.LCurly, token.If, token.True, token.False:
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

func (p *Parser) Parse() (*Module, error) {
	nodes := []Node{}
	for p.index < len(p.tokens) {
		node, err := p.ParseNode()
		if err == EOF {
			if len(nodes) == 0 {
				return nil, fmt.Errorf("expected at least one AST node")
			}
			return &Module{node: p.newNode(), Nodes: nodes}, nil
		}
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, node)
	}
	return nil, fmt.Errorf("unexpected end of file")
}

func Parse(tokens []token.Token) (*Module, error) {
	p := Parser{tokens: tokens, index: 0, nodeId: 0}
	return p.Parse()
}
