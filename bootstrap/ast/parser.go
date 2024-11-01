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

type Expression interface {
	String() string
	Id() NodeId
}

type IdentExpression struct {
	id   NodeId
	Name string
}

func (expr *IdentExpression) Id() NodeId {
	return expr.id
}

func (expr *IdentExpression) String() string {
	return fmt.Sprintf("IdentExpression(%s)", expr.Name)
}

type StringLiteralExpression struct {
	id    NodeId
	Value string
}

func (expr *StringLiteralExpression) Id() NodeId {
	return expr.id
}

func (expr *StringLiteralExpression) String() string {
	return fmt.Sprintf("StringLiteralExpression(%q)", expr.Value)
}

type BoolLiteralExpression struct {
	id    NodeId
	Value bool
}

func (expr *BoolLiteralExpression) Id() NodeId {
	return expr.id
}

func (expr *BoolLiteralExpression) String() string {
	return fmt.Sprintf("BoolLiteralExpression(%s)", strconv.FormatBool(expr.Value))
}

type CallExpression struct {
	id     NodeId
	Callee Expression
	Args   []Expression
}

func (expr *CallExpression) Id() NodeId {
	return expr.id
}

func (expr *CallExpression) String() string {
	return fmt.Sprintf("CallExpression(%s, %v)", expr.Callee, expr.Args)
}

type BlockExpression struct {
	id    NodeId
	Nodes []Node
}

func (expr *BlockExpression) Id() NodeId {
	return expr.id
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
	id        NodeId
	Condition Expression
	TrueBody  *BlockExpression
}

func (expr *IfExpression) Id() NodeId {
	return expr.id
}

func (expr *IfExpression) String() string {
	condition := strings.ReplaceAll(expr.Condition.String(), "\n", "\n    ")
	trueBody := strings.ReplaceAll(expr.TrueBody.String(), "\n", "\n    ")
	return fmt.Sprintf("IfExpression(\n    %s\n    %s\n)", condition, trueBody)
}

type Module struct {
	id    NodeId
	Nodes []Node
}

func (m *Module) Id() NodeId {
	return m.id
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
	id   NodeId
	Name string
	Type string
}

func (f *FunctionArg) Id() NodeId {
	return f.id
}

func (f *FunctionArg) String() string {
	return fmt.Sprintf("FunctionArg(%s, %s)", f.Name, f.Type)
}

type FunctionDefinition struct {
	id   NodeId
	Name string
	Args []FunctionArg
	Body *BlockExpression
}

func (f *FunctionDefinition) Id() NodeId {
	return f.id
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

func (p *Parser) nextNodeId() NodeId {
	p.nodeId = p.nodeId + 1
	return p.nodeId
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
	return &CallExpression{id: p.nextNodeId(), Callee: callee, Args: args}, nil
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
	return &BlockExpression{id: p.nextNodeId(), Nodes: nodes}, nil
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
	return &IfExpression{id: p.nextNodeId(), Condition: condition, TrueBody: trueBody}, nil
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
		arg := FunctionArg{id: p.nextNodeId(), Name: argNameToken.Value, Type: argTypeToken.Value}
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
	return &FunctionDefinition{id: p.nextNodeId(), Name: nameToken.Value, Args: args, Body: body}, nil
}

func (p *Parser) parseExpression() (Expression, error) {
	t := p.peek()
	switch t.Kind {
	case token.Ident:
		if _, err := p.consume(token.Ident); err != nil {
			return nil, err
		}
		expr := &IdentExpression{id: p.nextNodeId(), Name: t.Value}
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
		return &StringLiteralExpression{id: p.nextNodeId(), Value: t.Value}, nil
	case token.True:
		p.consumeAny()
		return &BoolLiteralExpression{id: p.nextNodeId(), Value: true}, nil
	case token.False:
		p.consumeAny()
		return &BoolLiteralExpression{id: p.nextNodeId(), Value: false}, nil
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
			return &Module{id: p.nextNodeId(), Nodes: nodes}, nil
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
