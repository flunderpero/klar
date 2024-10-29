package main

import (
	"fmt"
	"strconv"
	"strings"
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

type Parser struct {
	tokens []Token
	index  int
	nodeId NodeId
}

func (p *Parser) nextNodeId() NodeId {
	p.nodeId = p.nodeId + 1
	return p.nodeId
}

func (p *Parser) consume(kind TokenKind) error {
	token := p.tokens[p.index]
	if token.Kind != kind {
		return fmt.Errorf("Expected token kind %s, got %s", kind, token.Kind)
	}
	p.consumeAny()
	return nil
}

func (p *Parser) consumeAny() {
	p.index = p.index + 1
}

func (p *Parser) peek() Token {
	return p.tokens[p.index]
}

func (p *Parser) parseCallExpression(callee Expression) (*CallExpression, error) {
	if err := p.consume(TKOpenParen); err != nil {
		return nil, err
	}
	arg, err := p.parseExpression()
	if err != nil {
		return nil, fmt.Errorf("failed to parse call argument: %v", err)
	}
	if err = p.consume(TKCloseParen); err != nil {
		return nil, err
	}
	return &CallExpression{id: p.nextNodeId(), Callee: callee, Args: []Expression{arg}}, nil
}

func (p *Parser) parseBlockExpression() (*BlockExpression, error) {
	if err := p.consume(TKOpenCurly); err != nil {
		return nil, err
	}
	var nodes []Node
	for p.index < len(p.tokens) {
		token := p.peek()
		if token.Kind == TKCloseCurly {
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
	if err := p.consume(TKIf); err != nil {
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

func (p *Parser) parseExpression() (Expression, error) {
	token := p.peek()
	switch token.Kind {
	case TKIdentifier:
		if err := p.consume(TKIdentifier); err != nil {
			return nil, err
		}
		expr := &IdentExpression{id: p.nextNodeId(), Name: token.Value}
		if p.peek().Kind == TKOpenParen {
			expr, err := p.parseCallExpression(expr)
			if err != nil {
				return nil, err
			}
			return expr, nil
		}
		return expr, nil
	case TKString:
		p.consumeAny()
		return &StringLiteralExpression{id: p.nextNodeId(), Value: token.Value}, nil
	case TKTrue:
		p.consumeAny()
		return &BoolLiteralExpression{id: p.nextNodeId(), Value: true}, nil
	case TKFalse:
		p.consumeAny()
		return &BoolLiteralExpression{id: p.nextNodeId(), Value: false}, nil
	case TKOpenCurly:
		return p.parseBlockExpression()
	case TKIf:
		return p.parseIfExpression()
	default:
		return nil, fmt.Errorf("expected expression, got token: %s", token)
	}
}

var EOF = fmt.Errorf("EOF")

func (p *Parser) ParseNode() (Node, error) {
	for p.index < len(p.tokens) {
		token := p.peek()
		switch token.Kind {
		case TKEOF:
			return nil, EOF
		case TKIdentifier, TKOpenCurly, TKIf, TKTrue, TKFalse:
			expr, err := p.parseExpression()
			if err != nil {
				return nil, err
			}
			return expr, nil
		default:
			return nil, fmt.Errorf("unexpected token: %s", token)
		}
	}
	return nil, fmt.Errorf("unexpected end of file")
}

func (p *Parser) Parse() (Node, error) {
	nodes := []Node{}
	for p.index < len(p.tokens) {
		node, err := p.ParseNode()
		if err == EOF {
			if len(nodes) != 1 {
				return nil, fmt.Errorf("want: exactly one AST node, got: %d", len(nodes))
			}
			return nodes[0], nil
		}
		if err != nil {
			return nil, err
		}
		nodes = append(nodes, node)
	}
	return nil, fmt.Errorf("unexpected end of file")
}

func Parse(tokens []Token) (Node, error) {
	p := Parser{tokens: tokens, index: 0, nodeId: 0}
	return p.Parse()
}
