package main

import "fmt"

type Token struct {
	Kind  TokenKind
	Value string
}

type TokenKind int

const (
	TKIdentifier TokenKind = iota
	TKOpenParen
	TKCloseParen
	TKOpenCurly
	TKCloseCurly
	TKString
	TKEOF
)

func (k TokenKind) String() string {
	switch k {
	case TKIdentifier:
		return "ident"
	case TKOpenParen:
		return "oparen"
	case TKCloseParen:
		return "cparen"
	case TKOpenCurly:
		return "ocurly"
	case TKCloseCurly:
		return "ccurly"
	case TKString:
		return "string"
	case TKEOF:
		return "eof"
	default:
		return fmt.Sprintf("<unknown: %d>", k)
	}
}

func (t Token) String() string {
	kind := t.Kind.String()
	switch t.Kind {
	case TKString:
		return fmt.Sprintf("%s(%q)", kind, t.Value)
	case TKIdentifier:
		return fmt.Sprintf("%s(%s)", kind, t.Value)
	default:
		return kind
	}
}

func Tokenize(src []byte, file string) ([]Token, error) {
	var tokens []Token
	var i = 0
	for i < len(src) {
		c := src[i]
		i += 1
		if c == ' ' || c == '\t' || c == '\n' || c == '\r' {
			// Skip whitespace.
		} else if c == '(' {
			tokens = append(tokens, Token{Kind: TKOpenParen, Value: ""})
		} else if c == ')' {
			tokens = append(tokens, Token{Kind: TKCloseParen, Value: ""})
		} else if c == '{' {
			tokens = append(tokens, Token{Kind: TKOpenCurly, Value: ""})
		} else if c == '}' {
			tokens = append(tokens, Token{Kind: TKCloseCurly, Value: ""})
		} else if c == '"' {
			// Parse string.
			value := []byte{}
			for i < len(src) {
				c = src[i]
				if c != '"' {
					i += 1
					value = append(value, c)
				} else {
					i += 1 // Consume the closing '"'.
					break
				}
			}
			tokens = append(tokens, Token{Kind: TKString, Value: string(value)})
		} else if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') {
			// Parse identifier.
			value := []byte{c}
			for i < len(src) {
				c = src[i]
				if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' {
					i += 1
					value = append(value, c)
				} else {
					break
				}
			}
			tokens = append(tokens, Token{Kind: TKIdentifier, Value: string(value)})
		} else {
			// Unexpected character.
			return tokens, fmt.Errorf("unexpected character: %c", c)
		}
	}
	tokens = append(tokens, Token{Kind: TKEOF, Value: ""})
	return tokens, nil
}
