package token

import (
	"fmt"
	"unicode"
)

type Token struct {
	Kind  TokenKind
	Value string
}

type TokenKind string

const (
	Ident      TokenKind = "ident"
	TypeIdent  TokenKind = "typeident"
	LParen     TokenKind = "("
	RParen     TokenKind = ")"
	LCurly     TokenKind = "{"
	RCurly     TokenKind = "}"
	Comma      TokenKind = ","
	Dot        TokenKind = "."
	Plus       TokenKind = "+"
	Equal      TokenKind = "="
	EqualEqual TokenKind = "=="
	Str        TokenKind = "Str"
	Int        TokenKind = "Int"
	True       TokenKind = "true"
	False      TokenKind = "false"
	If         TokenKind = "if"
	Else       TokenKind = "else"
	Fn         TokenKind = "fn"
	Mut        TokenKind = "mut"
	Let        TokenKind = "let"
	Loop       TokenKind = "loop"
	Break      TokenKind = "break"
	Continue   TokenKind = "continue"
	Struct     TokenKind = "struct"
	EOF        TokenKind = "EOF"
)

func (t Token) String() string {
	kind := string(t.Kind)
	switch t.Kind {
	case Str:
		return fmt.Sprintf("%s(%q)", kind, t.Value)
	case Ident, TypeIdent:
		return fmt.Sprintf("%s(%s)", kind, t.Value)
	default:
		return kind
	}
}

func isTypeIdentifier(name string) bool {
	firstRune := []rune(name)[0]
	return unicode.IsUpper(firstRune)
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
			tokens = append(tokens, Token{Kind: LParen, Value: ""})
		} else if c == ')' {
			tokens = append(tokens, Token{Kind: RParen, Value: ""})
		} else if c == '{' {
			tokens = append(tokens, Token{Kind: LCurly, Value: ""})
		} else if c == '}' {
			tokens = append(tokens, Token{Kind: RCurly, Value: ""})
		} else if c == ',' {
			tokens = append(tokens, Token{Kind: Comma, Value: ""})
		} else if c == '+' {
			tokens = append(tokens, Token{Kind: Plus, Value: ""})
		} else if c == '.' {
			tokens = append(tokens, Token{Kind: Dot, Value: ""})
		} else if c == '=' {
			if src[i] == '=' {
				i += 1
				tokens = append(tokens, Token{Kind: EqualEqual, Value: ""})
			} else {
				tokens = append(tokens, Token{Kind: Equal, Value: ""})
			}
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
			tokens = append(tokens, Token{Kind: Str, Value: string(value)})
		} else if c >= '0' && c <= '9' {
			// Parse int.
			value := []byte{c}
			for i < len(src) {
				c = src[i]
				if c >= '0' && c <= '9' {
					i += 1
					value = append(value, c)
				} else {
					break
				}
			}
			tokens = append(tokens, Token{Kind: Int, Value: string(value)})
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
			var token Token
			switch string(value) {
			case "fn":
				token = Token{Kind: Fn, Value: ""}
			case "loop":
				token = Token{Kind: Loop, Value: ""}
			case "break":
				token = Token{Kind: Break, Value: ""}
			case "continue":
				token = Token{Kind: Continue, Value: ""}
			case "let":
				token = Token{Kind: Let, Value: ""}
			case "mut":
				token = Token{Kind: Mut, Value: ""}
			case "if":
				token = Token{Kind: If, Value: ""}
			case "else":
				token = Token{Kind: Else, Value: ""}
			case "true":
				token = Token{Kind: True, Value: ""}
			case "false":
				token = Token{Kind: False, Value: ""}
			case "struct":
				token = Token{Kind: Struct, Value: ""}
			default:
				kind := Ident
				if isTypeIdentifier(string(value)) {
					kind = TypeIdent
				}
				token = Token{Kind: kind, Value: string(value)}
			}
			tokens = append(tokens, token)
		} else {
			// Unexpected character.
			return tokens, fmt.Errorf("unexpected character: %c", c)
		}
	}
	tokens = append(tokens, Token{Kind: EOF, Value: ""})
	return tokens, nil
}
