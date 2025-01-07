package token

import (
	"fmt"
	"strconv"
	"unicode"

	"github.com/pkg/errors"
)

type Span struct {
	File  string
	Src   *[]byte
	Start int
	End   int
}

func (span Span) Pos() (row int, col int) {
	row = 1
	col = 1
	for i := 0; i < span.Start; i++ {
		if (*span.Src)[i] == '\n' {
			row++
			col = 1
		} else {
			col++
		}
	}
	return row, col
}

func (span Span) String() string {
	row, col := span.Pos()
	return fmt.Sprintf("%s:%d:%d", span.File, row, col)
}

type Token struct {
	Kind  TokenKind
	Value string
	Span  Span
}

func (self Token) Immediate(other Token) bool {
	return self.Span.End+1 == other.Span.Start
}

type TokenKind string

const (
	Ident              TokenKind = "Ident"
	TypeIdent          TokenKind = "TypeIdent"
	LParen             TokenKind = "("
	RParen             TokenKind = ")"
	LCurly             TokenKind = "{"
	RCurly             TokenKind = "}"
	LAngle             TokenKind = "<"
	RAngle             TokenKind = ">"
	LBracket           TokenKind = "["
	RBracket           TokenKind = "]"
	LessThanOrEqual    TokenKind = "<="
	GreaterThanOrEqual TokenKind = ">="
	Comma              TokenKind = ","
	Dot                TokenKind = "."
	ClosedRange        TokenKind = ".."
	HalfOpenRange      TokenKind = "..<"
	Plus               TokenKind = "+"
	Star               TokenKind = "*"
	Slash              TokenKind = "/"
	Percent            TokenKind = "%"
	Equal              TokenKind = "="
	NotEqual           TokenKind = "!="
	FatArrow           TokenKind = "=>"
	Pipe               TokenKind = "|"
	BitwiseAnd         TokenKind = "&"
	BitwiseXor         TokenKind = "^"
	BitwiseNot         TokenKind = "~"
	EqualEqual         TokenKind = "=="
	Str                TokenKind = "Str"
	Char               TokenKind = "Char"
	Int                TokenKind = "Int"
	True               TokenKind = "true"
	False              TokenKind = "false"
	If                 TokenKind = "if"
	Else               TokenKind = "else"
	Fn                 TokenKind = "fn"
	Mut                TokenKind = "mut"
	Let                TokenKind = "let"
	Loop               TokenKind = "loop"
	Break              TokenKind = "break"
	Continue           TokenKind = "continue"
	Struct             TokenKind = "struct"
	Impl               TokenKind = "impl"
	Trait              TokenKind = "trait"
	For                TokenKind = "for"
	Self               TokenKind = "self"
	Union              TokenKind = "union"
	Minus              TokenKind = "-"
	LineComment        TokenKind = "LineComment"
	EOF                TokenKind = "EOF"
	And                TokenKind = "and"
	Not                TokenKind = "not"
	Or                 TokenKind = "or"
	Match              TokenKind = "match"
	Case               TokenKind = "case"
	As                 TokenKind = "as"
	Underscore         TokenKind = "_"
	Return             TokenKind = "return"
)

func (t Token) String() string {
	kind := string(t.Kind)
	switch t.Kind {
	case Str:
		return fmt.Sprintf("%q", t.Value)
	case Char:
		return fmt.Sprintf("'%s'", t.Value)
	case Ident, TypeIdent, LineComment:
		return fmt.Sprintf("%s(%s)", kind, t.Value)
	default:
		return string(kind)
	}
}

func isTypeIdentifier(name string) bool {
	firstRune := []rune(name)[0]
	return unicode.IsUpper(firstRune)
}

func parseEscapeSequence(src []byte, insideStr bool) ([]byte, int, error) {
	c := src[0]
	switch c {
	case 'n':
		return []byte{'\n'}, 1, nil
	case 'r':
		return []byte{'\r'}, 1, nil
	case 't':
		return []byte{'\t'}, 1, nil
	case '0':
		return []byte{0}, 1, nil
	case '\\':
		return []byte{'\\'}, 1, nil
	case '\'':
		if insideStr {
			return nil, 0, errors.Errorf("invalid escape sequence: \\%c", c)
		}
		return []byte{'\''}, 1, nil
	case '"':
		if !insideStr {
			return nil, 0, errors.Errorf("invalid escape sequence: \\%c", c)
		}
		return []byte{'"'}, 1, nil
	case 'u':
		if len(src) < 2 {
			return nil, 0, errors.Errorf("invalid escape sequence: \\u")
		}
		if src[1] != '{' {
			return nil, 0, errors.Errorf("invalid escape sequence: \\u%c", src[1])
		}
		i := 2
		for ; i < len(src); i++ {
			if src[i] == '}' {
				break
			}
		}
		if i == len(src) {
			return nil, 0, errors.Errorf("invalid escape sequence: \\u{")
		}
		value, err := strconv.ParseInt(string(src[2:i]), 16, 32)
		if err != nil {
			return nil, 0, errors.Errorf("invalid escape sequence: \\u{%s}", string(src[2:i]))
		}
		if value < 0 || value > 0x10FFFF {
			return nil, 0, errors.Errorf("invalid escape sequence: \\u{%s}", string(src[2:i]))
		}
		return []byte(string(rune(value))), i + 1, nil
	default:
		return nil, 0, errors.Errorf("unknown escape sequence: \\%c", c)
	}

}

func Tokenize(src []byte, file string) ([]Token, error) {
	var tokens []Token
	var i = 0
	for i < len(src) {
		c := src[i]
		span := Span{file, &src, i, i}
		i += 1
		if c == ' ' || c == '\t' || c == '\n' || c == '\r' {
			// Skip whitespace.
		} else if c == '(' {
			tokens = append(tokens, Token{Kind: LParen, Value: "", Span: span})
		} else if c == ')' {
			tokens = append(tokens, Token{Kind: RParen, Value: "", Span: span})
		} else if c == '{' {
			tokens = append(tokens, Token{Kind: LCurly, Value: "", Span: span})
		} else if c == '}' {
			tokens = append(tokens, Token{Kind: RCurly, Value: "", Span: span})
		} else if c == '[' {
			tokens = append(tokens, Token{Kind: LBracket, Value: "", Span: span})
		} else if c == ']' {
			tokens = append(tokens, Token{Kind: RBracket, Value: "", Span: span})
		} else if c == '<' {
			kind := LAngle
			if src[i] == '=' {
				i += 1
				span.End += 1
				kind = LessThanOrEqual
			}
			tokens = append(tokens, Token{Kind: kind, Value: "", Span: span})
		} else if c == '>' {
			kind := RAngle
			if src[i] == '=' {
				i += 1
				span.End += 1
				kind = GreaterThanOrEqual
			}
			tokens = append(tokens, Token{Kind: kind, Value: "", Span: span})
		} else if c == ',' {
			tokens = append(tokens, Token{Kind: Comma, Value: "", Span: span})
		} else if c == '+' {
			tokens = append(tokens, Token{Kind: Plus, Value: "", Span: span})
		} else if c == '*' {
			tokens = append(tokens, Token{Kind: Star, Value: "", Span: span})
		} else if c == '/' {
			tokens = append(tokens, Token{Kind: Slash, Value: "", Span: span})
		} else if c == '%' {
			tokens = append(tokens, Token{Kind: Percent, Value: "", Span: span})
		} else if c == '_' {
			tokens = append(tokens, Token{Kind: Underscore, Value: "", Span: span})
		} else if c == '.' {
			kind := Dot
			if src[i] == '.' {
				i += 1
				span.End += 1
				kind = ClosedRange
				if src[i] == '<' {
					i += 1
					span.End += 1
					kind = HalfOpenRange
				}
			}
			tokens = append(tokens, Token{Kind: kind, Value: "", Span: span})
		} else if c == '|' {
			tokens = append(tokens, Token{Kind: Pipe, Value: "", Span: span})
		} else if c == '&' {
			tokens = append(tokens, Token{Kind: BitwiseAnd, Value: "", Span: span})
		} else if c == '^' {
			tokens = append(tokens, Token{Kind: BitwiseXor, Value: "", Span: span})
		} else if c == '~' {
			tokens = append(tokens, Token{Kind: BitwiseNot, Value: "", Span: span})
		} else if c == '-' {
			if src[i] == '-' {
				i += 1
				value := []byte{}
				for i < len(src) {
					c = src[i]
					if c != '\n' {
						i += 1
						value = append(value, c)
					} else {
						break
					}
				}
				span.End = i - 1
				tokens = append(tokens, Token{Kind: LineComment, Value: string(value), Span: span})
			} else {
				tokens = append(tokens, Token{Kind: Minus, Value: "", Span: span})
			}

		} else if c == '=' {
			if src[i] == '=' {
				i += 1
				span.End += 1
				tokens = append(tokens, Token{Kind: EqualEqual, Value: "", Span: span})
			} else if src[i] == '>' {
				i += 1
				span.End += 1
				tokens = append(tokens, Token{Kind: FatArrow, Value: "", Span: span})
			} else {
				tokens = append(tokens, Token{Kind: Equal, Value: "", Span: span})
			}
		} else if c == '!' && src[i] == '=' {
			i += 1
			span.End += 1
			tokens = append(tokens, Token{Kind: NotEqual, Value: "", Span: span})
		} else if c == '\'' {
			// Parse char.
			var value []byte
			c = src[i]
			if c == '\\' {
				i += 1
				value_, len, err := parseEscapeSequence(src[i:], false)
				if err != nil {
					return nil, err
				}
				value = value_
				i += len
			} else if c == '\'' {
				return nil, errors.Errorf("empty char literal")
			} else {
				value = []byte{c}
				i += 1
			}
			if src[i] != '\'' {
				return nil, errors.Errorf("invalid char literal")
			}
			i += 1
			span.End = i
			tokens = append(tokens, Token{Kind: Char, Value: string(value), Span: span})
		} else if c == '"' {
			// Parse string.
			value := []byte{}
			for i < len(src) {
				c = src[i]
				if c == '\\' {
					i += 1
					value_, len, err := parseEscapeSequence(src[i:], true)
					if err != nil {
						return nil, err
					}
					value = append(value, value_...)
					i += len
				} else if c != '"' {
					i += 1
					value = append(value, c)
				} else {
					i += 1 // Consume the closing '"'.
					break
				}
			}
			span.End = i
			tokens = append(tokens, Token{Kind: Str, Value: string(value), Span: span})
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
			span.End = i - 1
			tokens = append(tokens, Token{Kind: Int, Value: string(value), Span: span})
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
			span.End = i - 1
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
			case "impl":
				token = Token{Kind: Impl, Value: ""}
			case "trait":
				token = Token{Kind: Trait, Value: ""}
			case "for":
				token = Token{Kind: For, Value: ""}
			case "self":
				token = Token{Kind: Self, Value: ""}
			case "union":
				token = Token{Kind: Union, Value: ""}
			case "or":
				token = Token{Kind: Or, Value: ""}
			case "and":
				token = Token{Kind: And, Value: ""}
			case "not":
				token = Token{Kind: Not, Value: ""}
			case "match":
				token = Token{Kind: Match, Value: ""}
			case "case":
				token = Token{Kind: Case, Value: ""}
			case "as":
				token = Token{Kind: As, Value: ""}
			case "return":
				token = Token{Kind: Return, Value: ""}
			default:
				kind := Ident
				if isTypeIdentifier(string(value)) {
					kind = TypeIdent
				}
				token = Token{Kind: kind, Value: string(value)}
			}
			token.Span = span
			tokens = append(tokens, token)
		} else {
			// Unexpected character.
			return tokens, errors.Errorf("unexpected character: %c", c)
		}
	}
	tokens = append(tokens, Token{Kind: EOF, Value: "", Span: Span{file, &src, i, i}})
	return tokens, nil
}
