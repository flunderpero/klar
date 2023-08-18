use std::fmt::{Display, Formatter, self};
use std::rc::Rc;
use std::str::FromStr;
use strum_macros::{EnumString, IntoStaticStr};

#[derive(Debug, PartialEq, Clone, EnumString, IntoStaticStr)]
pub enum TokenKind {
    #[strum(disabled)]
    Keyword(Keywords),
    #[strum(disabled)]
    Operator(Operators),
    #[strum(disabled)]
    Ident(String),
    #[strum(disabled)]
    Comment(String),
    #[strum(disabled)]
    Error(String),
    #[strum(disabled)]
    Whitespace(String),
    // Literals:
    #[strum(disabled)]
    Str(String),
    #[strum(disabled)]
    FormatStr(String),
    #[strum(disabled)]
    Int(String),
    #[strum(disabled)]
    Float(String),
    // Punctuation:
    #[strum(serialize = "(")]
    LParen,
    #[strum(serialize = ")")]
    RParen,
    #[strum(serialize = "{")]
    LBrace,
    #[strum(serialize = "}")]
    RBrace,
    #[strum(serialize = "[")]
    LBracket,
    #[strum(serialize = "]")]
    RBracket,
    #[strum(serialize = ":")]
    Colon,
    #[strum(serialize = ";")]
    Comma,
    #[strum(serialize = ",")]
    Dot,
    #[strum(serialize = "&")]
    Ampersand,
    #[strum(serialize = "=>")]
    FatArrow,
}
use TokenKind::*;

#[derive(Debug, PartialEq, Clone, EnumString, IntoStaticStr)]
pub enum Operators {
    #[strum(serialize = "+")]
    Plus,
    #[strum(serialize = "-")]
    Minus,
    #[strum(serialize = "*")]
    Star,
    #[strum(serialize = "/")]
    Slash,
    #[strum(serialize = "=")]
    Equals,
    #[strum(serialize = "==")]
    DoubleEquals,
    #[strum(serialize = "!=")]
    NotEquals,
    #[strum(serialize = "<")]
    LessThan,
    #[strum(serialize = "<=")]
    LessThanOrEqual,
    #[strum(serialize = ">")]
    GreaterThan,
    #[strum(serialize = ">=")]
    GreaterThanOrEqual,
}
use Operators::*;

#[derive(Debug, PartialEq, Clone, EnumString, IntoStaticStr)]
#[strum(serialize_all = "lowercase")]
pub enum Keywords {
    Let,
    Mut,
    If,
    Else,
    Match,
    While,
    For,
    Return,
    Fn,
    True,
    False,
    End,
    Struct,
    Enum,
    Trait,
    Impl,
    #[strum(serialize = "self")]
    Self_,
    Unsafe,
    And,
    Or,
    Not,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SourceMap {
    pub file_name: &'static str,
    pub source: &'static str,
    pub lines: Vec<usize>,
}

impl SourceMap {
    fn new(file_name: &'static str, source: &'static str) -> SourceMap {
        let mut lines = vec![0];
        for (i, c) in source.chars().enumerate() {
            if c == '\n' {
                lines.push(i + 1);
            }
        }
        SourceMap { file_name, source, lines }
    }

    fn get_line_column(&self, index: usize) -> (usize, usize) {
        let mut line = 0;
        for (i, l) in self.lines.iter().enumerate() {
            if index >= *l {
                line = i;
            } else {
                break;
            }
        }
        let column = index - self.lines[line];
        (line + 1, column + 1)
    }
}

#[derive(PartialEq, Clone)]
pub struct Span {
    pub source_map: Rc<SourceMap>,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(source_map: Rc<SourceMap>, start: usize, end: usize) -> Span {
        Span {
            source_map,
            start,
            end,
        }
    }

    pub fn merge(&self, other: &Span) -> Span {
        Span {
            source_map: self.source_map.clone(),
            start: self.start,
            end: other.end,
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.start == self.end {
            let (line, column) = self.source_map.get_line_column(self.start);
            write!(f, "{}#{}:{}", self.source_map.file_name, line, column)
        } else {
            let (start_line, start_column) = self.source_map.get_line_column(self.start);
            let (end_line, end_column) = self.source_map.get_line_column(self.end);
            write!(
                f,
                "{}#{}:{}-{}:{}",
                self.source_map.file_name,
                start_line, start_column, end_line, end_column
            )
        }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {:?}", self.span, self.kind)
    }
}

struct LexerSource {
    source_map: Rc<SourceMap>,
    chars: std::iter::Peekable<std::str::Chars<'static>>,
    pos: usize,
}

impl LexerSource {
    fn new(file_name: &'static str, src: &'static str) -> LexerSource {
        let source_map = Rc::new(SourceMap::new(file_name, src));
        LexerSource {
            source_map,
            chars: src.chars().peekable(),
            pos: 0,
        }
    }

    fn consume(&mut self) -> char {
        let c = self.chars.next().unwrap();
        self.pos += 1;
        c
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn span(&self, offset: usize) -> Span {
        Span::new(
            self.source_map.clone(),
            self.pos - offset,
            self.pos - offset,
        )
    }

    fn consume_while<F>(&mut self, mut f: F) -> (String, Span)
    where
        F: FnMut(char) -> bool,
    {
        let mut value = String::new();
        let start_span = self.span(0);
        while let Some(&c) = self.peek() {
            if f(c) {
                value.push(self.consume());
            } else {
                break;
            }
        }
        let end_span = self.span(1);
        (value, start_span.merge(&end_span))
    }

    fn parse_number(&mut self, first_char: Option<char>) -> Token {
        let res = self.consume_while(|c| c.is_ascii_digit() || c == '.');
        let mut span = res.1;
        let mut value = res.0;
        if let Some(c) = first_char {
            value.insert(0, c);
            span.start -= 1;
        }
        if value.contains('.') {
            Token::new(Float(value), span)
        } else {
            Token::new(Int(value), span)
        }
    }

    fn parse_char_token(&mut self, kind: TokenKind) -> Token {
        let span = self.span(0);
        self.consume();
        Token::new(kind, span)
    }

    fn parse_string(&mut self) -> Token {
        let mut value = String::new();
        let start = self.span(0);
        self.consume();
        while let Some(&c) = self.peek() {
            match c {
                '"' => {
                    self.consume();
                    return Token::new(Str(value), start.merge(&self.span(1)));
                }
                '\\' => {
                    self.consume();
                    let c = self.consume();
                    let c = match c {
                        'n' => '\n',
                        't' => '\t',
                        'r' => '\r',
                        '\\' => '\\',
                        '"' => '"',
                        _ => {
                            return Token::new(
                                Error(format!("unknown escape sequence: \\{}", c)),
                                start.merge(&self.span(1)),
                            )
                        }
                    };
                    value.push(c);
                }
                _ => value.push(self.consume()),
            }
        }
        Token::new(
            Error("unterminated string".to_string()),
            start.merge(&self.span(1)),
        )
    }
}

pub fn lexer(file_name: &'static str, src: &'static str) -> Result<Vec<Token>, String> {
    let mut tokens = Vec::new();
    let mut chars = LexerSource::new(file_name, src);
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            let (value, span) = chars.consume_while(|c| c.is_whitespace());
            tokens.push(Token::new(Whitespace(value), span));
        } else if c.is_ascii_digit() {
            tokens.push(chars.parse_number(None));
        } else if c == '"' {
            tokens.push(chars.parse_string());
        } else if c.is_ascii_alphabetic() {
            let start_span = chars.span(0);
            let mut value: String = chars.consume().to_string();
            if c == 'f' && chars.peek() == Some(&'"') {
                let string = chars.parse_string();
                match string.kind {
                    Error(_) => tokens.push(string),
                    Str(s) => tokens.push(Token::new(FormatStr(s), start_span.merge(&string.span))),
                    _ => unreachable!(),
                }
                continue;
            }
            let (more_value, span) = chars.consume_while(|c| c.is_ascii_alphanumeric() || c == '_');
            value.push_str(&more_value);
            let kind = Keywords::from_str(&value)
                .map(Keyword)
                .unwrap_or_else(|_| Ident(value));
            tokens.push(Token::new(kind, start_span.merge(&span)));
        } else if c == '-' {
            let start_span = chars.span(0);
            chars.consume();
            if chars.peek() == Some(&'-') {
                chars.consume();
                let mut comment = "--".to_owned();
                if chars.peek() == Some(&'-') {
                    chars.consume();
                    comment.push('-');
                    while let Some(&c) = chars.peek() {
                        comment.push(c);
                        chars.consume();
                        if comment.ends_with("---") {
                            break;
                        }
                    }
                } else {
                    let (value, _) = chars.consume_while(|c| c != '\n');
                    comment.push_str(&value);
                }
                tokens.push(Token::new(
                    TokenKind::Comment(comment),
                    start_span.merge(&chars.span(1)),
                ));
            } else if chars.peek().map_or(false, |c| c.is_ascii_digit()) {
                tokens.push(chars.parse_number(Some('-')));
            } else {
                tokens.push(Token::new(Operator(Minus), start_span));
            }
        } else if c == '+' {
            tokens.push(chars.parse_char_token(Operator(Plus)));
        } else if c == '*' {
            tokens.push(chars.parse_char_token(Operator(Star)));
        } else if c == '/' {
            tokens.push(chars.parse_char_token(Operator(Slash)));
        } else if c == '(' {
            tokens.push(chars.parse_char_token(LParen));
        } else if c == ')' {
            tokens.push(chars.parse_char_token(RParen));
        } else if c == '{' {
            tokens.push(chars.parse_char_token(LBrace));
        } else if c == '}' {
            tokens.push(chars.parse_char_token(RBrace));
        } else if c == '[' {
            tokens.push(chars.parse_char_token(LBracket));
        } else if c == ']' {
            tokens.push(chars.parse_char_token(RBracket));
        } else if c == ':' {
            tokens.push(chars.parse_char_token(Colon));
        } else if c == ',' {
            tokens.push(chars.parse_char_token(Comma));
        } else if c == '&' {
            tokens.push(chars.parse_char_token(Ampersand));
        } else if c == '.' {
            tokens.push(chars.parse_char_token(Dot));
        } else if c == '=' {
            let start_span = chars.span(0);
            chars.consume();
            if chars.peek() == Some(&'=') {
                chars.consume();
                tokens.push(Token::new(
                    Operator(DoubleEquals),
                    start_span.merge(&chars.span(1)),
                ));
            } else if chars.peek() == Some(&'>') {
                chars.consume();
                tokens.push(Token::new(FatArrow, start_span.merge(&chars.span(1))));
            } else {
                tokens.push(Token::new(Operator(Equals), start_span));
            }
        } else if c == '!' {
            chars.consume();
            if chars.peek() == Some(&'=') {
                chars.consume();
                tokens.push(Token::new(Operator(NotEquals), chars.span(0)));
            } else {
                tokens.push(Token::new(
                    Error("unrecognized token `!`".to_string()),
                    chars.span(0),
                ));
            }
        } else if c == '<' {
            let start_span = chars.span(0);
            chars.consume();
            if chars.peek() == Some(&'=') {
                chars.consume();
                tokens.push(Token::new(
                    Operator(LessThanOrEqual),
                    start_span.merge(&chars.span(1)),
                ));
            } else {
                tokens.push(Token::new(Operator(LessThan), start_span));
            }
        } else if c == '>' {
            let start_span = chars.span(0);
            chars.consume();
            if chars.peek() == Some(&'=') {
                chars.consume();
                tokens.push(Token::new(
                    Operator(GreaterThanOrEqual),
                    start_span.merge(&chars.span(1)),
                ));
            } else {
                tokens.push(Token::new(Operator(GreaterThan), start_span));
            }
        } else {
            let span = chars.span(0);
            tokens.push(Token::new(
                Error(format!("unknown character: {}", chars.consume())),
                span,
            ));
        }
    }
    Ok(tokens)
}

pub fn tokens_to_src(tokens: &[Token]) -> String {
    let mut src = String::new();
    for token in tokens {
        match &token.kind {
            Whitespace(value) | Comment(value) | Ident(value) | Int(value) | Float(value) => {
                src.push_str(value)
            }
            Str(value) => src.push_str(&format!("\"{}\"", value)),
            FormatStr(value) => src.push_str(&format!("f\"{}\"", value)),
            Operator(op) => src.push_str(op.into()),
            Keyword(kw) => src.push_str(kw.into()),
            Error(msg) => src.push_str(&format!("ERROR: {}", msg)),
            others => src.push_str(others.into()),
        }
    }
    src
}

#[cfg(test)]
mod tests {

    use super::{Keywords::*, *};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_number() {
        assert_lexer("123", &[Int("123".to_owned())]);
        assert_lexer("123.45", &[Float("123.45".to_owned())]);
        assert_lexer("-123.45", &[Float("-123.45".to_owned())]);
    }

    #[test]
    fn test_let() {
        assert_lexer(
            r#"
            let x = 123
            let a = "Ok"
            "#,
            &[
                Keyword(Let),
                Ident("x".to_owned()),
                Operator(Equals),
                Int("123".to_owned()),
                Keyword(Let),
                Ident("a".to_owned()),
                Operator(Equals),
                Str("Ok".to_owned()),
            ],
        );
    }

    fn assert_lexer(input: &'static str, expected: &[TokenKind]) {
        let tokens = lexer(input).unwrap();
        let src_reconstructed = tokens_to_src(&tokens);
        assert_eq!(src_reconstructed, input);
        let token_kinds: Vec<TokenKind> = tokens
            .into_iter()
            .map(|t| t.kind)
            .filter(|t| !matches!(t, Whitespace(_)))
            .collect();
        assert_eq!(token_kinds, expected);
    }
}
