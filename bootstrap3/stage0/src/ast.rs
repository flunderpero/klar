use std::{iter::Peekable, slice::Iter, fmt::{Display, Formatter}};

use crate::lexer::{Keywords::*, TokenKind as TK, *};

#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct Item {
    pub identifier: Identifier,
    pub kind: ItemKind,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function(Function),
    // Struct(Struct),
    // Enum(Enum),
    // Union(Union),
    // Trait(Trait),
    // Impl(Impl),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub span: Span,
    pub parameters: Vec<Parameter>,
    pub body: Block,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub span: Span,
    pub items: Vec<Item>,
    pub expressions: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub span: Span,
    pub identifier: Identifier,
    pub kind: Type,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub span: Span,
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Str,
    Void,
    // Struct(Struct),
    // Enum(Enum),
    // Union(Union),
    // Trait(Trait),
    // Impl(Impl),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"), 
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i16"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "i1"),
            Type::Str => write!(f, "i8*"),
            Type::Void => write!(f, "void"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn new(span: Span, kind: ExpressionKind) -> Self {
        Self { span, kind }
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Literal(Literal),
    Return(Box<Expression>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    String(String),
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::I8(val) => write!(f, "{}", val),
            Literal::I16(val) => write!(f, "{}", val),
            Literal::I32(val) => write!(f, "{}", val),
            Literal::I64(val) => write!(f, "{}", val),
            Literal::F32(val) => write!(f, "{}", val),
            Literal::F64(val) => write!(f, "{}", val),
            Literal::Bool(val) => write!(f, "{}", val),
            Literal::String(val) => write!(f, "\"{}\"", val),
        }
    }
}

impl Literal {
    pub fn type_(&self) -> Type {
        match self {
            Literal::I8(_) => Type::I8,
            Literal::I16(_) => Type::I16,
            Literal::I32(_) => Type::I32,
            Literal::I64(_) => Type::I64,
            Literal::F32(_) => Type::F32,
            Literal::F64(_) => Type::F64,
            Literal::String(_) => Type::Str,
            Literal::Bool(_) => Type::Bool,
        }
    }
}


#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub token: Option<Token>,
}

impl From<ParseError> for String {
    fn from(val: ParseError) -> Self {
        val.message
    }
}

impl ParseError {
    pub fn new(message: &str, token: Option<Token>) -> Self {
        Self {
            message: message.to_string(),
            token,
        }
    }

    pub fn unexpected(token: Token) -> Self {
        Self {
            message: "unexpected token".to_string(),
            token: Some(token),
        }
    }
}

impl From<std::num::ParseIntError> for ParseError {
    fn from(error: std::num::ParseIntError) -> Self {
        Self {
            message: error.to_string(),
            token: None,
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;

type TokenStream<'a> = Peekable<Iter<'a, Token>>;

#[derive(Debug)]
struct ParseStream<'a> {
    tokens: TokenStream<'a>,
    last_token: Option<&'a Token>,
}

impl ParseStream<'_> {
    fn next_identifier(&mut self) -> ParseResult<Identifier> {
        let token = self.next()?;
        match &token.kind {
            TK::Ident(name) => Ok(Identifier {
                span: token.span.clone(),
                name: name.clone(),
            }),
            _ => Err(ParseError::new("expected identifier", Some(token.clone()))),
        }
    }

    fn next_match(&mut self, predicate: fn(&Token) -> bool) -> ParseResult<&Token> {
        let token = self.next()?;
        if predicate(token) {
            Ok(token)
        } else {
            Err(ParseError::new("expected token", Some(token.clone())))
        }
    }

    fn next(&mut self) -> ParseResult<&Token> {
        match self.next_optional() {
            Some(token) => {
                // self.last_consumed_span = Some(token.span.clone());
                Ok(token)
            }
            None => Err(ParseError::new("unexpected end of file", None)),
        }
    }

    fn next_optional(&mut self) -> Option<&Token> {
        self.peek_optional();
        let token = self.tokens.next();
        if let Some(token) = &token {
            self.last_token = Some(token);
        }
        token
    }

    fn peek_optional(&mut self) -> Option<&Token> {
        while let Some(token) = self.tokens.peek() {
            match token.kind {
                TK::Whitespace(_) => {
                    self.tokens.next();
                }
                TK::Comment(_) => {
                    self.tokens.next();
                }
                _ => break,
            }
        }
        self.tokens.peek().cloned()
    }

    fn peek(&mut self) -> ParseResult<&Token> {
        match self.peek_optional() {
            Some(token) => Ok(token),
            None => Err(ParseError::new("unexpected end of file", None)),
        }
    }
}

pub fn parse(tokens: Vec<Token>) -> Result<Module, ParseError> {
    let mut items: Vec<Item> = Vec::new();
    let mut stream = ParseStream {
        tokens: tokens.iter().peekable(),
        last_token: None,
    };
    while let Some(token) = stream.next_optional() {
        let span = token.span.clone();
        match token.kind {
            TK::Keyword(Fn) => {
                let name = stream.next_identifier()?;
                stream.next_match(|token| token.kind == TK::LParen)?;
                // TODO: parse parameters
                let parameters = Vec::new();
                stream.next_match(|token| token.kind == TK::RParen)?;
                let body = parse_block(&mut stream, |token| {
                    matches!(token.kind, TK::Keyword(Keywords::End))
                })?;
                // TODO: parse return type
                items.push(Item {
                    identifier: name,
                    kind: ItemKind::Function(Function {
                        parameters,
                        span: span.merge(&body.span),
                        body,
                        return_type: Type::Void,
                    }),
                });
            }
            _ => return Err(ParseError::unexpected(token.clone())),
        }
    }
    Ok(Module { items })
}

fn parse_block(stream: &mut ParseStream, end_predicate: fn(&Token) -> bool) -> ParseResult<Block> {
    let items: Vec<Item> = Vec::new();
    let mut body: Vec<Expression> = Vec::new();
    let block_type = stream.next()?;
    let mut span = block_type.span.clone();
    match block_type.kind {
        TK::Colon => loop {
            let token = stream.peek()?;
            if end_predicate(token) {
                stream.next()?;
                break;
            }
            body.push(parse_expression(stream)?);
        },
        TK::FatArrow => {
            body.push(parse_expression(stream)?);
        }
        _ => return Err(ParseError::unexpected(block_type.clone())),
    };
    if let Some(token) = &stream.last_token {
        span = span.merge(&token.span);
    }
    Ok(Block {
        items,
        expressions: body,
        span,
    })
}

fn parse_expression(stream: &mut ParseStream) -> ParseResult<Expression> {
    let token = stream.next()?;
    match &token.kind {
        TK::Int(value) => {
            let value = value.parse()?;
            Ok(Expression::new(
                token.span.clone(),
                ExpressionKind::Literal(Literal::I64(value)),
            ))
        }
        _ => Err(ParseError::unexpected(token.clone())),
    }
}
