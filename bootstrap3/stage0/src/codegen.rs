use std::fmt::Display;

use crate::{
    ast::{self, Type},
    lexer::Token,
};

struct Value {
    value: String,
    type_: Type,
}

impl Value {
    fn new(value: String, type_: Type) -> Self {
        Self { value, type_ }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

pub struct CompileError {
    pub message: String,
    pub token: Option<Token>,
}

impl CompileError {
    pub fn new(message: &str, token: Option<Token>) -> Self {
        Self {
            message: message.to_string(),
            token,
        }
    }
}

impl From<CompileError> for String {
    fn from(val: CompileError) -> Self {
        val.message
    }
}

struct CodeEmitter {
    code: String,
    id: usize,
    literals: Vec<ast::Literal>,
}

impl CodeEmitter {
    fn new() -> Self {
        Self {
            code: String::new(),
            id: 1,
            literals: Vec::new(),
        }
    }

    fn emit(&mut self, code: &str) {
        self.code.push_str(code);
        self.code.push('\n');
    }

    fn emit_reg(&mut self, code: &str, type_: ast::Type) -> Value {
        let reg = Value::new(format!("%{}", self.id), type_);
        self.id += 1;
        self.emit(&format!("{} = {}", reg, code));
        reg
    }

    fn emit_literal(&mut self, value: ast::Literal) -> Value {
        if let ast::Literal::String(_) = value {
            let index = match self.literals.iter().position(|x| *x == value) {
                Some(index) => index,
                None => {
                    self.literals.push(value.clone());
                    self.literals.len() - 1
                }
            };
            return Value::new(format!("*@{}", index), value.type_());
        }
        Value::new(value.to_string(), value.type_())
    }

    fn emit_literal_constants(&mut self) {
        let mut output = String::new();
        for (index, literal) in self.literals.iter().enumerate() {
            output.push_str(&format!(
                "@{} = private unnamed_addr constant {}\n",
                index, literal
            ));
        }
        self.emit(&output);
    }
}

type CompileResult = Result<(), CompileError>;

pub fn generate_code(module: ast::Module) -> Result<String, CompileError> {
    let mut code = CodeEmitter::new();
    for item in module.items {
        generate_item(&mut code, item)?;
    }
    code.emit_literal_constants();
    Ok(code.code)
}

fn generate_item(code: &mut CodeEmitter, item: ast::Item) -> CompileResult {
    match item.kind {
        ast::ItemKind::Function(function) => generate_function(code, item.identifier, function),
    }
}

fn generate_function(
    code: &mut CodeEmitter,
    name: ast::Identifier,
    function: ast::Function,
) -> CompileResult {
    code.emit(&format!("define {} @{}() {{", function.return_type, name.name));
    for item in function.body.items {
        generate_item(code, item)?;
    }
    for expression in function.body.expressions {
        generate_expression(code, expression);
    }
    code.emit("}");
    Ok(())
}

fn generate_expression(code: &mut CodeEmitter, expression: ast::Expression) -> Value {
    match expression.kind {
        ast::ExpressionKind::Literal(literal) => code.emit_literal(literal),
        ast::ExpressionKind::Return(expression) => {
            let reg = generate_expression(code, *expression);
            code.emit(&format!("ret {} {}", reg.type_, reg));
            reg
        }
    }
}
