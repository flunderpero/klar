use crate::ast;

pub fn transform(ast: &mut ast::Module) {
    for item in &mut ast.items {
        match &mut item.kind {
            ast::ItemKind::Function(function) => if item.identifier.name == "main" {
                transform_main(function);
            },
        }
    }
}

fn transform_main(main_fn: &mut ast::Function) {
    main_fn.return_type = ast::Type::I32;
    main_fn.body.expressions.push(ast::Expression::new(
        main_fn.span.clone(),
        ast::ExpressionKind::Return(Box::new(ast::Expression::new(
            main_fn.span.clone(),
            ast::ExpressionKind::Literal(ast::Literal::I32(0)),
        ))),
    ));
}
