from __future__ import annotations

from . import ast, typechecker, types


def adapt_call(node: ast.Call, fn: types.Fn) -> None:
    assert isinstance(node.callee, ast.Member), f"expected ast.Member, got {node.callee}"
    node.args = [node.callee.target, *node.args]
    node.callee = ast.Ident(
        node.callee.id,
        str(fn.fqn),
        [],
        node.callee.span,
    )


def lower_instance_methods(module: ast.Module, type_env: typechecker.TypeEnv) -> None:
    """Modify the AST in place and convert all instance method calls to regular calls.

    This turns calls like this:
        instance.method(x, y)
    to this:
        Instance::method(instance, x, y)
    """

    def visit(node: ast.Node, _parent: ast.Node | None) -> None:
        match node:
            case ast.Call():
                typ = type_env.get_node_type(node.callee)
                if isinstance(typ, types.Instance) and isinstance(typ.typ, types.Fn) and typ.typ.is_instance_method():
                    adapt_call(node, typ.typ)
        ast.walk(node, visit)

    visit(module, None)
