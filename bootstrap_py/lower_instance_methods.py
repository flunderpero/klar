from __future__ import annotations

from . import ast, typechecker, types


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
                if isinstance(node.callee, ast.Member):
                    callee = type_env.get_node_type(node.callee)
                    if isinstance(callee, types.Member):
                        callee = callee.deep_member().typ
                    assert isinstance(callee, types.Fn), f"expected function, got {callee.signature()}"
                    if callee.is_instance_method():
                        node.args = [node.callee.target, *node.args]
                        node.callee = ast.Ident(
                            node.callee.id,
                            str(callee.fqn),
                            [],
                            node.callee.span,
                        )

        ast.walk(node, visit)

    visit(module, None)
