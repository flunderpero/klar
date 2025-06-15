from __future__ import annotations

from . import ast, typechecker, types


def lower_instance_methods(module: ast.Module, type_env: typechecker.TypeEnv) -> None:
    receivers: dict[int, ast.Expr] = {}

    def visit(node: ast.Node, _parent: ast.Node | None) -> ast.Node:
        ast.walk(node, visit)
        match node:
            case ast.Member():
                callee = type_env.get_node_type(node)[0]
                if isinstance(callee, types.Fn) and callee.is_instance_method():
                    ident = ast.Ident(node.id, str(callee.fqn), [], node.span)
                    type_env.set_node_type(ident, callee)
                    receivers[id(callee)] = node.target
                    return ident
            case ast.Let():
                value = type_env.get_node_type(node.value)[0]
                type_env.set_node_type(node.value, value)
            case ast.Call():
                callee = type_env.get_node_type(node.callee)[0]
                receiver = receivers.get(id(callee))
                if receiver is not None:
                    node.args = [receiver, *node.args]
        return node

    visit(module, None)
