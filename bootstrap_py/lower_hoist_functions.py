from __future__ import annotations

from . import ast


def lower_hoist_functions(module: ast.Module) -> None:
    def visit(node: ast.Node, _parent: ast.Node | None) -> ast.Node:
        ast.walk(node, visit)
        match node:
            case ast.Block():
                fn_defs = [x for x in node.nodes if isinstance(x, ast.FnDef)]
                node.nodes = [x for x in node.nodes if not isinstance(x, ast.FnDef)]
                for fn_def in fn_defs:
                    module.nodes.append(fn_def)
        return node

    visit(module, None)
