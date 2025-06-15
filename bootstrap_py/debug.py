from __future__ import annotations

from . import ast, types

debug_level = 0


def debug(level: int, node_or_type: ast.Node | types.Type, msg: str) -> None:
    if debug_level < level:
        return
    id_str = f"node#{node_or_type.id}" if isinstance(id, ast.Node) else f"type#{node_or_type.id}"
    msg = f"DEBUG {id_str} {msg} at {node_or_type.span}"
    print("\033[1;34m" + msg + "\033[0m")
