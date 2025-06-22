from __future__ import annotations

import contextlib
import os
import tempfile
import textwrap
from dataclasses import dataclass
from typing import cast

import pytest

from . import ast, compiler, error, ir, parser, tokenizer, types
from . import typechecker as tc

id_ = 0


def next_id() -> int:
    global id_  # noqa: PLW0603
    id_ += 1
    return id_


def errors_str(errors: list[error.Error]) -> str:
    return "\n".join(f"{x} at {x.stacktrace}" for x in errors)


def parse(code: str) -> ast.Module:
    global id_  # noqa: PLW0603
    id_ = 0
    code = strip(code)
    tokens, errors = tokenizer.tokenize(tokenizer.Input("test.kl", code.strip()))
    assert errors_str(errors) == ""
    module, errors = parser.parse(parser.Input(tokens, next_id))
    assert errors_str(errors) == ""
    return module


def typecheck(code: str) -> TypeChecker:
    module = parse(code)
    type_env, errors = tc.typecheck(module, next_id)
    assert errors_str(errors) == ""
    return TypeChecker(module, type_env)


def typecheck_err(code: str) -> list[error.Error]:
    module = parse(code)
    _, errors = tc.typecheck(module, next_id)
    return errors


def compile_and_run(code: str, *, debug: str = "") -> compiler.RunStep:
    outfile = tempfile.gettempdir() + "/test"
    code = strip(code)
    compilation = compiler.compile(tokenizer.Input("test.kl", code), str(outfile))
    run: compiler.RunStep | None = None
    try:
        for step in compilation:
            match step:
                case compiler.TokenStep():
                    if debug == "token":
                        print(step)
                    assert errors_str(step.errors) == ""
                case compiler.ParseStep():
                    if debug == "parse":
                        print(step)
                    assert errors_str(step.errors) == ""
                case compiler.TypecheckStep():
                    if debug == "typecheck":
                        print(step)
                    assert errors_str(step.errors) == ""
                case compiler.AbortStep():
                    raise AssertionError(f"Unexpected abort step: {step}")
                case compiler.LowerStep():
                    if debug == "lower":
                        print(step.debug())
                case compiler.IRStep():
                    if debug == "ir":
                        print(step)
                case compiler.ASMStep():
                    if debug == "asm":
                        print(step)
                case compiler.CompileStep():
                    assert step.stderr == ""
                    assert step.returncode == 0
                case compiler.RunStep():
                    run = step
                case _:
                    raise AssertionError(f"Unexpected step: {step}")
    finally:
        with contextlib.suppress(BaseException):
            os.remove(outfile)
    assert run is not None
    return run


def strip(s: str) -> str:
    return textwrap.dedent(s).strip() + "\n"


def compile_and_run_success(code: str, *, debug: str = "") -> str:
    run = compile_and_run(code, debug=debug)
    assert run.stderr == ""
    assert run.returncode == 0
    return run.stdout


@dataclass
class IRTester:
    reg_id: int
    block_id: int

    def reg(self, typ: ir.Type) -> ir.Reg:
        if typ == ir.NoneTyp:
            return ir.NoneReg
        self.reg_id += 1
        return ir.Reg(id=ir.RegId(self.reg_id), typ=typ)

    def block(self, insts: list[ir.Inst], terminator: ir.Terminator | None) -> ir.Block:
        self.block_id += 1
        return ir.Block(id=ir.BlockId(self.block_id), insts=insts, terminator=terminator)

    def fn_ir(self, blocks: list[ir.Block]) -> ir.FnIR:
        return ir.FnIR(
            fn_def=cast(ast.FnDef, None), fn_name="main", params=[], result=cast(ir.Type, None), blocks=blocks
        )

    def int(self) -> ir.Int:
        return ir.Int(bits=64, signed=True)


@pytest.fixture
def ir_tester() -> IRTester:
    return IRTester(reg_id=0, block_id=0)


@dataclass
class TypeChecker:
    module: ast.Module
    type_env: tc.TypeEnv

    def type_at[T: types.Type](self, line: int, col: int, node_typ: type[ast.Node], typ: type[T] | None = None) -> T:
        node = self.node_at(line, col, node_typ)
        if typ is not None:
            assert isinstance(self.type_env.get_node_type(node)[0], typ), (
                f"Expected {typ}, got {type(self.type_env.get_node_type(node))}"
            )
        return self.type_env.get_node_type(node)[0]

    def node_at(self, line: int, col: int, typ: type[ast.Node]) -> ast.Node:
        res: ast.Node | None = None

        def visit(node: ast.Node, _parent: ast.Node | None) -> ast.Node:
            nonlocal res
            start = node.span.start_line_col()
            if start[0] == line and start[1] >= col and isinstance(node, typ):  # noqa: SIM102
                if res is None or start[0] < res.span.start_line_col()[0]:
                    res = node
            ast.walk(node, visit)
            return node

        visit(self.module, None)
        assert res is not None, f"No node found at {line}:{col}"
        return res

    def debug(self) -> str:
        lines = []

        def visit(node: ast.Node, _parent: ast.Node | None) -> ast.Node:
            if not isinstance(node, (ast.Module, ast.FnDef)):
                try:
                    typ = self.type_env.get_node_type(node)
                    lines.append(
                        "\n".join(x.rstrip() for x in node.span.formatted_lines(0, enclosing_empty_lines=False))
                    )
                    lines.append(f"{typ.__class__.__name__:10}: {typ[0].debug()}")
                    lines.append("")
                except KeyError:
                    print("Node has no type - fix that!", node)
            ast.walk(node, visit)
            return node

        visit(self.module, None)
        return "\n".join(lines)
