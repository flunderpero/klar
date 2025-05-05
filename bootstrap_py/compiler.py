from __future__ import annotations

from dataclasses import dataclass
from subprocess import run
from time import time
from typing import TYPE_CHECKING

from . import asm_darwin_arm64, ast, error, ir, lower, lower_instance_methods, lower_monomorphize, parser, typechecker
from . import tokenizer as token

if TYPE_CHECKING:
    from collections.abc import Generator


@dataclass
class TokenStep:
    tokens: list[token.Token]
    errors: list[error.Error]
    duration: float

    def __str__(self) -> str:
        return "\n".join(str(x) for x in self.tokens)


@dataclass
class ParseStep:
    module: ast.Module
    errors: list[error.Error]
    duration: float

    def __str__(self) -> str:
        return str(self.module)


@dataclass
class TypecheckStep:
    module: ast.Module
    type_env: typechecker.TypeEnv
    errors: list[error.Error]
    duration: float

    def __str__(self) -> str:
        return self.debug()

    def __repr__(self) -> str:
        return self.signature()

    def debug(self) -> str:
        return self.debug_or_signature(debug=True)

    def signature(self) -> str:
        return self.debug_or_signature(debug=False)

    def debug_or_signature(self, *, debug: bool) -> str:
        lines = []

        def visit(node: ast.Node, _parent: ast.Node | None) -> None:
            typ = self.type_env.node_types.get(node.id)
            typ_str = (typ.debug() if debug else typ.signature()) if typ else "NOT_FOUND"
            node_str = str(node) if debug else ast.to_str_withoud_nid(node)
            code_str = node.span.lines(0)[1][0].strip()
            typ_str = typ_str.replace("\n", "\n    ")
            node_str = node_str.replace("\n", "\n    ")
            lines.append(str(node.span) + "    " + code_str)
            lines.append(f"    {node_str}")
            lines.append(f" => {typ_str}\n")
            ast.walk(node, visit)

        ast.walk(self.module, visit)
        return "\n".join(lines)


@dataclass
class AbortStep:
    errors: list[error.Error]
    duration: float

    def __str__(self) -> str:
        return "\n".join(str(x) for x in self.errors)


@dataclass
class LowerStep:
    fn_specs: list[lower.FnSpec]
    module: ast.Module
    duration: float

    def __str__(self) -> str:
        return str(self.module)

    def __repr__(self) -> str:
        return self.signature()

    def debug(self) -> str:
        return self.debug_or_signature(debug=True)

    def signature(self) -> str:
        return self.debug_or_signature(debug=False)

    def debug_or_signature(self, *, debug: bool) -> str:
        module = str(self.module) if debug else ast.to_str_withoud_nid(self.module)
        return module + "\n\nSpecs:\n" + "\n".join(x.debug() if debug else x.signature() for x in self.fn_specs)


@dataclass
class IRStep:
    ir: ir.IR
    duration: float

    def __str__(self) -> str:
        return str(self.ir)


@dataclass
class ASMStep:
    asm: str
    duration: float

    def __str__(self) -> str:
        return str(self.asm)


@dataclass
class CompileStep:
    returncode: int
    stdout: str
    stderr: str
    duration: float

    def __str__(self) -> str:
        return f"statuscode: {self.returncode}\nstdout: {self.stdout}\nstderr: {self.stderr}"


@dataclass
class RunStep:
    returncode: int
    stdout: str
    stderr: str
    duration: float

    def __str__(self) -> str:
        return f"statuscode: {self.returncode}\nstdout: {self.stdout}\nstderr: {self.stderr}"


CompilationStep = (
    TokenStep | ParseStep | TypecheckStep | AbortStep | LowerStep | IRStep | ASMStep | CompileStep | RunStep
)


def compile(input: token.Input, outfile: str) -> Generator[CompilationStep]:  # noqa: A001
    cur_id = 0

    def next_id() -> int:
        nonlocal cur_id
        cur_id += 1
        return cur_id

    start = time()
    tokens, tokenize_errors = token.tokenize(input)
    yield TokenStep(tokens, tokenize_errors, time() - start)
    start = time()
    module, parse_errors = parser.parse(parser.Input(tokens, next_id))
    yield ParseStep(module, parse_errors, time() - start)
    start = time()
    type_env, type_errors = typechecker.typecheck(module, next_id)
    yield TypecheckStep(module, type_env, type_errors, time() - start)
    start = time()
    if tokenize_errors or parse_errors or type_errors:
        yield AbortStep(tokenize_errors + parse_errors + type_errors, time() - start)
        return
    lower_instance_methods.lower_instance_methods(module, type_env)
    specs = lower_monomorphize.monomorphize(module, type_env)
    yield LowerStep(specs, module, time() - start)
    start = time()
    ir_ = ir.generate_ir(specs)
    yield IRStep(ir_, time() - start)
    start = time()
    asm = asm_darwin_arm64.generate(ir_)
    yield ASMStep(asm, time() - start)
    start = time()
    p = run(
        ["clang", "-o", outfile, "-x", "assembler", "-"],
        input=str(asm),
        text=True,
        check=False,
        capture_output=True,
    )
    yield CompileStep(p.returncode, p.stdout, p.stderr, time() - start)
    if p.returncode != 0:
        return
    start = time()
    p = run([outfile], check=False, capture_output=True, text=True)
    yield RunStep(p.returncode, p.stdout, p.stderr, time() - start)
