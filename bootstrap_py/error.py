from __future__ import annotations

from dataclasses import dataclass
from traceback import format_stack
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .span import Span


@dataclass
class SimpleError:
    span: Span
    message: str
    stacktrace: str

    def __str__(self) -> str:
        code = "\n".join(self.span.formatted_lines())
        return f"{self.span}: {self.message}\n{code}"


@dataclass
class DuplicateError:
    name: str
    span: Span
    defined_here: Span
    stacktrace: str

    def __str__(self) -> str:
        code = "\n".join(self.span.formatted_lines())
        defined_here_code = "\n".join(self.defined_here.formatted_lines())
        return f"{self.span}: Duplicate `{self.name}` at:\n{code}\nis already defined here:\n{defined_here_code}"


@dataclass
class WithDefinitionError:
    span: Span
    message: str
    defined_here: Span
    stacktrace: str

    def __str__(self) -> str:
        code = "\n".join(self.span.formatted_lines())
        s = f"{self.span}: {self.message}\n{code}"
        if self.defined_here.start == 0 and self.defined_here.end == 0:
            return s
        defined_here_code = "\n".join(self.defined_here.formatted_lines())
        return s + f"\nDefined here:\n{defined_here_code}"


@dataclass
class CascadedError:
    span: Span
    origin_message: str
    originated_here: Span
    stacktrace: str

    def __str__(self) -> str:
        code = "\n".join(self.span.formatted_lines())
        origin_code = "\n".join(self.originated_here.formatted_lines())
        return f"{self.span}: {self.origin_message}\n{code}\nOriginated here:\n{origin_code}"


Error = SimpleError | WithDefinitionError | DuplicateError | CascadedError


def _stack() -> str:
    return "".join(format_stack()[:-2])


def unknown_token(span: Span, token: str) -> Error:
    return SimpleError(span, f"Unknown token `{token}`", _stack())


def unterminated_str_lit(span: Span, *, eof: bool) -> Error:
    return SimpleError(
        span, "Unexpected end of file while parsing string literal" if eof else "Unterminated string literal", _stack()
    )


def unexpected_token(span: Span, got: str, *expected: str) -> Error:
    expected_names = ", ".join(f"`{x}`" for x in expected)
    prefix = "Expected one of " if len(expected) > 1 else "Expected "
    return SimpleError(span, f"{prefix}{expected_names}, got `{got}`", _stack())


def expected_block_node(span: Span, token: str) -> Error:
    return SimpleError(span, f"Expected a block node, got token `{token}`", _stack())


def expected_ident(expr: str, span: Span) -> Error:
    return SimpleError(span, f"Expected an identifier, got `{expr}`", _stack())


def duplicate_fn(name: str, span: Span, defined_here: Span) -> Error:
    return DuplicateError(name, span, defined_here, _stack())


def duplicate_param_name(name: str, span: Span, defined_here: Span) -> Error:
    return DuplicateError(name, span, defined_here, _stack())


def undefined_name(name: str, span: Span) -> Error:
    return SimpleError(span, f"Undefined name `{name}`", _stack())


def unexpected_type(expected: str, got: str, span: Span) -> Error:
    return SimpleError(span, f"Expected {expected}, got {got}", _stack())


def cascaded_error(span: Span, origin_message: str, originated_here: Span) -> Error:
    return CascadedError(span, origin_message, originated_here, _stack())


def wrong_number_of_args(span: Span, params: int, args: int, defined_here: Span) -> Error:
    return WithDefinitionError(span, f"Expected {params} arguments, got {args}", defined_here, _stack())


def type_not_assignable_from(span: Span, target: str, from_: str) -> Error:
    return SimpleError(span, f"Type `{from_}` is not assignable to type `{target}`", _stack())


def not_mutable(name: str, span: Span) -> Error:
    return SimpleError(span, f"`{name}` is not mutable", _stack())
