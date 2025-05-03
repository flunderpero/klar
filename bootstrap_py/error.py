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

    def short_message(self) -> str:
        return self.message


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

    def short_message(self) -> str:
        return f"Duplicate `{self.name}`"


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

    def short_message(self) -> str:
        return self.message


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

    def short_message(self) -> str:
        return self.origin_message


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
    if not expected:
        return SimpleError(span, f"Unexpected token `{got}`", _stack())
    expected_names = ", ".join(f"`{x}`" for x in expected)
    prefix = "Expected one of " if len(expected) > 1 else "Expected "
    return SimpleError(span, f"{prefix}{expected_names}, got `{got}`", _stack())


def expected_block_node(span: Span, token: str) -> Error:
    return SimpleError(span, f"Expected a block node, got token `{token}`", _stack())


def expected_ident(expr: str, span: Span) -> Error:
    return SimpleError(span, f"Expected an identifier, got `{expr}`", _stack())


def duplicate_declaration(name: str, span: Span, defined_here: Span) -> Error:
    return DuplicateError(name, span, defined_here, _stack())


def undefined_name(name: str, span: Span) -> Error:
    return SimpleError(span, f"Undefined name `{name}`", _stack())


def no_member(name: str, target: str, span: Span, target_defined_here: Span) -> Error:
    return WithDefinitionError(span, f"No member `{name}` in type `{target}`", target_defined_here, _stack())


def unexpected_type(expected: str, got: str, span: Span) -> Error:
    return SimpleError(span, f"Expected {expected}, got {got}", _stack())


def cascaded_error(span: Span, origin_message: str, originated_here: Span) -> Error:
    return CascadedError(span, origin_message, originated_here, _stack())


def wrong_number_of_args(span: Span, params: int, args: int, defined_here: Span) -> Error:
    return WithDefinitionError(span, f"Expected {params} arguments, got {args}", defined_here, _stack())


def wrong_number_of_type_args(type_params: int, type_args: int, span: Span, defined_here: Span) -> Error:
    return WithDefinitionError(span, f"Expected {type_params} type arguments, got {type_args}", defined_here, _stack())


def type_not_assignable_from(span: Span, target: str, from_: str) -> Error:
    return SimpleError(span, f"Type `{from_}` is not assignable to type `{target}`", _stack())


def not_mutable(name: str, span: Span) -> Error:
    return SimpleError(span, f"`{name}` is not mutable", _stack())


def break_outside_loop(span: Span) -> Error:
    return SimpleError(span, "`break` outside of a loop", _stack())


def continue_outside_loop(span: Span) -> Error:
    return SimpleError(span, "`continue` outside of a loop", _stack())


def not_generic(span: Span, defined_here: Span) -> Error:
    return WithDefinitionError(span, "Type is not generic", defined_here, _stack())


def type_param_not_bound(name: str, span: Span) -> Error:
    return SimpleError(span, f"Type parameter `{name}` is not bound to a trait", _stack())


def self_not_allowed_here(span: Span) -> Error:
    return SimpleError(span, "`self` is not allowed here", _stack())


def not_declared_in_current_scope(name: str, span: Span) -> Error:
    return SimpleError(span, f"`{name}` is not declared in the current scope", _stack())


def trait_method_impl_missing(trait_name: str, method_name: str, trait_span: Span, span: Span) -> Error:
    return WithDefinitionError(
        span,
        f"Missing implementation of trait method `{method_name}` in trait `{trait_name}`",
        trait_span,
        _stack(),
    )


def trait_method_impl_mismatch(trait_method_signature: str, impl_signature: str, trait_span: Span, span: Span) -> Error:
    return WithDefinitionError(
        span,
        f"Method signature `{impl_signature}` does not match trait method signature `{trait_method_signature}`",
        trait_span,
        _stack(),
    )


def trait_qualifier_mismatch(trait_signature: str, target_fqn: str, trait_span: Span, span: Span) -> Error:
    return WithDefinitionError(
        span,
        f"Trait has already been implemented for `{target_fqn}` with signature `{trait_signature}`",
        trait_span,
        _stack(),
    )
