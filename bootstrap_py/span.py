from __future__ import annotations

from dataclasses import dataclass


@dataclass
class Span:
    file: str
    src: str
    start: int
    end: int

    def code(self) -> str:
        return self.src[self.start : self.end]

    def start_line_col(self) -> tuple[int, int]:
        return self.line_col(self.start)

    def end_line_col(self) -> tuple[int, int]:
        return self.line_col(self.end)

    def line_col(self, pos: int) -> tuple[int, int]:
        line = 1
        col = 1
        for i in range(pos):
            if self.src[i] == "\n":
                line += 1
                col = 1
            else:
                col += 1
        return line, col

    def lines(self, pad: int = 0) -> tuple[list[str], list[str], list[str]]:
        """Return the lines the span belongs to as well as up to `pad` lines before and after."""
        start = self.start
        while start > 0 and self.src[start - 1] != "\n":
            start -= 1
        end = self.end
        while end < len(self.src) and self.src[end] != "\n":
            end += 1
        before_start = start - 1
        after_end = end + 1
        for _ in range(pad):
            while before_start > 0 and self.src[before_start] != "\n":
                before_start -= 1
            while after_end < len(self.src) - 1 and self.src[after_end] != "\n":
                after_end += 1
            after_end += 1
        return (
            [x for x in self.src[before_start:start].split("\n") if x],
            [x for x in self.src[start:end].split("\n") if x],
            [x for x in self.src[end:after_end].split("\n") if x],
        )

    def formatted_lines(self, pad: int = 2, *, enclosing_empty_lines: bool = True) -> list[str]:
        before, lines, after = self.lines(pad)
        start = self.start_line_col()
        end = self.end_line_col()
        line_num_width = len(str(start[0] + len(lines) - 1))
        line_num = start[0] - len(before)

        def code_line(line: str) -> str:
            nonlocal line_num
            res = f" {line_num:>{line_num_width}} | {line}"
            line_num += 1
            return res

        empty_line_prefix = " " + " " * line_num_width + " |"
        result = []
        if enclosing_empty_lines:
            result.append(empty_line_prefix)
        result.extend(code_line(x) for x in before)
        for i, line in enumerate(lines):
            result.append(code_line(line))
            if len(lines) > 1 and i == 0:
                result.append(empty_line_prefix + " " * start[1] + "^")
        if len(lines) == 1:
            result.append(empty_line_prefix + " " * start[1] + "^" * (end[1] - start[1]))
        result.extend(code_line(x) for x in after)
        if enclosing_empty_lines:
            result.append(empty_line_prefix)
        return result

    def merge(self, other: Span) -> Span:
        return Span(self.file, self.src, self.start, other.end)

    def __str__(self) -> str:
        line, col = self.start_line_col()
        return f"{self.file}:{line}:{col}"


@dataclass
class FQN:
    path: list[str]

    def __str__(self) -> str:
        return ".".join(self.path)

    def __hash__(self) -> int:
        return hash(str(self))

    def concat(self, *parts: str) -> FQN:
        return FQN(self.path + list(parts))
