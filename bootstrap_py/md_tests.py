"""Run tests found in a markdown file."""
# ruff: noqa: T201

import contextlib
import re
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

from . import compiler, error, tokenizer


@dataclass
class Test:
    headings: list[str]
    test_num: int
    line: int
    code: str
    expected_stdout: str

    def name(self) -> str:
        return " > ".join(self.headings) + f" ({self.test_num})"


def run_test(test: Test, print_code: str) -> list:
    def handle_errors(errors: list[error.Error]) -> list[error.Error]:
        if errors:
            for err in list(errors):
                line_number = err.span.start_line_col()[0]
                line = test.code.split("\n")[line_number - 1]
                try:
                    index = line.index("-- ERROR: ")
                except ValueError:
                    index = -1
                if index >= 0:
                    expected_error_message = line[index + len("-- ERROR: ") :].strip()
                    if expected_error_message in str(err).split("\n")[0]:
                        # This error is expected.
                        errors = [x for x in errors if x != err]
                        continue
        return errors

    tmp_file = Path(tempfile.gettempdir(), re.sub(r"[^a-zA-Z0-9]", "_", test.name()))
    try:
        for step in compiler.compile(tokenizer.Input("test.kl", test.code), str(tmp_file)):
            match step:
                case compiler.TokenStep():
                    if print_code == "tokens":
                        print()
                        print(step)
                        return handle_errors(step.errors)
                    if step.errors:
                        return handle_errors(step.errors)
                case compiler.ParseStep():
                    if print_code == "ast":
                        print()
                        print(step)
                        return handle_errors(step.errors)
                    if step.errors:
                        return handle_errors(step.errors)
                case compiler.TypecheckStep():
                    if print_code == "types":
                        print()
                        print(step)
                        return handle_errors(step.errors)
                    if step.errors:
                        return handle_errors(step.errors)
                case compiler.AbortStep():
                    pass
                case compiler.LowerStep():
                    if "-- ERROR:" in test.code:
                        return ["Expected compile error did not occur"]
                    if print_code == "lower":
                        print()
                        print(step)
                        return []
                case compiler.IRStep():
                    if print_code == "ir":
                        print()
                        print(step)
                        return []
                case compiler.ASMStep():
                    if print_code == "asm":
                        print()
                        print(step)
                        return []
                case compiler.CompileStep():
                    if step.returncode != 0:
                        return [f"Compilation (clang) failed with code {step.returncode}\n{step.stdout}\n{step.stderr}"]
                case compiler.RunStep():
                    if step.returncode != 0:
                        return [f"Test exited with code {step.returncode}\n{step.stdout}\n{step.stderr}"]
                    stdout = step.stdout.strip().replace("\0", "")
                    if stdout != test.expected_stdout:
                        return [f"Expected:\n\n`{test.expected_stdout}`\n\ngot:\n\n`{stdout}`"]
                case _:
                    raise ValueError(f"Unknown step: {step}")
    finally:
        with contextlib.suppress(FileNotFoundError):
            tmp_file.unlink()

    return []


def find_tests(src: str, chapter: str) -> list[Test]:
    tests: list[Test] = []
    lines = src.split("\n")
    i = 0
    while i < len(lines):
        line = lines[i]
        i += 1
        headings = []
        if line.startswith("#") and chapter.lower() in line.lower():
            headings.append(line)
            heading_level = len(line.split(" ")[0])
            test_num = 1
            while i < len(lines):
                line = lines[i]
                if line.startswith("#"):
                    this_level = len(line.split(" ")[0])
                    if this_level <= heading_level:
                        break
                    headings = [x for x in headings if len(x.split(" ")[0]) < this_level]
                    headings.append(line)
                    test_num = 1
                i += 1
                if line.startswith("```klar"):
                    test_line = i - 1
                    expected_stdout = []
                    code = []
                    while i < len(lines):
                        line = lines[i]
                        i += 1
                        if line.startswith("```"):
                            break
                        code.append(line)
                    code_str = "\n".join(code)
                    if lines[i].strip() == "" and lines[i + 1].strip() == "```":
                        i += 2
                        while i < len(lines):
                            line = lines[i]
                            if line.strip() == "```":
                                break
                            expected_stdout.append(line)
                            i += 1

                    test = Test(headings, test_num, test_line, code_str, "\n".join(expected_stdout))
                    test_num += 1
                    tests.append(test)
    return tests


def main() -> int:
    args = sys.argv
    print_code = ""
    stages = ("tokens", "ast", "types", "lower", "ir", "asm")
    for stage in stages:
        if f"--{stage}" in args:
            print_code = stage
            break
    print_error_stack = "--err-stack" in args
    args = [x for x in args if not x.startswith("--")]
    if len(args) == 1:
        print("Usage: md_tests.py <file> [chapter] [#test] [options]")
        print("  Options:")
        print("    --err-stack     Print error stack")
        for stage in stages:
            print(f"    --{stage}        Print {stage} output")
        return 1
    file = args[1]
    src = open(file).read()
    tests = find_tests(src, "" if len(args) == 2 else args[2])
    if len(args) > 3:
        test_num = int(args[3]) - 1
        tests = tests[test_num : test_num + 1]
    failed = 0
    for test in tests:
        print(test.name(), f"at {file}:{test.line}", end="")
        if errors := run_test(test, print_code):
            failed += 1
            print(" \033[0;31mFAIL\033[0m")
            for err in errors:
                print()
                print(err)
                if print_error_stack:
                    print("at", err.stacktrace)
        elif not print_code:
            print(" \033[0;32mPASS\033[0m")
    if failed:
        print(f"\n{failed}/{len(tests)} tests \033[0;31mFAILED\033[0m")
        return 2
    if not print_code:
        print(f"\nAll {len(tests)} tests \033[0;32mPASSED\033[0m")
    return 0


if __name__ == "__main__":
    ret = main()
    if ret:
        sys.exit(ret)
