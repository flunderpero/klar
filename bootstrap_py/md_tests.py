"""Run tests found in a markdown file."""

import sys
from dataclasses import dataclass
from subprocess import run
from tempfile import NamedTemporaryFile

from . import asm_darwin_arm64, error, ir, parser, tokenizer, typechecker


@dataclass
class Test:
    headings: list[str]
    line: int
    code: str
    expected_stdout: str


def run_test(test: Test) -> list:
    def handle_errors(errors: list[error.Error]) -> list[error.Error]:
        if errors:
            for err in list(errors):
                line_number = err.span.end_line_col()[0]
                line = test.code.split("\n")[line_number - 1]
                try:
                    index = line.index("-- Compile error: ")
                except ValueError:
                    index = -1
                if index >= 0:
                    expected_error_message = line[index + len("-- Compile error: ") :].strip()
                    if expected_error_message in str(err).split("\n")[0]:
                        # This error is expected.
                        errors = [x for x in errors if x != err]
                        continue
        return errors

    tokens, tokenize_errors = tokenizer.tokenize(tokenizer.Input("test.kl", test.code))
    if tokenize_errors:
        return handle_errors(tokenize_errors)
    next_id_value = 0

    def next_id() -> int:
        nonlocal next_id_value
        next_id_value += 1
        return next_id_value

    module, parse_errors = parser.parse(parser.Input(tokens, next_id))
    if parse_errors:
        return handle_errors(parse_errors)
    type_env, type_errors = typechecker.typecheck(module, next_id)
    if type_errors:
        return handle_errors(type_errors)
    ir_ = ir.generate_ir(module, type_env)
    asm = asm_darwin_arm64.generate(ir_)
    with NamedTemporaryFile() as tmp_file:
        p = run(
            ["clang", "-o", tmp_file.name, "-x", "assembler", "-"],
            input=str(asm),
            text=True,
            check=False,
            capture_output=True,
        )
        if p.returncode != 0:
            print(f"Compilation failed, clang exited with: {p.returncode}\n{p.stdout}\n{p.stderr}")
            if p.stdout:
                print(p.stdout)
            if p.stderr:
                print(p.stderr)
            return [f"Compilation failed, clang exited with: {p.returncode}"]
        p = run([tmp_file.name], check=False, capture_output=True, text=True)
        if p.returncode != 0:
            return [f"Test exited with code {p.returncode}\n{p.stdout}\n{p.stderr}"]

        stdout = p.stdout.strip().replace("\0", "")
        if stdout != test.expected_stdout:
            return [f"Expected:\n\n`{test.expected_stdout}`\n\ngot:\n\n`{stdout}`"]

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
            while i < len(lines):
                line = lines[i]
                if line.startswith("#"):
                    this_level = len(line.split(" ")[0])
                    if this_level <= heading_level:
                        break
                    headings = [x for x in headings if len(x.split(" ")[0]) < this_level]
                    headings.append(line)
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
                        if line.startswith("-- Output:"):
                            while i < len(lines):
                                line = lines[i]
                                i += 1
                                if not line.startswith("-- "):
                                    break
                                expected_stdout.append(line.split("-- ")[1].rstrip())
                            break
                        code.append(line)
                    code_str = "\n".join(code)
                    if "fn main()" not in code_str:
                        code_str = f"fn main() {{\n{code_str}\n}}"

                    test = Test(headings, test_line, code_str, "\n".join(expected_stdout))
                    tests.append(test)
    return tests


def main() -> int:
    if len(sys.argv) == 1:
        print("Usage: md_tests.py <file> [chapter]")
        return 1
    file = sys.argv[1]
    src = open(file).read()
    tests = find_tests(src, "" if len(sys.argv) == 2 else sys.argv[2])
    failed = 0
    for test in tests:
        print(" | ".join(test.headings), f"at {file}:{test.line} - ", end="")
        if errors := run_test(test):
            failed += 1
            print("\033[0;31mFAIL\033[0m")
            for err in errors:
                print()
                print(err)
        else:
            print("\033[0;32mPASS\033[0m")
    if failed:
        print(f"\n{failed}/{len(tests)} tests \033[0;31mFAILED\033[0m")
        return 2
    print(f"\nAll {len(tests)} tests \033[0;32mPASSED\033[0m")
    return 0


if __name__ == "__main__":
    ret = main()
    if ret:
        sys.exit(ret)
