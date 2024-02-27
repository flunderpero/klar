""" 
    This is a faster version of `interpreter.py` that first tokenizes
    the input and then executes these tokens instead of constantly 
    reading one byte at a time.
    
    We needed this version, because `interpreter.py` took minutes(!)
    to interpret our stage1 compiler `compiler.tl`.

    Because we had to switch during development of `compiler.tl` this
    interpreter is slightly more "advanced" and differs in the handling
    of some syntax.

    Basically, this is the implementation `compiler.tl` follows behaviour
    wise and `interpreter.py` approach wise.
"""
from sys import argv, stdin
from enum import Enum
from dataclasses import dataclass

buffer = open(argv[1])


def read():
    return buffer.read(1)


parsing_string = False
parsing_string_delimiter = ""
tokens = []
token_values = []
token = ""
next_c = read()

# Tokenizer.
while True:
    c = next_c
    next_c = read()
    if c == "":
        break
    if parsing_string:
        if c == parsing_string_delimiter:
            parsing_string = False
            tokens.append("string")
            token_values.append(token)
            token = ""
            c = ""
        else:
            if c == "\\" and next_c == "n":
                token = token + "\n"
                next_c = read()
                continue
            elif c == "\\" and next_c == "\\":
                token = token + "\\"
                next_c = read()
                continue
    elif c == " " or c == "(" or c == ")" or c == "\n" or c == "=" or c == '"' or c == "'" or c == "+":
        if token == "let" or token == "if" or token == "then" or token == "else" or token == "end" or token == "loop" or token == "do" or token == "break" or token == "let":
            tokens.append(token)
            token_values.append("")
            token = ""
        elif token == "--":
            tokens.append(token)
            value = ""
            token = ""
            while True:
                if not next_c or next_c == "\n":
                    break
                value = value + next_c
                next_c = read()
            token_values.append(value)
        elif token:
            tokens.append("identifier")
            token_values.append(token)
            token = ""
        if c == "(" or c == ")" or c == "\n" or c == "+":
            tokens.append(c)
            token_values.append("")
        if c == "=" and next_c != "=":
            tokens.append(c)
            token_values.append("")
        if c == "=" and next_c == "=":
            tokens.append("==")
            token_values.append("")
            next_c = read()
        if c == "'" or c == '"':
            parsing_string = True
            parsing_string_delimiter = c
        continue
    token = token + c

# Interpreter.
pc = 0
variables = {}
block_level = 0


def read():
    global pc
    if pc >= len(tokens):
        return None, None
    ret = tokens[pc], token_values[pc]
    pc = pc + 1
    return ret


def peek():
    if pc >= len(tokens) - 1:
        return None, None
    return tokens[pc], token_values[pc]


def current():
    if pc > len(tokens) or pc < 1:
        return None, None
    return tokens[pc - 1], token_values[pc - 1]


def look_behind():
    if pc > len(tokens) or pc < 2:
        return None, None
    return tokens[pc - 2], token_values[pc - 2]


def pc_of_next_token_of_type(type):
    end_pc = pc
    while tokens[end_pc] != type:
        end_pc += 1
    return end_pc


def call_func():
    global pc
    _, func_name = current()
    assert func_name == "print" or func_name == "read" or func_name == "sizeof", f"Unknown function: {func_name}"
    value = expression(pc_of_next_token_of_type(")"))
    read()
    if func_name == "print":
        print(value)
    if func_name == "read":
        return stdin.buffer.read(1).decode()
    if func_name == "sizeof":
        size = str(len(value))
        return size


def expression(end_pc: int):
    expression_value = ""
    while pc != end_pc:
        cur_token, cur_value = read()
        if cur_token == "+":
            expression_value = expression_value + expression(pc + 1)
        elif cur_token == "==":
            expression_value = expression_value == expression(pc + 1)
        else:
            if cur_token == "string":
                expression_value = cur_value
            elif cur_token == "identifier" and peek()[0] == "(":
                expression_value = call_func()
            elif cur_token == "identifier":
                expression_value = variables[current()[1]]
    return expression_value


def assign():
    name = look_behind()[1]
    value = expression(pc_of_next_token_of_type("\n"))
    variables[name] = value


def skip_block():
    global block_level
    stop_block_level = block_level
    while pc < len(tokens):
        token, _ = read()
        if token == "if":
            block_level = block_level + 1
        if token == "end" or token == "else":
            block_level = block_level - 1
            if block_level < stop_block_level:
                if token == "else":
                    block_level = block_level + 1
                return
            if token == "else":
                block_level = block_level + 1


def if_then():
    global block_level
    condition = expression(pc_of_next_token_of_type("then"))
    block_level = block_level + 1
    if not condition:
        skip_block()
        token = current()[0]
        if token != "else":
            read()
            return
    stop_block_level = block_level
    while pc < len(tokens):
        step()
        token, _ = current()
        if token == "end" or token == "else":
            if block_level == stop_block_level:
                break
    if condition == True:
        if token == "else":
            block_level = block_level + 1
            skip_block()
    block_level = block_level - 1
    read()


def loop():
    global block_level, pc
    loop_start_pc = pc
    stop_block_level = block_level
    while pc < len(tokens):
        step()
        token, _ = current()
        # if token == "end":
        #     print("token", token, pc, block_level)
        if token == "end" and block_level == stop_block_level:
            pc = loop_start_pc


def break_loop():
    global pc
    pc = len(tokens)


def step():
    global block_level, pc
    # print(f"[{pc}]")
    token, value = read()
    if token == "identifier" and peek()[0] == "(":
        call_func()
    elif token == "loop":
        loop()
    elif token == "break":
        break_loop()
    elif token == "if":
        if_then()
    elif token == "=":
        assign()
    elif token == "--":
        while True:
            token = read()[0]
            if not token or token == "\n":
                break


while pc < len(tokens):
    step()
