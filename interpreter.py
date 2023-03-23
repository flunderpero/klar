"""
    A super naive interpreter of our language that restricts itself
    to a small subset of Python language features used. 
    
    This is the blueprint to how `compiler.tl` is implemented.

    This interpreter is superceeded 
"""
from sys import argv, stdin

token = ""
last_token = ""
last_token_is_string = ""
do_evaluate_token = ""
is_string = "" 
string_delimiter = ""
assign_to_variable_at_eol = ""
do_assign_to_variable_at_eol = ""
execute_function_at_rparen = ""
do_execute_function_at_rparen = ""
concatenate_expression_at_eol = ""
do_concatenate_expression_at_eol = ""
equals_if_expression_value = ""
do_evaluate_if_statement_at_eol = ""
skip_if_level = 0
do_execute_else_branch = ""
expression_value = ""
do_record_loop = ""
record_loop = ""
playing_loop = ""
c = ""
last_c = ""
do_skip_to_eol = ""

variables = {}

buffer = open(argv[1])

def read():
    global playing_loop
    c = buffer.read(1)
    if c == "":
        if playing_loop:
            c = playing_loop[0]
            playing_loop = playing_loop[1:]
        elif record_loop:
            playing_loop = record_loop[1:]
            c = record_loop[0]
    return c

while True:
    if do_evaluate_token == "true":
        if token == "":
            pass
        elif token == "--":
            do_skip_to_eol = "true"
        elif token == "loop":
            do_record_loop = "start_at_eol" 
        elif token == "if":
            do_evaluate_if_statement_at_eol = "true"
        elif token == "+":
            do_concatenate_expression_at_eol = "true"
            concatenate_expression_at_eol = expression_value
        elif token == "=":
            assign_to_variable_at_eol = last_token
            do_assign_to_variable_at_eol = "true"
        elif token == "==":
            equals_if_expression_value = expression_value
        elif token in variables:
            expression_value = variables[token]
        if c == "\n":
            if do_concatenate_expression_at_eol == "true":
                expression_value = concatenate_expression_at_eol + expression_value 
                do_concatenate_expression_at_eol = ""
                concatenate_expression_at_eol = ""
            if do_assign_to_variable_at_eol == "true":
                variables[assign_to_variable_at_eol] = expression_value
                expression_value = ""
                do_assign_to_variable_at_eol = ""
                assign_to_variable_at_eol = ""
            if do_evaluate_if_statement_at_eol == "true":
                if expression_value == equals_if_expression_value:
                    pass
                else:
                    skip_if_level = 1
                do_evaluate_if_statement_at_eol = ""
            if token == "else":
                skip_if_level = 1
            if token == "break":
                break
            if do_record_loop == "start_at_eol":
                do_record_loop = "true"
                record_loop = ""
        if c == "(":
            execute_function_at_rparen = token
            do_execute_function_at_rparen = "true"
        if c == ")":
            if do_execute_function_at_rparen == "true":
                if execute_function_at_rparen == "print":
                    print(expression_value) 
                    expression_value = ""
                if execute_function_at_rparen == "read":
                    expression_value = stdin.buffer.read(1).decode()
                if execute_function_at_rparen == "sizeof":
                    expression_value = str(len(expression_value))
                execute_function_at_rparen = ""
                do_execute_function_at_rparen = ""
        last_token = token
        last_token_is_string = ""
        token = ""
        do_evaluate_token = ""
    c = read()
    if c == "":
        break
    if do_skip_to_eol == "true":
        if c == "\n":
            do_skip_to_eol = ""
        c = ""
    if do_record_loop == "true":
        record_loop = record_loop + c
    if skip_if_level > 0:
        if c == "\n":
            if token == "end":
                skip_if_level = skip_if_level - 1
            if token == "else":
                if skip_if_level == 1:
                    skip_if_level = 0
            token = ""
        else:
            if c == " ":
                if token == "if":
                    skip_if_level = skip_if_level + 1
                token = ""
            else:
                token = token + c
    elif is_string == "true":
        if c == string_delimiter:
            if last_c == "\\":
                token = token + last_c
            last_token = token
            last_token_is_string = "true"
            expression_value = token
            token = ""
            is_string = ""
        else:
            if last_c == "\\":
                if c == "n":
                    c = chr(10)
                else:
                    token = token + last_c
                token = token + c
            else:
                if c == "\\":
                    pass
                else:
                    token = token + c
            last_c = c
    elif c == '"':
        is_string = "true"
        string_delimiter = c
        last_c = ""
    elif c == "'":
        is_string = "true"
        string_delimiter = c
        last_c = ""
    elif c == " ":
        do_evaluate_token = "true"
    elif c == "(":
        do_evaluate_token = "true"
    elif c == ")":
        do_evaluate_token = "true"
    elif c == "\n":
        do_evaluate_token = "true"
    else:
        token += c
