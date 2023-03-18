from sys import stdin

token = ""
last_token = ""
last_token_is_string = ""
is_string = "" 
assign_to_variable_at_eol = ""
do_assign_to_variable_at_eol = ""
execute_function_at_rparen = ""
do_execute_function_at_rparen = ""
concatenate_expression_at_eol = ""
do_concatenate_expression_at_eol = ""
equals_if_expression_value = ""
do_evaluate_if_statement_at_eol = ""
do_skip_to_end_or_else_statement = "" 
skip_if_level_1 = ""
skip_if_level_2 = ""
skip_if_level_3 = ""
do_execute_else_branch = ""
expression_value = ""
do_record_loop = ""
record_loop = ""
playing_loop = ""

var_name = ""
var_value = ""

def read():
    global playing_loop
    c = stdin.buffer.read(1).decode()
    if c == "":
        if playing_loop:
            c = playing_loop[0]
            playing_loop = playing_loop[1:]
        elif record_loop:
            playing_loop = record_loop[1:]
            c = record_loop[0]
    return c

while True:
    c = read()
    if c == "":
        break
    if do_record_loop == "true":
        record_loop = record_loop + c
    if do_skip_to_end_or_else_statement == "true":
        if c == "\n":
            if token == "if":
                if skip_if_level_1 == "true":
                    if skip_if_level_2 == "true":
                        skip_if_level_3 = "true"
                    else: 
                        skip_if_level_2 = "true"
                else:
                    skip_if_level_1 = "true"
            if token == "end":
                if skip_if_level_3 == "true":
                    skip_if_level_3 = ""
                elif skip_if_level_2 == "true":
                    skip_if_level_2 = ""
                elif skip_if_level_1 == "true":
                    skip_if_level_1 = ""
                else:
                    do_skip_to_end_or_else_statement = ""
            if token == "else":
                if skip_if_level_3 == "true":
                    skip_if_level_3 = ""
                elif skip_if_level_2 == "true":
                    skip_if_level_2 = ""
                elif skip_if_level_1 == "true":
                    skip_if_level_1 = ""
                else:
                    do_skip_to_end_or_else_statement = ""
            token = ""
        else:
            if c == " ":
                pass
            else:
                token = token + c
    elif is_string == "true":
        if c == '"':
            last_token = token
            last_token_is_string = "true"
            expression_value = token
            token = ""
            is_string = ""
        else:
            token += c
    elif c == '"':
        is_string = "true"
    elif c == " ":
        if token == "":
            pass
        elif token == "loop":
            do_record_loop = "start_at_eol" 
        elif token == "if":
            do_evaluate_if_statement_at_eol = "true"
        elif token == "+":
            do_concatenate_expression_at_eol = "true"
            concatenate_expression_at_eol = expression_value
        elif token == "print":
            execute_function_at_rparen = token
            do_execute_function_at_rparen = "true"
        elif token == "=":
            assign_to_variable_at_eol = last_token
            do_assign_to_variable_at_eol = "true"
        elif token == "==":
            equals_if_expression_value = expression_value
        elif token == var_name:
            expression_value = var_value
        last_token = token
        last_token_is_string = ""
        token = ""
    elif c == "(":
        pass
    elif c == ")":
        if do_execute_function_at_rparen == "true":
            if execute_function_at_rparen == "print":
                print(expression_value) 
            execute_function_at_rparen = ""
            do_execute_function_at_rparen = ""
            expression_value = ""
    elif c == "\n":
        if do_concatenate_expression_at_eol == "true":
            expression_value = concatenate_expression_at_eol + expression_value 
            do_concatenate_expression_at_eol = ""
            concatenate_expression_at_eol = ""
        if do_assign_to_variable_at_eol == "true":
            var_name = assign_to_variable_at_eol
            var_value = expression_value
            expression_value = ""
            do_assign_to_variable_at_eol = ""
            assign_to_variable_at_eol = ""
        if do_evaluate_if_statement_at_eol == "true":
            if expression_value == equals_if_expression_value:
                pass
            else:
                do_skip_to_end_or_else_statement = "true"
            do_evaluate_if_statement_at_eol = ""
        if token == "else":
            do_skip_to_end_or_else_statement = "true"
        if token == "break":
            break
        if do_record_loop == "start_at_eol":
            do_record_loop = "true"
            record_loop = ""
        token = ""
    else:
        token += c
