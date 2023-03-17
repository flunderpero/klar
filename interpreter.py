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
do_skip_to_end_statement = "" 
expression_value = ""

var_name = ""
var_value = ""

while True:
    c = stdin.buffer.read(1).decode()
    if not c:
        break
    if do_skip_to_end_statement == "true":
        if c == "\n":
            if token == "end":
                do_skip_to_end_statement = ""
            token = ""
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
                do_skip_to_end_statement = "true"
            do_evaluate_if_statement_at_eol = ""
        token = ""
    else:
        token += c
