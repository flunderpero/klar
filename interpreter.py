from sys import stdin

token = ""
last_token = ""
last_token_is_string = ""
is_string = "" 
assign_to_variable_at_eol = ""
do_assign_to_variable_at_eol = ""
execute_function_at_rparen = ""
do_execute_function_at_rparen = ""
expression_value = ""

var_name = ""
var_value = ""

while True:
    c = stdin.buffer.read(1).decode()
    if not c:
        break
    if is_string == "true":
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
        elif token == "print":
            execute_function_at_rparen = token
            do_execute_function_at_rparen = "true"
        elif token == "=":
            assign_to_variable_at_eol = last_token
            do_assign_to_variable_at_eol = "true"
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
        if do_assign_to_variable_at_eol == "true":
            var_name = assign_to_variable_at_eol
            var_value = expression_value
            expression_value = ""
            do_assign_to_variable_at_eol = ""
            assign_to_variable_at_eol = ""
    else:
        token += c
