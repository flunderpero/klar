from sys import stdin

token = ""
last_token = ""
last_string = ""
last_identifier = ""
last_function = ""
last_expression_type = ""
last_expression_lhs = ""
execute_operator_at_line_end = ""
inside_string = ""
inside_if = ""
skip_to_end_token = ""

var1_name = ""
var1_value = ""

while True:
    c = stdin.buffer.read(1).decode()
    if not c:
        break
    if skip_to_end_token == "true":
        if c == "\n":
            if token == "end":
                skip_to_end_token = ""
            token = ""
        else:
            token = token + c
    elif inside_string == "true":
        if c == '"':
            last_string = token
            inside_string = ""
            token = ""
        else:
            token = token + c
    elif c == "(":
        if token == "print":
            last_function = token
        else:
            last_identifier = token
        token = ""
    elif c == ")":
        if last_function == "print":
            last_function = ""
            if last_string:
                print(last_string)
                last_string = ""
            elif last_token == var1_name:
                print(var1_value)
    elif c == '"':
        inside_string = "true"
    elif c == "=":
        if inside_if:
            if token == "=":
                token = ""
                last_expression_type = "=="
                if last_string:
                    last_expression_lhs = last_string
                    last_string = ""
                elif last_identifier == var1_name:
                    last_expression_lhs = var1_value
                    last_identifier = ""
                last_expression_lhs
            else:
                token = token + c
        else:
            execute_operator_at_line_end = token
            token = ""
    elif c == " ":
        if token == "if":
            inside_if = "true"
        elif token == "then":
            inside_if = ""
            if last_expression_type == "==":
                if last_string:
                    if last_string == last_expression_lhs:
                        pass
                    else:
                        skip_to_end_token = "true"
                    last_string = ""
        else:
            last_identifier = token
        token = ""
    elif c == "\n":
        if execute_operator_at_line_end == "=":
            var1_name = last_identifier
            var1_value = last_string
            last_identifier = ""
            last_string = ""
    else:
        token = token + c
