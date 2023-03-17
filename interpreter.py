from sys import stdin

token = ""
last_token = ""
last_string = ""
last_identifier = ""
last_function = ""
execute_operator_at_line_end = ""
inside_string = ""

var1_name = ""
var1_value = ""

while True:
    c = stdin.buffer.read(1).decode()
    if not c:
        break
    if inside_string == "true":
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
        execute_operator_at_line_end = token
        token = ""
    elif c == " ":
        pass
    elif c == "\n":
        if execute_operator_at_line_end == "=":
            var1_name = last_identifier
            var1_value = last_string
            last_identifier = ""
            last_string = ""
    else:
        token = token + c

