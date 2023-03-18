token = ""
function_call = ""
parsing_string = "no"
parsing_string_delimiter = ""


loop do
    c = read()
    if c == "" then
        break
    end
    if parsing_string == "true" then
        if c == '"' then
            parsing_string = ""
            print("String constant")
            size = sizeof(token)
            print(size)
            print(token)
        else
            token = token + c
        end
    else
        if c == "(" then
            function_call = token
            token = ""
            c = ""
        end
        if c == " " then
            token = ""
            c = ""
        end
        if c == ")" then
            print("Function call")
            print(function_call )
        end
        if c == '"' then
            parsing_string = "true"
            parsing_string_delimiter = c
            c = ""
        end
        token = token + c
    end
end


