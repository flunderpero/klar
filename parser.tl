token = ""
function_call = ""
function_call_arg_reg = ""
parsing_string = "no"
parsing_string_delimiter = ""

string_contstants_id = ""
reg_id = ""
out_string_constants = ""
out_main = ""


loop do
    c = read()
    if c == "" then
        print('declare i32 @"printf"(i8* %".1", ...)')
        print('define void @"main"()')
        print("{")
        print("entry:")
        print(out_main)
        print("ret void")
        print("}")
        print(out_string_constants)
        break
    end
    if parsing_string == "true" then
        if c == '"' then
            parsing_string = ""
            token = token + "\"
            size = sizeof(token)
            string_contstants_id = string_contstants_id + "s"
            out_string_constants = out_string_constants + '@"'
            out_string_constants = out_string_constants + string_contstants_id
            out_string_constants = out_string_constants + '" = internal constant ['
            out_string_constants = out_string_constants + size
            out_string_constants = out_string_constants + ' x i8] c"'
            out_string_constants = out_string_constants + token
            out_string_constants = out_string_constants + '00"\n'
            reg_id = reg_id + "1"
            function_call_arg_reg = reg_id
            out_main = '%".' 
            out_main = out_main + reg_id
            out_main = out_main + '" = bitcast ['
            out_main = out_main + size
            out_main = out_main + ' x i8]* @"'
            out_main = out_main + string_contstants_id
            out_main = out_main + '" to i8*\n'
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
            if function_call == "print" then
                reg_id = reg_id + "1"
                out_main = out_main + '%".'
                out_main = out_main + reg_id
                out_main = out_main + '" = call i32 (i8*, ...) @"printf"(i8* %".'
                out_main = out_main + function_call_arg_reg
                out_main = out_main + '")'
            end
        end
        if c == '"' then
            parsing_string = "true"
            parsing_string_delimiter = c
            c = ""
        end
        token = token + c
    end
end


