let token = ""
let last_token = ""
let last_string_reg = ""
let function_call = ""
let parsing_string = "no"
let parsing_string_delimiter = ""
let assign_to_variable_at_eol = ""
let do_assign_to_variable_at_eol = ""
let do_create_to_variable_at_eol = ""

let string_contstants_id = ""
let reg_id = ""
let out_string_constants = ""
let out_alloc_local_variable = ""
let out_main = ""


loop do
    c = read()
    if c == "" then
        print('declare i32 @"printf"(i8* %".1", ...)')
        print('declare i8* @strcpy(i8* %dst, i8* %src)')
        print('define void @"main"()')
        print("{")
        print("entry:")
        print(out_alloc_local_variable)
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
            let size = sizeof(token)
            string_contstants_id = string_contstants_id + "s"
            out_string_constants = out_string_constants + '@"'
            out_string_constants = out_string_constants + string_contstants_id
            out_string_constants = out_string_constants + '" = internal constant ['
            out_string_constants = out_string_constants + size
            out_string_constants = out_string_constants + ' x i8] c"'
            out_string_constants = out_string_constants + token
            out_string_constants = out_string_constants + '00"\n'
            reg_id = reg_id + "1"
            last_string_reg = reg_id
            out_main = out_main + '%".' 
            out_main = out_main + reg_id
            out_main = out_main + '" = bitcast ['
            out_main = out_main + size
            out_main = out_main + ' x i8]* @"'
            out_main = out_main + string_contstants_id
            out_main = out_main + '" to i8*\n'
            token = ""
        else
            token = token + c
        end
    else
        if c == "(" then
            function_call = token
            token = ""
            c = ""
        end
        if c == "=" then
            assign_to_variable_at_eol = last_token
            do_assign_to_variable_at_eol = "true"
            c = ""
            last_token = ""
        end
        if c == " " then
            if token == "let" then
                do_create_to_variable_at_eol = "true"
                token = ""
            end
            last_token = token
            token = ""
            c = ""
        end
        if c == "\n" then
            if do_create_to_variable_at_eol == "true" then
                do_create_to_variable_at_eol = ""
                out_alloc_local_variable = out_alloc_local_variable + "%._"
                out_alloc_local_variable = out_alloc_local_variable + assign_to_variable_at_eol
                out_alloc_local_variable = out_alloc_local_variable + " = alloca [1024 x i8]\n"
                out_main = out_main + '%".' 
                out_main = out_main + assign_to_variable_at_eol
                out_main = out_main + '" = bitcast [1024 x i8]* %"._'
                out_main = out_main + assign_to_variable_at_eol
                out_main = out_main + '" to i8*\n'
            end
            if do_assign_to_variable_at_eol == "true" then
                reg_id = reg_id + "1"
                out_main = out_main + '%".'
                out_main = out_main + reg_id
                out_main = out_main + '" = call i8* (i8*, i8*) @"strcpy"(i8* %".'
                out_main = out_main + assign_to_variable_at_eol
                out_main = out_main + '", i8* %".'
                out_main = out_main + last_string_reg
                out_main = out_main + '")\n'
                do_assign_to_variable_at_eol = ""
            end
            token = ""
            c = ""
        end
        if c == ")" then
            let arg_reg = "" 
            if token == "" then
                arg_reg = last_string_reg
            else
                arg_reg = token
            end
            if function_call == "print" then
                reg_id = reg_id + "1"
                out_main = out_main + '%".'
                out_main = out_main + reg_id
                out_main = out_main + '" = call i32 (i8*, ...) @"printf"(i8* %".'
                out_main = out_main + arg_reg
                out_main = out_main + '")\n'
            end
            function_call = ""
            last_string_reg = ""
            token = ""
            c = ""
        end
        if c == '"' then
            parsing_string = "true"
            parsing_string_delimiter = c
            c = ""
        end
        token = token + c
    end
end
