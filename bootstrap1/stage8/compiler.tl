-- Bootstrap stage 8
--
-- Changes in this stage:
--
-- - Use malloc to allocate memory instead of putting everything on the stack.
--   We still don't free anything though. :)
-- - Add support for functions returning lists (of string).
-- - Add support for functions returning structs.
--
-- But it will still be a bit messy.

--
-- The lexer turns the input into lists of token types and values.
--
fn lexer(mut token_types List<string>, mut token_values List<string>, mut constant_definitions string, mut string_constant_declarations string) void begin
    let c = ""
    let next_c = ""
    let c_is_int = 0
    let skip_to_eol = 0
    let add_token = 0
    let parsing_int = 0
    let parsing_string = 0
    let parsing_string_length = 0
    let parsing_string_delimiter = "" 
    let parsing_string_escape_sequence = 0
    let finish_token = 0
    let token_value = ""
    let token_type = ""
    let next_token_type = ""
    let next_token_value = ""
    let id_counter = 0
    let id = ""
    next_c = read()
    loop 
        c = next_c
        if c == "" then
            break
        end
        next_c = read()
        if skip_to_eol == 1 then
            if c == "\n" then
                skip_to_eol = 0
            end
            continue
        end
        if c == "0" then
            c_is_int = 1
        end
        if c == "1" then
            c_is_int = 1
        end
        if c == "2" then
            c_is_int = 1
        end
        if c == "3" then
            c_is_int = 1
        end
        if c == "4" then
            c_is_int = 1
        end
        if c == "5" then
            c_is_int = 1
        end
        if c == "6" then
            c_is_int = 1
        end
        if c == "7" then
            c_is_int = 1
        end
        if c == "8" then
            c_is_int = 1
        end
        if c == "9" then
            c_is_int = 1
        end
        if parsing_string == 1 then
        else
            if c_is_int == 1 then
                if token_value == "" then
                    parsing_int = 1 
                end
            end
        end
        if parsing_int == 1 then
            -- Detect whether we are done parsing.
            if c == " " then
                parsing_int = 0
            end
            if c == "." then
                parsing_int = 0
            end
            if c == "\n" then
                parsing_int = 0
            end
            if c == ")" then
                parsing_int = 0
            end
            if c == "," then
                parsing_int = 0
            end
            if parsing_int == 0 then
                -- We are done parsing, convert to an int.
                -- Generate that unique id.
                id_counter = id_counter + 1
                id = id_counter.to_string()
                constant_definitions.push("\n%.")
                constant_definitions.push(id)
                constant_definitions.push(" = alloca i32, align 4\n")
                constant_definitions.push("store i32 ")
                constant_definitions.push(token_value)
                constant_definitions.push(", ptr %.")
                constant_definitions.push(id)
                token_types.push("int")
                token_values.push(id)
                token_value = ""
                parsing_int = 0
            else
                token_value.push(c)
                continue
            end
        end
        c_is_int = 0

        if parsing_string == 1 then
            if c == parsing_string_delimiter then
                id_counter = id_counter + 1
                id = id_counter.to_string()
                string_constant_declarations.push("@.")
                string_constant_declarations.push(id)
                string_constant_declarations.push(" = internal constant [")
                string_constant_declarations.push(parsing_string_length.to_string())
                string_constant_declarations.push(' x i8] c"')
                string_constant_declarations.push(token_value)
                string_constant_declarations.push("\\")
                string_constant_declarations.push("00")
                string_constant_declarations.push('"\n')
                constant_definitions.push("\n%." )
                constant_definitions.push(id)
                constant_definitions.push(" = getelementptr [")
                constant_definitions.push(parsing_string_length.to_string())
                constant_definitions.push(" x i8], ptr @.")
                constant_definitions.push(id)
                constant_definitions.push(", i32 0, i32 0")
                token_types.push("string")
                token_values.push(id)
                token_value = ""
                parsing_string = 0
                parsing_string_escape_sequence = 0
            else
                -- We only follow some escape rules, enough so this
                -- compiler is able to compile itself.
                if parsing_string_escape_sequence == 1 then
                    if c == "n" then
                        -- This is a LLVM IR string escape sequence.
                        token_value.push("\\0a")
                        parsing_string_length = parsing_string_length + 1
                    end
                    if c == "\\" then
                        token_value.push("\\\\")
                        parsing_string_length = parsing_string_length + 1
                    end
                    parsing_string_escape_sequence = 0
                    continue
                end
                if c == '"' then
                    -- This is a LLVM IR string escape sequence.
                    token_value.push("\\22")
                    parsing_string_length = parsing_string_length + 1
                    parsing_string_escape_sequence = 0
                    continue
                end
                if c == "\\" then
                    parsing_string_escape_sequence = 1
                    continue
                end
                token_value.push(c)
                parsing_string_length = parsing_string_length + 1
            end
            continue
        end
        if c == '"' then
            parsing_string = 1
            parsing_string_delimiter = c
            parsing_string_length = 1
            continue
        end
        if c == "'" then
            parsing_string = 1
            parsing_string_delimiter = c
            parsing_string_length = 1
            continue
        end
        if c == " " then
            add_token = 1
            next_token_type = ""
        end
        if c == "\n" then
            add_token = 1
            next_token_type = "newline"
        end
        if c == "," then
            add_token = 1
            next_token_type = "comma"
        end
        if c == "+" then
            add_token = 1
            next_token_type = "add"
        end
        if c == "-" then
            add_token = 1
            if next_c == "-" then
                skip_to_eol = 1
            else
                next_token_type = "subtract"
            end
        end
        if c == "=" then
            add_token = 1
            if next_c == "=" then
                next_token_type = "eq"
                next_c = read()
            else
                next_token_type = "assign"
            end
        end
        if c == "." then
            add_token = 1
            next_token_type = "dot"
        end
        if c == "(" then
            add_token = 1
            next_token_type = "lparen"
        end
        if c == ")" then
            add_token = 1
            next_token_type = "rparen"
        end
        if add_token == 1 then
            add_token = 0
            if token_value == "" then
            else
                if token_value == "fn" then
                    token_types.push("function_declaration")
                    token_values.push("")
                else
                    if token_value == "return" then
                        token_types.push("return")
                        token_values.push("")
                    else
                        if token_value == "struct" then
                            token_types.push("struct_declaration")
                            token_values.push("")
                        else
                            token_types.push("identifier")
                            token_values.push(token_value)
                        end
                    end
                end
            end
            token_value = ""
            if next_token_type == "" then
            else
                token_types.push(next_token_type)
                token_values.push(next_token_value)
                next_token_type = ""
                next_token_value = ""
            end
        else
            token_value.push(c)
        end
    end
end

fn struct_member(obj_var string, obj_field_name string, struct_vars List<string>, struct_vars_type List<string>, struct_field1_name List<string>, struct_field1_type List<string>, struct_field2_name List<string>, struct_field2_type List<string>, struct_field3_name List<string>, struct_field3_type List<string>, struct_field4_name List<string>, struct_field4_type List<string>, struct_field5_name List<string>, struct_field5_type List<string>, id string, mut out string, mut out_expression_type string, mut out_expression_reg string) void begin
    let i = 0
    let type = ""
    let expression_type = ""
    let expression_reg = ""
    loop
        if i == struct_vars.length() then
            break
        end
        if struct_vars.at(i) == obj_var then
            break
        end
        i = i + 1
    end
    type = struct_vars_type.at(i)
    out.push("\n%.getstruct").push(id).push(" = getelementptr inbounds %").push(type)
    out.push(", ptr %.").push(obj_var).push(", i32 0, i32 ")
    if struct_field1_name.at(i) == obj_field_name then
        out.push("0")
        expression_type = struct_field1_type.at(i)
    end
    if struct_field2_name.at(i) == obj_field_name then
        out.push("1")
        expression_type = struct_field2_type.at(i)
    end
    if struct_field3_name.at(i) == obj_field_name then
        out.push("2")
        expression_type = struct_field3_type.at(i)
    end
    if struct_field4_name.at(i) == obj_field_name then
        out.push("3")
        expression_type = struct_field4_type.at(i)
    end
    if struct_field5_name.at(i) == obj_field_name then
        out.push("4")
        expression_type = struct_field5_type.at(i)
    end
    if expression_type == "List<string>" then
        expression_type = "list"
    end
    if expression_type == "i32" then
        out.push("\n%.getstruct_loadint").push(id)
        out.push(" = load i32, ptr %.getstruct").push(id)
        expression_reg = "getstruct_loadint"
        expression_reg.push(id)
        expression_type = "int"
    else
        expression_reg = "getstruct"
        expression_reg.push(id)
    end
    out_expression_reg.push(expression_reg)
    out_expression_type.push(expression_type)
end

fn size_of_struct(struct_name string, id string, mut out string) string begin
    -- This is a neat trick to get the size of a struct.
    -- We "simulate" an array of `struct_name` with two elements 
    -- and then take the offset of the second element as the size.
    -- Thanks Copilot. :)
    let res_reg = "sizeofstruct"
    res_reg.push(id)
    out.push("\n%.sizeofptr").push(id).push(" = getelementptr %").push(struct_name).push(", ptr null, i32 1")
    out.push("\n%.").push(res_reg).push(" = ptrtoint %").push(struct_name).push("* %.sizeofptr").push(id).push(" to i64")
    return res_reg
end

fn main() i32 begin
    let token_types = list()
    let token_values = list()
    let token_value = ""
    let token_type = ""
    let next_token_type = ""
    let next_token_value = ""

    -- These are just general helper variables.
    let s = ""
    let s2 = ""
    let s3 = ""
    let i = 0

    -- Variables used by the code generation.
    let id_counter = 0
    let token_index = 0
    let id = ""
    let string_constant_declarations = ""
    let constant_definitions = ""
    let out = ""
    let function_prelude = ""
    let function_name = ""
    let function_return_type = ""
    let function_params = ""
    let functions = list()
    let functions_return_type = list()
    let struct_name = ""
    let structs = list()
    let struct_field_name = ""
    let struct_field1_type = list()
    let struct_field2_type = list()
    let struct_field3_type = list()
    let struct_field4_type = list()
    let struct_field5_type = list()
    let struct_field1_name = list()
    let struct_field2_name = list()
    let struct_field3_name = list()
    let struct_field4_name = list()
    let struct_field5_name = list()
    let struct_vars = list()
    let struct_vars_type = list()
    let type = ""
    let expression_reg = ""
    let expression_type = ""
    let handle = ""
    let call_function = ""
    let call_args = ""
    let call_args_list = list()
    let string_vars = list()
    let int_vars = list()
    let list_vars = list()
    let expression_op = ""
    let expression_op_rhs_reg = ""
    let create_variable_at_eol = ""
    let assign_variable_at_eol = ""
    let obj_var = ""
    let obj_field_name = ""
    let obj_field_index = ""
    let obj_field_type = ""
    let nesting_types = list()
    let nesting_ids = list()
    let nesting_loop_ids = list()

    lexer(token_types, token_values, constant_definitions, string_constant_declarations)

    --
    -- Code generation.
    --
    print("declare i32 @puts(i8*)")
    print("declare i8* @strcpy(i8*, i8*)")
    print("declare i8* @strcat(i8*, i8*)")
    print("declare i8* @strstr(i8*, i8*)")
    print("declare i32 @strcmp(i8*, i8*)")
    print("declare i64 @strlen(i8*)")
    print("declare i32 @sprintf(i8*, i8*, ...)")
    print("declare i32 @read(i32, i8*, i32)")
    print("declare i8* @malloc(i64)")
    print("declare i32 @memset(i8*, i32, i32)")
    print('@int_to_string_sprintf_format = internal constant [3 x i8] c"%d\\00"')
    print("%List = type {")
    print("    i32, ; Length")
    print("    [10000 x ptr] ; Values")
    print("}")
    function_prelude = "\n%.read_buf = alloca [2 x i8]"
    function_prelude.push("\n%.read_buf_clear = call i32 @memset(ptr %.read_buf, i32 0, i32 2)")
    function_prelude.push("\n%.sprintf_buf = alloca [100 x i8]")
    function_prelude.push("\n%.int_to_string_sprintf_format = ")
    function_prelude.push("getelementptr [3 x i8], ptr @int_to_string_sprintf_format, i32 0, i32 0")

    token_index = 0
    loop 
        -- Read the current and the next token.
        if token_index == token_types.length() then
            break
        end
        token_type = token_types.at(token_index)
        token_value = token_values.at(token_index)
        token_index = token_index + 1
        if token_index == token_types.length() then
            next_token_type = ""
            next_token_value = ""
        else
            next_token_type = token_types.at(token_index)
            next_token_value = token_values.at(token_index)
        end
        id_counter = id_counter + 1
        id = id_counter.to_string()
        -- out.push("\n; TOKEN #")
        -- out.push(token_index.to_string())
        -- out.push(": token_type=")
        -- out.push(token_type)
        -- out.push(": token_value=")
        -- out.push(token_value)

        if token_type == "function_declaration" then
            print(out)
            out = ""
            string_vars.clear()
            int_vars.clear()
            list_vars.clear()
            struct_vars.clear()
            struct_vars_type.clear()
            function_name = next_token_value
            -- Skip to the parameter list and parse parameters.
            token_index = token_index + 2
            s = ""
            function_params = ""
            loop
                token_value = token_values.at(token_index)
                token_type = token_types.at(token_index)
                token_index = token_index + 1
                if token_type == "rparen" then
                    -- We are done parameter parsing.
                    break
                end
                if token_type == "comma" then
                    continue
                end
                if token_value == "mut" then
                    -- We ignore this modifier for now. All parameters are mutable.
                    continue
                end
                if s == "" then
                    s = ", "
                else
                    function_params.push(s)
                end
                type = token_values.at(token_index)
                if type == "string" then
                    type = "i8*"
                    string_vars.push(token_value)
                end
                if type == "i32" then
                    type = "i32"
                    int_vars.push(token_value)
                    out.push("\n%.").push(token_value).push(" = alloca i32")
                    out.push("\nstore i32 %.").push(token_value)
                    out.push("_, i32* %.").push(token_value)
                    token_value.push("_")
                end
                if type == "List<string>" then
                    type = "%List*"
                    list_vars.push(token_value)
                end
                if type in structs then
                    struct_vars.push(token_value)
                    struct_vars_type.push(type)
                    type = "ptr"
                end
                -- `type` `name`  
                function_params.push(type).push(" %.").push(token_value)
                token_index = token_index + 1
            end
            function_return_type = token_values.at(token_index)
            if function_return_type == "string" then
                function_return_type = "i8*"
            end
            if function_return_type == "List<string>" then
                function_return_type = "%List*"
            end
            if function_return_type in structs then
                s2 = "%"
                s2.push(function_return_type).push("*")
                function_return_type = s2
            end
            token_index = token_index + 2
            s = "\ndefine "
            s.push(function_return_type)
            s.push(" @").push(function_name)
            s.push("(")
            print(s)
            print(function_params)
            print(") {\n")
            print(function_prelude)
            print(constant_definitions)
            nesting_types.push("function")
            nesting_ids.push(function_name)
            functions.push(function_name)
            functions_return_type.push(function_return_type)
            continue
        end

        if token_type == "struct_declaration" then
            struct_name = next_token_value
            structs.push(struct_name)
            out.push("\n%").push(struct_name).push(" = type {\n")
            token_index = token_index + 3
            i = 0
            loop
                if i == 5 then
                    break
                end
                token_value = token_values.at(token_index)
                token_type = token_types.at(token_index)
                
                if token_value == "end" then
                    struct_field_name = ""
                    type = ""
                else
                    struct_field_name = token_value
                    token_index = token_index + 1
                    type = token_values.at(token_index)
                    out.push("    ")
                    if i == 0 then
                    else
                        out.push(", ")
                    end
                    if type == "string" then
                        out.push("i8*")
                    end
                    if type == "i32" then
                        out.push("i32")
                    end
                    if type == "List<string>" then
                        out.push("%List")
                    end
                    out.push(" ; ").push(struct_field_name).push("\n")
                    -- TODO: Add support for structs. 
                    token_index = token_index + 1
                    token_type = token_types.at(token_index)
                    if token_type == "newline" then
                        token_value = token_values.at(token_index)
                        -- out.push("\n; aaaaa ").push(token_value).push("\n")
                        token_index = token_index + 1 
                    end
                end
                if i == 0 then 
                    struct_field1_name.push(struct_field_name)
                    struct_field1_type.push(type)
                end
                if i == 1 then 
                    struct_field2_name.push(struct_field_name)
                    struct_field2_type.push(type)
                end
                if i == 2 then 
                    struct_field3_name.push(struct_field_name)
                    struct_field3_type.push(type)
                end
                if i == 3 then 
                    struct_field4_name.push(struct_field_name)
                    struct_field4_type.push(type)
                end
                if i == 4 then 
                    struct_field5_name.push(struct_field_name)
                    struct_field5_type.push(type)
                end
                i = i + 1
            end
            out.push("}\n")
            token_index = token_index + 1
            continue
        end

        if token_type == "return" then
            -- TODO: This allows only constants and variables to be returned.
            if function_return_type == "i32" then
                out.push("\n%_retvalue").push(" = load ")
                out.push(function_return_type).push(", ").push(function_return_type)
                out.push("* %.").push(next_token_value)
            else
                out.push("\n%_retvalue = bitcast ptr %.").push(next_token_value).push(" to ptr")
            end
            out.push("\nret ").push(function_return_type).push(" %_retvalue")
            token_index = token_index + 1
            continue
        end

        if next_token_type == "dot" then
            if token_type == "rparen" then
                -- We use the same `obj_var` when chaining methods.
            else
                if call_function == "" then
                    if obj_field_name == "" then
                        obj_var = token_value
                    end
                end
            end
            s = token_value
            token_index = token_index + 1
            token_value = token_values.at(token_index)
            if s in struct_vars then
                obj_var = s
                obj_field_name = token_value
                expression_op_rhs_reg = ""
                expression_type = ""
                struct_member(
                    obj_var, obj_field_name, struct_vars, struct_vars_type,
                    struct_field1_name, struct_field1_type,
                    struct_field2_name, struct_field2_type,
                    struct_field3_name, struct_field3_type,
                    struct_field4_name, struct_field4_type,
                    struct_field5_name, struct_field5_type,
                    id,
                    out, expression_type, expression_op_rhs_reg)
                token_index = token_index + 1
                token_type = token_types.at(token_index)
                if token_type == "assign" then
                    expression_reg = "getstruct"
                    expression_reg.push(id)
                    expression_op_rhs_reg = expression_reg
                    assign_variable_at_eol = expression_reg
                else
                    token_index = token_index - 1
                end
                continue
            end
            if obj_field_name == "" then
            else
                s = ""
                struct_member(
                    obj_var, obj_field_name, struct_vars, struct_vars_type,
                    struct_field1_name, struct_field1_type,
                    struct_field2_name, struct_field2_type,
                    struct_field3_name, struct_field3_type,
                    struct_field4_name, struct_field4_type,
                    struct_field5_name, struct_field5_type,
                    id,
                    out, s2, s)
                s = "getstruct"
                s.push(id)
            end
            if token_value == "to_string" then
                -- We shortcut this here, so we can use `int.to_string()` in
                -- string concatenations.
                out.push("\n%.tostrload")
                out.push(id)
                out.push(" = load i32, ptr %.")
                out.push(s)
                out.push("\n%.tostr")
                out.push(id)
                out.push(" = call i32 (i8*, i8*, ...) @sprintf(ptr %.sprintf_buf")
                out.push(", ptr %.int_to_string_sprintf_format, i32 %.tostrload")
                out.push(id)
                out.push(")")
                expression_op_rhs_reg = "sprintf_buf"
                expression_type = "string"
                token_index = token_index + 2
                continue
            end
            if token_value == "length" then
                if obj_var in struct_vars then
                    s3 = ""
                    s = ""
                    id.push("x")
                    struct_member(
                        obj_var, obj_field_name, struct_vars, struct_vars_type,
                        struct_field1_name, struct_field1_type,
                        struct_field2_name, struct_field2_type,
                        struct_field3_name, struct_field3_type,
                        struct_field4_name, struct_field4_type,
                        struct_field5_name, struct_field5_type,
                        id,
                        out, s3, s)
                else
                    if obj_var in list_vars then
                        s3 = "list"
                    end
                    if obj_var in string_vars then
                        s3 = "string"
                    end
                end
                -- We shortcut this here, so we can use `x.length()` in
                -- string concatenations.
                if s3 == "list" then
                    out.push("\n%.lenptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds %List, ptr %.")
                    out.push(s)
                    out.push(", i32 0")
                    out.push("\n%.loadlen")
                    out.push(id)
                    out.push(" = load i32, ptr %.lenptr")
                    out.push(id)
                    expression_op_rhs_reg = "loadlen"
                    expression_op_rhs_reg.push(id)
                end
                if s3 == "string" then
                    out.push("\n%.strlen").push(id).push(" = call i32 @strlen(i8* %.").push(s).push(")")
                    expression_op_rhs_reg = "strlen"
                    expression_op_rhs_reg.push(id)
                end
                expression_type = "int"
                token_index = token_index + 2
                continue
            end
            if token_value == "pop" then
                out.push("\n; CALLING method=pop")
                out.push(", obj_var=")
                out.push(s)
                -- First, decrement `length`.
                out.push("\n%.lenptr")
                out.push(id)
                out.push(" = getelementptr inbounds %List, ptr %.")
                out.push(s)
                out.push(", i32 0, i32 0")
                out.push("\n%.getlen_")
                out.push(id)
                out.push(" = load i32, ptr %.lenptr")
                out.push(id)
                out.push("\n%.declen")
                out.push(id)
                out.push(" = sub i32 %.getlen_")
                out.push(id)
                out.push(", 1\n")
                out.push("store i32 %.declen")
                out.push(id)
                out.push(", ptr %.lenptr")
                out.push(id)
                -- Then read what's at the end.
                out.push("\n%.valuesptr")
                out.push(id)
                out.push(" = getelementptr inbounds %List, ptr %.")
                out.push(s)
                out.push(", i32 0, i32 1")
                out.push("\n%.atptr")
                out.push(id)
                out.push(" = getelementptr inbounds [10000 x ptr], ptr %.valuesptr" )
                out.push(id)
                out.push(", i32 0, i32 %.declen")
                out.push(id)
                out.push("\n%.atload")
                out.push(id)
                out.push(" = load i8*, ptr %.atptr")
                out.push(id)
                expression_op_rhs_reg = "atload"
                expression_op_rhs_reg.push(id)
                expression_type = "string"
                token_index = token_index + 2
                continue
            end
        end

        if token_type == "identifier" then
            if token_value == "let" then
                create_variable_at_eol = next_token_value 
                assign_variable_at_eol = next_token_value
                -- Skip the next two tokens. We know that they have to be 
                -- an identifier and `assign`.
                token_index = token_index + 2 
                continue
            end
            -- Read variables.
            if token_value in string_vars then
                expression_op_rhs_reg = token_value
                expression_type = "string"
            end
            if token_value in int_vars then
                out.push("\n%.loadintvar_")
                out.push(id)
                out.push(" = load i32, ptr %.")
                out.push(token_value)
                expression_op_rhs_reg = "loadintvar_"
                expression_op_rhs_reg.push(id)
                expression_type = "int"
            end
            if token_value in list_vars then
                expression_op_rhs_reg = token_value
                expression_type = "list"
            end
            if token_value in struct_vars then
                expression_op_rhs_reg = token_value
                i = 0
                loop do
                    if struct_vars.at(i) == token_value then
                        break
                    end
                    i = i + 1
                end
                expression_type = struct_vars_type.at(i)
            end
            if token_value == "if" then
                continue
            end
            if token_value == "then" then
                nesting_types.push("if")
                nesting_ids.push(id)
                out.push("\nbr i1 %." )
                out.push(expression_reg)
                out.push(", label %if" )
                out.push(id)
                out.push(", label %else" )
                out.push(id)
                out.push("\nif")
                out.push(id)
                out.push(":")
                continue
            end
            if token_value == "loop" then
                nesting_types.push("loop")
                nesting_ids.push(id)
                nesting_loop_ids.push(id)
                out.push("\nbr label %loop" )
                out.push(id)
                out.push("\nloop")
                out.push(id)
                out.push(":")
                continue
            end
            if token_value == "break" then
                id = nesting_loop_ids.pop()
                nesting_loop_ids.push(id)
                out.push("\nbr label %loop_end" )
                out.push(id)
                continue
            end
            if token_value == "continue" then
                id = nesting_loop_ids.pop()
                nesting_loop_ids.push(id)
                out.push("\nbr label %loop" )
                out.push(id)
                continue
            end
            if token_value == "end" then
                s = nesting_types.pop()
                id = nesting_ids.pop()
                if s == "if" then
                    out.push("\nbr label %else")
                    out.push(id)
                    out.push("\nelse")
                    out.push(id)
                    out.push(":")
                end
                if s == "else" then
                    out.push("\nbr label %else_end")
                    out.push(id)
                    out.push("\nelse_end")
                    out.push(id)
                    out.push(":")
                end
                if s == "loop" then
                    nesting_loop_ids.pop()
                    out.push("\nbr label %loop")
                    out.push(id)
                    out.push("\nloop_end")
                    out.push(id)
                    out.push(":")
                end
                if s == "function" then
                    out.push("\n")
                    if function_return_type == "void" then
                        out.push("\nret void")
                    end
                    out.push("\n}")
                    expression_reg = ""
                end
                continue
            end
            if token_value == "else" then
                s = nesting_types.pop()
                id = nesting_ids.pop()
                nesting_types.push("else")
                nesting_ids.push(id)
                out.push("\nbr label %else_end")
                out.push(id)
                out.push("\nelse")
                out.push(id)
                out.push(":")
                continue
            end
        end
        if next_token_type == "assign" then
            assign_variable_at_eol = token_value
        end
        if next_token_type == "lparen" then
            call_function = token_value
            call_args = ""
            call_args_list.clear()
        end
        if token_type == "string" then
            expression_op_rhs_reg = token_value
            expression_type = "string"
        end
        if token_type == "int" then
            out.push("\n%.loadintconst_")
            out.push(id)
            out.push(" = load i32, ptr %.")
            out.push(token_value)
            expression_op_rhs_reg = "loadintconst_"
            expression_op_rhs_reg.push(id)
            expression_type = "int"
        end
        if token_type == "add" then
            expression_op = "add"
            continue
        end
        if token_type == "subtract" then
            expression_op = "subtract"
            continue
        end
        if token_type == "eq" then
            expression_op = "eq"
            continue
        end
        if token_type == "identifier" then
            if token_value == "in" then
                expression_op = "in"
                continue
            end
        end
        if expression_op == "" then
            if expression_op_rhs_reg == "" then
            else
                expression_reg = expression_op_rhs_reg
            end
        else
            -- Handle operators.
            if expression_op == "add" then
                out.push("\n\n; ADD expression_reg=")
                out.push(expression_reg)
                out.push(", expression_op_rhs_reg=")
                out.push(expression_op_rhs_reg)
                out.push(", expression_type=")
                out.push(expression_type)
                if expression_type == "string" then
                    if assign_variable_at_eol == expression_reg then
                        -- Concatenate with expression_op_rhs_reg.
                        out.push("\n%.addstr")
                        out.push(id)
                        out.push(" = call i8* (i8*, i8*) @strcat(ptr %.")
                        out.push(assign_variable_at_eol)
                        out.push(", ptr %.")
                        out.push(expression_op_rhs_reg)
                        out.push(")\n")
                        expression_reg = "addstr"
                        expression_reg.push(id)
                    end
                end
                if expression_type == "int" then
                    out.push("\n%.add")
                    out.push(id)
                    out.push(" = add i32 %.")
                    out.push(expression_reg)
                    out.push(", %.")
                    out.push(expression_op_rhs_reg)
                    expression_reg = "add"
                    expression_reg.push(id)
                end
            end
            if expression_op == "subtract" then
                out.push("\n\n; SUBTRACT expression_reg=")
                out.push(expression_reg)
                out.push(", expression_op_rhs_reg=")
                out.push(expression_op_rhs_reg)
                out.push(", expression_type=")
                out.push(expression_type)
                if expression_type == "int" then
                    out.push("\n%.sub")
                    out.push(id)
                    out.push(" = sub i32 %.")
                    out.push(expression_reg)
                    out.push(", %.")
                    out.push(expression_op_rhs_reg)
                    expression_reg = "sub"
                    expression_reg.push(id) 
                end
            end
            if expression_op == "eq" then
                if expression_type == "int" then
                    out.push("\n%.eqint")
                    out.push(id)
                    out.push(" = icmp eq i32 %.")
                    out.push(expression_reg)
                    out.push(", %.")
                    out.push(expression_op_rhs_reg )
                    expression_reg = "eqint"
                    expression_reg.push(id)
                end
                if expression_type == "string" then
                    out.push("\n%.strcmp")
                    out.push(id)
                    out.push(" = call i32 (i8*, i8*) @strcmp(ptr %.")
                    out.push(expression_reg)
                    out.push(", ptr %.")
                    out.push(expression_op_rhs_reg)
                    out.push(")\n")
                    out.push("%.eqstr")
                    out.push(id)
                    out.push(" = icmp eq i32 %.strcmp")
                    out.push(id)
                    out.push(", 0")
                    expression_reg = "eqstr"
                    expression_reg.push(id)
                end
            end
            if expression_op == "in" then
                -- `in
                if expression_type == "string" then
                    out.push("\n%.instrstr"  )
                    out.push(id)
                    out.push(" = call i8* (i8*, i8*) @strstr(ptr %.")
                    out.push(expression_op_rhs_reg)
                    out.push(", ptr %.")
                    out.push(expression_reg)
                    out.push(")\n")
                    out.push("%.instrcmp")
                    out.push(id)
                    out.push(" = icmp eq ptr %.instrstr")
                    out.push(id)
                    out.push(", null")
                    out.push("%.instr")
                    out.push(id)
                    out.push(" = sub i1 1, %.instrcmp")
                    out.push(id)
                    expression_reg = "instr"
                    expression_reg.push(id) 
                end
                if expression_type == "list" then
                    out.push("\n%.lenptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds %List, ptr %.")
                    out.push(expression_op_rhs_reg)
                    out.push(", i32 0, i32 0")
                    out.push("\n%.valuesptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds %List, ptr %.")
                    out.push(expression_op_rhs_reg)
                    out.push(", i32 0, i32 1")
                    out.push("\n%.in_pos")
                    out.push(id)
                    out.push(" = alloca i32\n")
                    out.push("%.getlen_")
                    out.push(id)
                    out.push(" = load i32, ptr %.lenptr")
                    out.push(id)
                    out.push("\nstore i32 %.getlen_")
                    out.push(id)
                    out.push(", ptr %.in_pos")
                    out.push(id)
                    out.push("\nbr label %in_loop")
                    out.push(id)
                    -- The loop.
                    out.push("\nin_loop")
                    out.push(id)
                    out.push(":\n")
                    out.push("%.idx_load")
                    out.push(id)
                    out.push(" = load i32, ptr %.in_pos")
                    out.push(id)
                    out.push("\n%.idx")
                    out.push(id)
                    out.push(" = sub i32 %.idx_load"  )
                    out.push(id)
                    out.push(", 1"  )
                    out.push("\n%.idxcmp")
                    out.push(id)
                    out.push(" = icmp eq i32 %.idx")
                    out.push(id)
                    out.push(", -1\n")
                    out.push("br i1 %.idxcmp" )
                    out.push(id)
                    out.push(", label %in_end" )
                    out.push(id)
                    out.push(", label %cont" )
                    out.push(id)
                    out.push("\ncont")
                    out.push(id)
                    out.push(":\n")
                    out.push("store i32 %.idx")
                    out.push(id)
                    out.push(", ptr %.in_pos")
                    out.push(id)
                    -- Read what's at the current position.
                    out.push("\n%.atptr")
                    out.push(id)
                    out.push(" = getelementptr [10000 x ptr], ptr %.valuesptr" )
                    out.push(id)
                    out.push(", i32 0, i32 %.idx")
                    out.push(id)
                    out.push("\n%.atload")
                    out.push(id)
                    out.push(" = load i8*, ptr %.atptr")
                    out.push(id)
                    out.push("\n%.strcmp")
                    out.push(id)
                    out.push(" = call i32 (i8*, i8*) @strcmp(ptr %.atload")
                    out.push(id)
                    out.push(", ptr %.")
                    out.push(expression_reg)
                    out.push(")\n")
                    out.push("%.strcmpb")
                    out.push(id)
                    out.push(" = icmp eq i32 %.strcmp")
                    out.push(id)
                    out.push(", 0\n")
                    out.push("br i1 %.strcmpb" )
                    out.push(id)
                    out.push(", label %in_end" )
                    out.push(id)
                    out.push(", label %in_loop" )
                    out.push(id)
                    out.push("\n")
                    out.push("in_end")
                    out.push(id)
                    out.push(":")
                    out.push("%.listin")
                    out.push(id)
                    out.push(" = sub i1 1, %.idxcmp")
                    out.push(id)
                    expression_reg = "listin"
                    expression_reg.push(id)
                end
            end
            expression_op = ""
        end
        expression_op_rhs_reg = ""

        -- Handle function parameters.
        if next_token_type == "rparen" then
            handle = "call_arg"
        end
        if next_token_type == "comma" then
            handle = "call_arg"
        end
        if handle == "call_arg" then
            if call_function == "" then
            else
                if expression_reg == "" then
                else
                    out.push("\n; SETTING CALLARGS: expression_reg=")
                    out.push(expression_reg)
                    out.push(", call_function=")
                    out.push(call_function)
                    s = "" 
                    if call_args == "" then
                    else
                        call_args.push(", ")
                    end
                    if expression_type == "int" then
                        s.push("i32 ")
                    else
                        s.push("ptr ")
                    end
                    s.push("%.")
                    s.push(expression_reg)
                    call_args_list.push(s)
                    call_args.push(s)
                    expression_reg = ""
                end
            end
            handle = ""
        end

        -- Call functions.
        if token_type == "rparen" then
            out.push("\n; CALLING: call_function=")
            out.push(call_function)
            out.push(", call_args=")
            out.push(call_args)
            if call_function == "print" then
                out.push("\n; PRINT call_args=")
                out.push(call_args)
                out.push(", expression_type=")
                out.push(expression_type)
                out.push("\n%.puts")
                out.push(id)
                out.push(" = call i32 (i8*) @puts(")
                out.push(call_args)
                out.push(")")
            end
            if call_function == "list" then
                s = size_of_struct("List", id, out)
                out.push("\n%.list")
                out.push(id)
                out.push(" = call i8* @malloc(i64 %.").push(s).push(")")
                -- Set length to 0.
                out.push("\nstore i32 0, ptr %.list")
                out.push(id)
                s2 = id
                -- Push all call_args to the list.
                i = 0
                loop
                    if i == call_args_list.length() then
                        break
                    end
                    s = call_args_list.at(i)
                    i = i + 1
                    id_counter = id_counter + 1
                    id = id_counter.to_string()
                    -- This code is straight up copied from the `push` function above.
                    -- First, copy the current expression value (which is expected to be a string).
                    -- Get the current length of the array.
                    out.push("\n%.lenptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds %List, ptr %.list")
                    out.push(s2)
                    out.push(", i32 0, i32 0")
                    out.push("\n%.pushlen_")
                    out.push(id)
                    out.push(" = load i32, ptr %.lenptr")
                    out.push(id)
                    -- And store the copied value at the index.
                    out.push("\n%.valuesptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds %List, ptr %.list")
                    out.push(s2)
                    out.push(", i32 0, i32 1")
                    out.push("\n%.elmptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds [10000 x ptr], ptr %.valuesptr" )
                    out.push(id)
                    out.push(", i32 0, i32 %.pushlen_")
                    out.push(id)
                    out.push("\nstore ")
                    out.push(s)
                    out.push(", ptr %.elmptr")
                    out.push(id)
                    -- At last, increment the length of the array.
                    out.push("\n%.inclen")
                    out.push(id)
                    out.push(" = add i32 %.pushlen_")
                    out.push(id)
                    out.push(", 1\n")
                    out.push("store i32 %.inclen")
                    out.push(id)
                    out.push(", ptr %.lenptr")
                    out.push(id)
                end
                expression_type = "list"
                expression_reg = "list"
                expression_reg.push(s2)
            end
            if call_function == "read" then
                -- Read from stdin. Evaluate to "" if we are at the end of input.
                out.push("\n%.read")
                out.push(id)
                out.push(" = call i32 (i32, i8*, i32) @read(i32 0, ptr %.read_buf, i32 1)\n")
                out.push("%.read")
                out.push(id)
                out.push("_test = icmp eq i32 %.read")
                out.push(id)
                out.push(" , 0\n")
                out.push("br i1 %.read")
                out.push(id)
                out.push("_test, label %b")
                out.push(id)
                out.push("_true, label %b")
                out.push(id)
                out.push("_end\nb")
                out.push(id)
                out.push("_true:\n%b")
                out.push(id)
                out.push("_clear = call i32 @memset(ptr %.read_buf, i32 0, i32 2)\n")
                out.push("br label %b")
                out.push(id)
                out.push("_end\nb")
                out.push(id)
                out.push("_end:")
                expression_reg = "read_buf"
                expression_type = "string"
            end
            if call_function == "push" then
                if obj_var in struct_vars then
                    s = ""
                    s2 = ""
                    struct_member(
                        obj_var, obj_field_name, struct_vars, struct_vars_type,
                        struct_field1_name, struct_field1_type,
                        struct_field2_name, struct_field2_type,
                        struct_field3_name, struct_field3_type,
                        struct_field4_name, struct_field4_type,
                        struct_field5_name, struct_field5_type,
                        id,
                        out, s, s2)
                    obj_var = s2
                else
                    if obj_var in list_vars then
                        s = "list"
                    end
                    if obj_var in string_vars then
                        s = "string"
                    end
                end
                if s == "list" then
                    -- First, copy the current expression value (which is expected to be a string).
                    out.push("\n%.pushcopy")
                    out.push(id)
                    out.push(" = alloca [500 x i8]\n" )
                    out.push("%.pushstrcopy")
                    out.push(id)
                    out.push(" = call i8* (i8*, i8*) @strcpy(ptr %.pushcopy")
                    out.push(id)
                    out.push(", ")
                    out.push(call_args)
                    out.push(")")
                    -- Get the current length of the array.
                    out.push("\n%.lenptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds %List, ptr %.")
                    out.push(obj_var)
                    out.push(", i32 0, i32 0")
                    out.push("\n%.pushlen_")
                    out.push(id)
                    out.push(" = load i32, ptr %.lenptr")
                    out.push(id)
                    -- And store the copied value at the index.
                    out.push("\n%.valuesptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds %List, ptr %.")
                    out.push(obj_var)
                    out.push(", i32 0, i32 1")
                    out.push("\n%.elmptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds [10000 x ptr], ptr %.valuesptr" )
                    out.push(id)
                    out.push(", i32 0, i32 %.pushlen_")
                    out.push(id)
                    out.push("\nstore ptr %.pushcopy")
                    out.push(id)
                    out.push(", ptr %.elmptr")
                    out.push(id)
                    -- At last, increment the length of the array.
                    out.push("\n%.inclen")
                    out.push(id)
                    out.push(" = add i32 %.pushlen_")
                    out.push(id)
                    out.push(", 1\n")
                    out.push("store i32 %.inclen")
                    out.push(id)
                    out.push(", ptr %.lenptr")
                    out.push(id)
                end
                if s == "string" then
                    out.push("\n%.strpush")
                    out.push(id)
                    out.push(" = call i8* (i8*, i8*) @strcat(ptr %.")
                    out.push(obj_var)
                    out.push(", " )
                    out.push(call_args)
                    out.push(")\n")
                    expression_reg = "strpush"
                    expression_reg.push(id)
                    expression_type = "string"
                end
            end
            if call_function == "at" then
                if obj_var in struct_vars then
                    s = ""
                    s2 = ""
                    struct_member(
                        obj_var, obj_field_name, struct_vars, struct_vars_type,
                        struct_field1_name, struct_field1_type,
                        struct_field2_name, struct_field2_type,
                        struct_field3_name, struct_field3_type,
                        struct_field4_name, struct_field4_type,
                        struct_field5_name, struct_field5_type,
                        id,
                        out, s, s2)
                    obj_var = s2
                else
                    if obj_var in list_vars then
                        s = "list"
                    end
                    if obj_var in string_vars then
                        s = "string"
                    end
                end
                if s == "list" then
                    out.push("\n%.valuesptr")
                    out.push(id)
                    out.push(" = getelementptr inbounds %List, ptr %.")
                    out.push(obj_var)
                    out.push(", i32 0, i32 1")
                    out.push("\n%.atptr")
                    out.push(id)
                    out.push(" = getelementptr [10000 x ptr], ptr %.valuesptr")
                    out.push(id)
                    out.push(", i32 0, ")
                    out.push(call_args)
                    out.push("\n%.atload")
                    out.push(id)
                    out.push(" = load i8*, ptr %.atptr")
                    out.push(id)
                    expression_reg = "atload"
                    expression_reg.push(id)
                    expression_type = "string"
                end
                if s == "string" then
                    out.push("\n%.atptr").push(id)
                    out.push(" = getelementptr inbounds i8, i8* %.").push(obj_var).push(", ").push(call_args)
                    out.push("\n%.atload").push(id)
                    out.push(" = load i8, i8* %.atptr").push(id)
                    out.push("\n%.attmp")
                    out.push(id)
                    out.push(" = call i8* @malloc(i32 500)")
                    out.push("\n%.attmpclear").push(id).push("= call i32 @memset(ptr %.attmp").push(id)
                    out.push(", i32 0, i32 500")
                    out.push(")")
                    out.push("\nstore i8 %.atload").push(id).push(", i8* %.attmp").push(id)
                    expression_reg = "attmp"
                    expression_reg.push(id)
                    expression_type = "string"
                end
            end
            if call_function == "clear" then
                if obj_var in struct_vars then
                    s = ""
                    s2 = ""
                    struct_member(
                        obj_var, obj_field_name, struct_vars, struct_vars_type,
                        struct_field1_name, struct_field1_type,
                        struct_field2_name, struct_field2_type,
                        struct_field3_name, struct_field3_type,
                        struct_field4_name, struct_field4_type,
                        struct_field5_name, struct_field5_type,
                        id,
                        out, s, s2)
                    obj_var = s2
                end
                -- Just set `length` to 0.
                out.push("\n%.lenptr")
                out.push(id)
                out.push(" = getelementptr inbounds %List, ptr %.")
                out.push(obj_var)
                out.push(", i32 0, i32 0")
                out.push("\nstore i32 0")
                out.push(", ptr %.lenptr")
                out.push(id)
                expression_reg = "" 
            end
            if call_function in functions then
                -- First find the return type of the function.
                i = 0
                loop
                    s = functions.at(i)
                    if s == call_function then
                        s = functions_return_type.at(i)
                        break
                    end
                    i = i + 1
                end
                out.push("\n")
                i = 0
                loop
                    if i == structs.length() then
                        break
                    end
                    s2 = structs.at(i)
                    s3 = "%"
                    s3.push(s2)
                    if s3 == s then
                        expression_type = structs.at(i)
                        expression_reg = "ret_struct"
                        expression_reg.push(id)
                        break
                    end
                    i = i + 1
                end
                if s == "void" then
                else
                    out.push("%.call_").push(id).push(" = ")
                end
                out.push("call ").push(s).push(" @")
                out.push(call_function)
                out.push("(").push(call_args).push(")")
                if s == "i32" then
                    expression_type = "int" 
                end
                if s == "i8*" then
                    expression_type = "string" 
                end
                if s == "%List*" then
                    expression_type = "list" 
                else
                    if "%" in s then
                        i = 1
                        s2 = ""
                        loop
                            if i == s.length() then
                                break
                            end
                            s3 = s.at(i)
                            if s3 == "*" then
                                break
                            end
                            s2.push(s3)
                            i = i + 1
                        end
                        expression_type = s2
                    end
                end
                if s == "void" then
                    expression_type = ""
                    expression_reg = ""
                else
                    expression_reg = "call_"
                    expression_reg.push(id)
                end
            end
            if call_function in structs then
                -- Create a new struct.
                expression_type = call_function
            end
            call_function = ""
        end

        -- Create and assign variables. 
        if next_token_type == "newline" then
            obj_var = ""
            obj_field_name = ""
            s = ""
            if assign_variable_at_eol == "" then
            else
                if expression_reg == "" then
                else
                    s = "assign"
                end
            end
            if create_variable_at_eol == "" then
            else
                -- Create variable.
                if expression_type == "int" then
                    out.push("\n%.")
                    out.push(create_variable_at_eol)
                    out.push(" = alloca i32, align 4")
                    int_vars.push(create_variable_at_eol)
                end
                if expression_type in structs then
                    -- This is a struct.
                    if s == "assign" then
                        out.push("\n%.").push(create_variable_at_eol).push(" = bitcast ptr %.").push(expression_reg).push(" to ptr")
                    else
                        s = size_of_struct(expression_type, id, out)
                        out.push("\n%.").push(create_variable_at_eol)
                        out.push(" = call i8* @malloc(i64 %.").push(s).push(")")
                    end
                    struct_vars.push(create_variable_at_eol)
                    struct_vars_type.push(expression_type)
                end
                if expression_type == "list" then
                    list_vars.push(create_variable_at_eol)
                    out.push("\n%.").push(create_variable_at_eol).push(" = bitcast ptr %.").push(expression_reg).push(" to ptr")
                end
                if expression_type == "string" then
                    -- We don't care about buffer overruns as well at this stage.
                    -- Let's make the variables big enough for now.
                    -- Nobody would ever want to store more than 500 bytes in 
                    -- a variable anyway.
                    i = 500
                    if create_variable_at_eol == "string_constant_declarations" then
                        -- Oops, found that one variable that wants to be bigger
                        -- than 500 bytes. :-)
                        i = 100000
                    end
                    if create_variable_at_eol == "constant_definitions" then
                        -- ... and another one.
                        i = 100000
                    end
                    if create_variable_at_eol == "out" then
                        -- ... and another one.
                        i = 100000
                    end
                    out.push("\n; CREATE STRING: var=")
                    out.push(create_variable_at_eol)
                    out.push(", size=")
                    out.push(i.to_string())
                    out.push("\n%.").push(create_variable_at_eol)
                    out.push(" = call i8* @malloc(i64 ")
                    out.push(i.to_string())
                    out.push(")\n%.").push(create_variable_at_eol)
                    out.push("_clear = call i32 @memset(ptr %.")
                    out.push(create_variable_at_eol)
                    out.push(", i32 0, i32 ")
                    out.push(i.to_string())
                    out.push(")")
                    string_vars.push(create_variable_at_eol)
                end
                create_variable_at_eol = ""
            end
            if assign_variable_at_eol == "" then
            else
                out.push("\n; ASSIGN assign_variable_at_eol=")
                out.push(assign_variable_at_eol)
                out.push(", expression_reg=")
                out.push(expression_reg)
                out.push(", expression_type=")
                out.push(expression_type)
                if expression_type == "int" then
                    out.push("\nstore i32 %.")
                    out.push(expression_reg)
                    out.push(", ptr %.")
                    out.push(assign_variable_at_eol)
                end
                if expression_type == "string" then
                    out.push("\n%.strcpy")
                    out.push(id)
                    out.push(" = call i8* (i8*, i8*) @strcpy(ptr %.")
                    out.push(assign_variable_at_eol)
                    out.push(", ptr %.")
                    out.push(expression_reg)
                    out.push(")")
                end
                expression_reg = ""
                expression_type = ""
                assign_variable_at_eol = ""
            end
        end
        if out == "" then
        else
            print(out)
            out = ""
        end
    end

    if out == "" then
    else
        print(out)
        out = ""
    end

    print(string_constant_declarations)
    return 0
end
