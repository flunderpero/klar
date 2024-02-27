-- Bootstrap stage 4
--
-- This implementation cleans up a bit using the integer
-- type added by the previous stage and adds:
-- 
-- - lists of strings
-- - multiple (but not nested) loops
-- - continue statement
-- 
-- Things will get better in the next stages. :^)

-- All variables need to be declared at the top.

let out_string_constants = ""
let assign_to_variable_at_eol = ""
let do_assign_to_variable_at_eol = ""
let var_size = ""
let c = ""
let last_c = ""
let token = ""
let string_size = "."
let last_token = ""
let last_expression_reg = ""
let last_expression_type = ""
let function_call = ""
let parsing_string = ""
let parsing_string_delimiter = ""
let parsing_int = ""
let c_is_int = ""
let size = ""
let do_create_to_variable_at_eol = ""
let add_at_eol_reg = ""
let do_add_at_eol = ""
let sub_at_eol_reg = ""
let do_sub_at_eol = ""
let parsing_if = ""
let evaluate_equals_at_eol_reg = ""
let do_evaluate_if_at_eol = ""
let evaluate_in_at_eol_reg = ""
let obj_var = ""
-- We use this as a container to store the names of all
-- integer variable names so we can distinguish between
-- string and integer variables.
let int_var_names = ""
let int_var_lookup = ""
let list_var_names = ""
let list_var_lookup = ""

let loop_index = 0
let if_level_1_label = ""
let if_level_2_label = ""
let if_level_3_label = ""
let if_level_4_label = ""
let if_level_5_label = ""
let if_level_6_label = ""
let if_level_7_label = ""
let if_level_8_label = ""
let if_level_1_had_else = ""
let if_level_2_had_else = ""
let if_level_3_had_else = ""
let if_level_4_had_else = ""
let if_level_5_had_else = ""
let if_level_6_had_else = ""
let if_level_7_had_else = ""
let if_level_8_had_else = ""
let if_level_helper = "_if"
let do_skip_to_eol = ""
let sizeof_sprintf_format_str = "%ld"

let id_counter = 1
let id = ""
let if_branch_counter = 0
let if_branch_label = ""
let out = ""

-- Preamble.
-- Declare some functions we use from libc, define our `main`
-- function and initialize some helper variables directly in
-- LLVM IR.
print("declare i32 @puts(i8*)\n")
print("declare i8* @strcpy(i8*, i8*)\n")
print("declare i8* @strcat(i8*, i8*)\n")
print("declare i8* @strstr(i8*, i8*)\n")
print("declare i32 @strcmp(i8*, i8*)\n")
print("declare i64 @strlen(i8*)\n")
print("declare i32 @sprintf(i8*, i8*, ...)\n")
print("declare i32 @read(i32, i8*, i32)\n")
print("declare i32 @memset(i8*, i32, i32)\n")
print('@int_to_string_sprintf_format = internal constant [3 x i8] c"%d\\00"')
print("define i8 @main()\n")
print("{\n")
print("entry:\n")
print("%.read_buf_alloc = alloca [2 x i8]\n")
print("%.read_buf = bitcast [2 x i8]* %.read_buf_alloc to i8*\n")
print("%.read_buf_clear = call i32 @memset(i8* %.read_buf, i32 0, i32 2)\n")
print("%.sprintf_buf_alloc = alloca [100 x i8]\n")
print("%.sprintf_buf = bitcast [100 x i8]* %.sprintf_buf_alloc to i8*\n")
print("%.int_to_string_sprintf_format = getelementptr [3 x i8], [3 x i8]* @int_to_string_sprintf_format, i32 0, i32 0")

-- The main loop.
-- We read from stdin one byte at a time and generate the 
-- LLVM IR code in one pass.
loop 
    c = read()
    if c == "" then
        -- We are done.
        print("ret i8 0\n")
        print("}\n")
        print(out_string_constants)
        break
    end

    -- Skipping everything until EOL if we encountered a comment token.
    if c == "\n" then
        do_skip_to_eol = ""
    else
        if do_skip_to_eol == "true" then
            c = ""
        end
    end

    -- Number parsing.
    if c == "0" then
        c_is_int = "true"
    end
    if c == "1" then
        c_is_int = "true"
    end
    if c == "2" then
        c_is_int = "true"
    end
    if c == "3" then
        c_is_int = "true"
    end
    if c == "4" then
        c_is_int = "true"
    end
    if c == "5" then
        c_is_int = "true"
    end
    if c == "6" then
        c_is_int = "true"
    end
    if c == "7" then
        c_is_int = "true"
    end
    if c == "8" then
        c_is_int = "true"
    end
    if c == "9" then
        c_is_int = "true"
    end
    if parsing_string == "true" then
    else
        if c_is_int == "true" then
            if token == "" then
                parsing_int = "true"
            end
        end
    end
    if parsing_int == "true" then
        -- Detect whether we are done parsing.
        if c == " " then
            parsing_int = ""
        end
        if c == "\n" then
            parsing_int = ""
        end
        if c == ")" then
            parsing_int = ""
        end
        if parsing_int == "" then
            -- We are done parsing, convert to an int.
            -- Generate that unique id.
            id_counter = id_counter + 1
            id = id_counter.to_string()
            parsing_int = ""
            out = "%."
            out = out + id
            out = out + " = alloca i32, align 4\n"
            out = out + "store i32 "
            out = out + token
            out = out + ", i32* %."
            out = out + id
            print(out)
            out = "; PARSED num: "
            out = out + token
            print(out)
            last_expression_reg = id
            last_expression_type = "int"
            token = ""
        else
            token = token + c
            c = ""
        end
    end
    c_is_int = ""

    -- String parsing.
    if parsing_string == "true" then
        if c == parsing_string_delimiter then
            -- Generate that unique id.
            id_counter = id_counter + 1
            id = id_counter.to_string()
            parsing_string = ""

            -- We cannot use the size of `token` because of the string
            -- escaping rules we have to follow in order to create valid
            -- LLVM IR string constants. See below where we handle escape
            -- sequences.
            size = sizeof(string_size)

            -- Create the string constant as a global constant in LLVM IR.
            out_string_constants = out_string_constants + "@."
            out_string_constants = out_string_constants + id
            out_string_constants = out_string_constants + " = internal constant ["
            out_string_constants = out_string_constants + size
            out_string_constants = out_string_constants + ' x i8] c"'
            out_string_constants = out_string_constants + token
            out_string_constants = out_string_constants + "\\"
            out_string_constants = out_string_constants + "00"
            out_string_constants = out_string_constants + '"\n'
            last_expression_reg = id
            last_expression_type = "string"

            -- Cast the array of chars to a pointer.
            out =  "%." 
            out = out + id
            out = out + " = bitcast ["
            out = out + size
            out = out + " x i8]* @."
            out = out + id
            out = out + " to i8*"
            print(out)
            token = ""
        else
            -- We only follow some escape rules, enough so this
            -- compiler is able to compile itself.
            if last_c == "\\" then
                if c == "n" then
                    -- This is a LLVM IR string escape sequence.
                    token = token + "\\0a"
                    string_size = string_size + "."
                    c = ""
                    last_c = "" 
                end
                if c == "\\" then
                    token = token + "\\\\"
                    string_size = string_size + "."
                    c = ""
                    last_c = ""
                end
                token = token + last_c
                last_c = ""
            end
            if c == '"' then
                -- This is a LLVM IR string escape sequence.
                token = token + "\\22"
                string_size = string_size + "."
                last_c = ""
                c = ""
            end
            if c == "\\" then
                last_c = c
                c = ""
            end
            token = token + c
            if c == "" then
            else
                string_size = string_size + "."
            end
        end
    else
        if c == "(" then
            function_call = token
            token = ""
            c = ""
        end
        if c == "=" then
            if parsing_if == "true" then
            else
                assign_to_variable_at_eol = last_token
                do_assign_to_variable_at_eol = "true"
                c = ""
                last_token = ""
                last_expression_reg = ""
            end
        end
        if c == "+" then
            add_at_eol_reg = last_expression_reg
            do_add_at_eol = "true"
            last_expression_reg = ""
            c = ""
        end
        if c == "." then
            obj_var = token
            c = ""
            if token == "" then
            else
                -- This is a variable.
                out = "; READ var at dot: "
                out = out + token
                print(out)
                last_expression_reg = token
                last_token = token
                int_var_lookup = ":"
                int_var_lookup = int_var_lookup + token
                int_var_lookup = int_var_lookup + ":"
                if int_var_lookup in int_var_names then
                    last_expression_type = "int"
                else
                    list_var_lookup = ":"
                    list_var_lookup = list_var_lookup + token
                    list_var_lookup = list_var_lookup + ":"
                    if list_var_lookup in list_var_names then
                        last_expression_type = "list"
                    else
                        last_expression_type = "string"
                    end
                end
                token = ""
            end
        end
        if c == " " then
            if token == "then" then
                token = ""
            end
            if token == "in" then
                evaluate_in_at_eol_reg = last_expression_reg
                do_evaluate_if_at_eol = "true"
            end
            if token == "-" then
                sub_at_eol_reg = last_expression_reg
                do_sub_at_eol = "true"
                last_expression_reg = ""
                c = ""
            end
            if token == "--" then
                do_skip_to_eol = "true"
                token = ""
            end
            if token == "let" then
                do_create_to_variable_at_eol = "true"
                token = ""
            end
            if token == "if" then
                parsing_if = "true" 
                token = ""
            end
            if token == "==" then
                evaluate_equals_at_eol_reg = last_expression_reg
                do_evaluate_if_at_eol = "true"
                token = ""
            end
            if token == "loop" then
                -- In LLVM IR each block of code must end with a
                -- jump to the next block, even if it is the next.
                out = "br label %loop"
                out = out + loop_index.to_string()
                out = out + "\nloop"
                out = out + loop_index.to_string()
                out = out + ":"
                print(out)
            end
            if token == "" then
            else
                -- This is a variable.
                last_expression_reg = token
                last_token = token
                int_var_lookup = ":"
                int_var_lookup = int_var_lookup + token
                int_var_lookup = int_var_lookup + ":"
                if int_var_lookup in int_var_names then
                    last_expression_type = "int"
                else
                    list_var_lookup = ":"
                    list_var_lookup = list_var_lookup + token
                    list_var_lookup = list_var_lookup + ":"
                    if list_var_lookup in list_var_names then
                        last_expression_type = "list"
                    else
                        last_expression_type = "string"
                    end
                end
                out = "; READ var at space: "
                out = out + token
                out = out + " of type"
                out = out + last_expression_type
                print(out)
            end
            token = ""
            c = ""
        end
        if c == "\n" then
            id_counter = id_counter + 1
            id = id_counter.to_string()
            if token == "break" then
                out = "br label %end_loop"
                out = out + loop_index.to_string()
                token = ""
                print(out)
            end
            if token == "continue" then
                out = "br label %loop"
                out = out + loop_index.to_string()
                token = ""
                print(out)
            end
            if last_expression_reg == "" then
                if token == "" then
                else
                    -- This is a variable.
                    out = "; READ var at eol"
                    out = out + token
                    print(out)
                    last_expression_reg = token
                    int_var_lookup = ":"
                    int_var_lookup = int_var_lookup + token
                    int_var_lookup = int_var_lookup + ":"
                    if int_var_lookup in int_var_names then
                        last_expression_type = "int"
                    else
                        list_var_lookup = ":"
                        list_var_lookup = list_var_lookup + token
                        list_var_lookup = list_var_lookup + ":"
                        if list_var_lookup in list_var_names then
                            last_expression_type = "list"
                        else
                            last_expression_type = "string"
                        end
                    end
                end
            end
            -- We have to generate the correct labels and therefor must
            -- know the depth of if / else nesting.
            if token == "else" then
                if_level_helper = ""
                if if_level_8_label == "" then
                else
                    if_level_helper = if_level_8_label
                    if_level_8_had_else = "true"
                end
                if if_level_helper == "" then
                    if if_level_7_label == "" then
                    else
                        if_level_helper = if_level_7_label
                        if_level_7_had_else = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_6_label == "" then
                    else
                        if_level_helper = if_level_6_label
                        if_level_6_had_else = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_5_label == "" then
                    else
                        if_level_helper = if_level_5_label
                        if_level_5_had_else = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_4_label == "" then
                    else
                        if_level_helper = if_level_4_label
                        if_level_4_had_else = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_3_label == "" then
                    else
                        if_level_helper = if_level_3_label
                        if_level_3_had_else = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_2_label == "" then
                    else
                        if_level_helper = if_level_2_label
                        if_level_2_had_else = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_1_label == "" then
                    else
                        if_level_helper = if_level_1_label
                        if_level_1_had_else = "true"
                    end
                end
                out = "br label %end"
                out = out + if_level_helper
                out = out + "\nelse"
                out = out + if_level_helper
                out = out + ":"
                print(out)
            end
            if token == "end" then
                -- We have to generate the correct labels and therefor must
                -- know the depth of if / else nesting.
                if_level_helper = ""
                if if_level_8_label == "" then
                else
                    if_level_helper = if_level_8_label
                    if_level_8_label = ""
                    if if_level_8_had_else == "" then
                        out = "br label %end"
                        out = out + if_level_helper
                        out = out + "\nelse"
                        out = out + if_level_helper
                        out = out + ":"
                        print(out)
                    end
                    if_level_8_had_else = ""
                end
                if if_level_helper == "" then
                    if if_level_7_label == "" then
                    else
                        if_level_helper = if_level_7_label
                        if_level_7_label = ""
                        if if_level_7_had_else == "" then
                            out = "br label %end"
                            out = out + if_level_helper
                            out = out + "\nelse"
                            out = out + if_level_helper
                            out = out + ":"
                            print(out)
                        end
                        if_level_7_had_else = ""
                    end
                end
                if if_level_helper == "" then
                    if if_level_6_label == "" then
                    else
                        if_level_helper = if_level_6_label
                        if_level_6_label = ""
                        if if_level_6_had_else == "" then
                            out = "br label %end"
                            out = out + if_level_helper
                            out = out + "\nelse"
                            out = out + if_level_helper
                            out = out + ":"
                            print(out)
                        end
                        if_level_6_had_else = ""
                    end
                end
                if if_level_helper == "" then
                    if if_level_5_label == "" then
                    else
                        if_level_helper = if_level_5_label
                        if_level_5_label = ""
                        if if_level_5_had_else == "" then
                            out = "br label %end"
                            out = out + if_level_helper
                            out = out + "\nelse"
                            out = out + if_level_helper
                            out = out + ":"
                            print(out)
                        end
                        if_level_5_had_else = ""
                    end
                end
                if if_level_helper == "" then
                    if if_level_4_label == "" then
                    else
                        if_level_helper = if_level_4_label
                        if_level_4_label = ""
                        if if_level_4_had_else == "" then
                            out = "br label %end"
                            out = out + if_level_helper
                            out = out + "\nelse"
                            out = out + if_level_helper
                            out = out + ":"
                            print(out)
                        end
                        if_level_4_had_else = ""
                    end
                end
                if if_level_helper == "" then
                    if if_level_3_label == "" then
                    else
                        if_level_helper = if_level_3_label
                        if_level_3_label = ""
                        if if_level_3_had_else == "" then
                            out = "br label %end"
                            out = out + if_level_helper
                            out = out + "\nelse"
                            out = out + if_level_helper
                            out = out + ":"
                            print(out)
                        end
                        if_level_3_had_else = ""
                    end
                end
                if if_level_helper == "" then
                    if if_level_2_label == "" then
                    else
                        if_level_helper = if_level_2_label
                        if_level_2_label = ""
                        if if_level_2_had_else == "" then
                            out = "br label %end"
                            out = out + if_level_helper
                            out = out + "\nelse"
                            out = out + if_level_helper
                            out = out + ":"
                            print(out)
                        end
                        if_level_2_had_else = ""
                    end
                end
                if if_level_helper == "" then
                    if if_level_1_label == "" then
                    else
                        if_level_helper = if_level_1_label
                        if_level_1_label = ""
                        if if_level_1_had_else == "" then
                            out = "br label %end"
                            out = out + if_level_helper
                            out = out + "\nelse"
                            out = out + if_level_helper
                            out = out + ":"
                            print(out)
                        end
                        if_level_1_had_else = ""
                    end
                end
                if if_level_helper == "" then
                    -- This is the end of a loop.
                    out = "br label %loop"
                    out = out + loop_index.to_string()
                    out = out + "\nbr label %end_loop"
                    out = out + loop_index.to_string()
                    out = out + "\nend_loop"
                    out = out + loop_index.to_string()
                    out = out + ":"
                    loop_index = loop_index + 1
                else
                    out = "br label %end"
                    out = out + if_level_helper
                    out = out + "\nend"
                    out = out + if_level_helper
                    out = out + ":"
                end
                print(out)
            end
            if do_create_to_variable_at_eol == "true" then
                if last_expression_type == "int" then
                    out = "%."
                    out = out + assign_to_variable_at_eol
                    out = out + " = alloca i32, align 4"
                    int_var_names = int_var_names + ":"
                    int_var_names = int_var_names + assign_to_variable_at_eol
                    int_var_names = int_var_names + ":"
                    print(out)
                else
                    if last_expression_type == "string" then
                        -- Our main goal for now is to be able to compile this compiler
                        -- with itself. We don't care about memory usage at this point.
                        -- We don't care about buffer overruns as well at this stage.
                        -- Let's make the variables big enough for now.
                        -- Nobody would ever want to store more than 500 bytes in 
                        -- a variable anyway.
                        var_size = "500"
                        if assign_to_variable_at_eol == "out_string_constants" then
                            -- Oops, found that one variable that wants to be bigger
                            -- than 500 bytes. :-)
                            var_size = "100000"
                        end
                        if assign_to_variable_at_eol == "string_constant_declarations" then
                            -- ... and another one.
                            var_size = "100000"
                        end
                        if assign_to_variable_at_eol == "constant_definitions" then
                            -- ... and another one.
                            var_size = "100000"
                        end
                        if assign_to_variable_at_eol == "out" then
                            -- ... and another one.
                            var_size = "100000"
                        end
                        out = "\n%._"
                        out = out + assign_to_variable_at_eol
                        out = out + " = alloca ["
                        out = out + var_size
                        out = out + " x i8]\n"
                        out = out + "%." 
                        out = out + assign_to_variable_at_eol
                        out = out + " = bitcast ["
                        out = out + var_size
                        out = out + " x i8]* %._"
                        out = out + assign_to_variable_at_eol
                        out = out + " to i8*\n"
                        out = out + "%." 
                        out = out + assign_to_variable_at_eol
                        out = out + "_clear = call i32 @memset(i8* %."
                        out = out + assign_to_variable_at_eol
                        out = out + ", i32 0, i32 "
                        out = out + var_size
                        out = out + ")"
                        print(out)
                    else
                        -- This is a list.
                        out = "%."
                        out = out + assign_to_variable_at_eol
                        out = out + " = alloca [10000 x i8*]"
                        out = out + "\n%.len_"
                        out = out + assign_to_variable_at_eol
                        out = out + " = alloca i32\n"
                        out = out + "store i32 0, i32* %.len_"
                        out = out + assign_to_variable_at_eol
                        list_var_names = list_var_names + ":"
                        list_var_names = list_var_names + assign_to_variable_at_eol
                        list_var_names = list_var_names + ":"
                        print(out)
                    end
                end
            end
            if do_assign_to_variable_at_eol == "true" then
                if last_expression_type == "int" then
                    out = "%.assignint"
                    out = out + id
                    out = out + " = load i32, i32* %."
                    out = out + last_expression_reg
                    out = out + ", align 4\n"
                    if do_add_at_eol == "true" then
                        out = out + "%.addload"
                        out = out + id
                        out = out + " = load i32, i32* %."
                        out = out + add_at_eol_reg
                        out = out + "\n%.add"
                        out = out + id
                        out = out + " = add i32 %.assignint"
                        out = out + id
                        out = out + ", %.addload"
                        out = out + id
                        out = out + "\nstore i32 %.add"
                    else
                        if do_sub_at_eol == "true" then
                            out = out + "%.subload"
                            out = out + id
                            out = out + " = load i32, i32* %."
                            out = out + sub_at_eol_reg
                            out = out + "\n%.sub"
                            out = out + id
                            out = out + " = sub i32 %.subload"
                            out = out + id
                            out = out + ", %.assignint"
                            out = out + id
                            out = out + "\nstore i32 %.sub"
                        else
                            out = out + "store i32 %.assignint"
                        end
                    end
                    out = out + id
                    out = out + ", i32* %."
                    out = out + assign_to_variable_at_eol
                    print(out)
                else
                    if last_expression_type == "string" then
                        out = "%.strcpy"
                        out = out + id
                        out = out + " = call i8* (i8*, i8*) @strcpy(i8* %."
                        out = out + assign_to_variable_at_eol
                        out = out + ", i8* %."
                        if do_add_at_eol == "true" then
                            out = out + add_at_eol_reg
                        else
                            out = out + last_expression_reg
                        end
                        out = out + ")"
                        print(out)
                        if do_add_at_eol == "true" then
                            out = "%.strcat"
                            out = out + id
                            out = out + " = call i8* (i8*, i8*) @strcat(i8* %."
                            out = out + assign_to_variable_at_eol
                            out = out + ", i8* %."
                            out = out + last_expression_reg
                            out = out + ")"
                            print(out)
                        end
                    else
                        -- Assigning lists is currently not supported.
                    end
                end
            end
            if do_evaluate_if_at_eol == "true" then
                -- The following code handles the expression evaluation
                -- as well as branching.
                if_branch_counter = if_branch_counter + 1
                if_branch_label = if_branch_counter.to_string() 
                if_level_helper = ""
                if if_level_1_label == "" then
                    if_level_1_label = if_branch_label
                    if_level_helper = "true"
                end
                if if_level_helper == "" then
                    if if_level_2_label == "" then
                        if_level_2_label = if_branch_label
                        if_level_helper = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_3_label == "" then
                        if_level_3_label = if_branch_label
                        if_level_helper = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_4_label == "" then
                        if_level_4_label = if_branch_label
                        if_level_helper = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_5_label == "" then
                        if_level_5_label = if_branch_label
                        if_level_helper = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_5_label == "" then
                        if_level_4_label = if_branch_label
                        if_level_helper = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_6_label == "" then
                        if_level_6_label = if_branch_label
                        if_level_helper = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_7_label == "" then
                        if_level_7_label = if_branch_label
                        if_level_helper = "true"
                    end
                end
                if if_level_helper == "" then
                    if if_level_8_label == "" then
                        if_level_8_label = if_branch_label
                        if_level_helper = "true"
                    end
                end
                if evaluate_equals_at_eol_reg == "" then
                    -- `in
                    if last_expression_type == "string" then
                        out = "%.in"  
                        out = out + id
                        out = out + " = call i8* (i8*, i8*) @strstr(i8* %."  
                        out = out + last_expression_reg
                        out = out + ", i8* %."
                        out = out + evaluate_in_at_eol_reg
                        out = out + ")\n"
                        out = out + "%.in2"
                        out = out + id
                        out = out + " = icmp eq i8* %.in"
                        out = out + id
                        out = out + ", null\n"
                        out = out + "br i1 %.in2" 
                        out = out + id
                        out = out + ", label %else" 
                        out = out + if_branch_label
                        out = out + ", label %if" 
                        out = out + if_branch_label
                        out = out + "\n"
                        out = out + "if"
                        out = out + if_branch_label
                        out = out + ":"
                    else
                        -- This is a list.
                        out = "%.in_pos"
                        out = out + id
                        out = out + " = alloca i32\n"
                        out = out + "%.getlen_"
                        out = out + id
                        out = out + " = load i32, i32* %.len_"
                        out = out + last_expression_reg
                        out = out + "\nstore i32 %.getlen_"
                        out = out + id
                        out = out + ", i32* %.in_pos"
                        out = out + id
                        out = out + "\nbr label %in_loop"
                        out = out + id
                        -- The loop.
                        out = out + "\nin_loop"
                        out = out + id
                        out = out + ":\n"
                        out = out + "%.idx_load"
                        out = out + id
                        out = out + " = load i32, i32* %.in_pos"
                        out = out + id
                        out = out + "\n%.idx"
                        out = out + id
                        out = out + " = sub i32 %.idx_load"  
                        out = out + id
                        out = out + ", 1"  
                        out = out + "\n%.idxcmp"
                        out = out + id
                        out = out + " = icmp eq i32 %.idx"
                        out = out + id
                        out = out + ", -1\n"
                        out = out + "br i1 %.idxcmp" 
                        out = out + id
                        out = out + ", label %else" 
                        out = out + if_branch_label
                        out = out + ", label %cont" 
                        out = out + id
                        out = out + "\ncont"
                        out = out + id
                        out = out + ":\n"
                        out = out + "store i32 %.idx"
                        out = out + id
                        out = out + ", i32* %.in_pos"
                        out = out + id
                        -- Read what's at the current position.
                        out = out + "\n%.atptr"
                        out = out + id
                        out = out + " = getelementptr [10000 x i8*], [10000 x i8*]* %." 
                        out = out + last_expression_reg
                        out = out + ", i32 0, i32 %.idx"
                        out = out + id
                        out = out + "\n%.atload"
                        out = out + id
                        out = out + " = load i8*, i8** %.atptr"
                        out = out + id
                        out = out + "\n%.strcmp"
                        out = out + id
                        out = out + " = call i32 (i8*, i8*) @strcmp(i8* %.atload"
                        out = out + id
                        out = out + ", i8* %."
                        out = out + evaluate_in_at_eol_reg
                        out = out + ")\n"
                        out = out + "%.strcmpb"
                        out = out + id
                        out = out + " = icmp eq i32 %.strcmp"
                        out = out + id
                        out = out + ", 0\n"
                        out = out + "br i1 %.strcmpb" 
                        out = out + id
                        out = out + ", label %if" 
                        out = out + if_branch_label
                        out = out + ", label %in_loop" 
                        out = out + id
                        out = out + "\n"
                        out = out + "if"
                        out = out + if_branch_label
                        out = out + ":"
                    end
                    print(out)
                else
                    -- `==`
                    if last_expression_type == "int" then
                        out = "%.eqintleft"
                        out = out + id
                        out = out + " = load i32, i32* %."
                        out = out + last_expression_reg
                        out = out + ", align 4\n"
                        out = out + "%.eqintright"
                        out = out + id
                        out = out + " = load i32, i32* %."
                        out = out + evaluate_equals_at_eol_reg
                        out = out + "\n%.eqint"
                        out = out + id
                        out = out + " = icmp eq i32 %.eqintleft"
                        out = out + id
                        out = out + ", %.eqintright"
                        out = out + id
                        out = out + "\nbr i1 %.eqint" 
                        out = out + id
                        out = out + ", label %if" 
                        out = out + if_branch_label
                        out = out + ", label %else" 
                        out = out + if_branch_label
                        out = out + "\n"
                        out = out + "if"
                        out = out + if_branch_label
                        out = out + ":"
                        print(out)
                    else
                        out = "%.strcmp"
                        out = out + id
                        out = out + " = call i32 (i8*, i8*) @strcmp(i8* %."
                        out = out + evaluate_equals_at_eol_reg
                        out = out + ", i8* %."
                        out = out + last_expression_reg
                        out = out + ")\n"
                        out = out + "%.strcmpb"
                        out = out + id
                        out = out + " = icmp eq i32 %.strcmp"
                        out = out + id
                        out = out + ", 0\n"
                        out = out + "br i1 %.strcmpb" 
                        out = out + id
                        out = out + ", label %if" 
                        out = out + if_branch_label
                        out = out + ", label %else" 
                        out = out + if_branch_label
                        out = out + "\n"
                        out = out + "if"
                        out = out + if_branch_label
                        out = out + ":"
                        print(out)
                    end
                end
            end
            parsing_if = ""
            do_evaluate_if_at_eol = ""
            evaluate_equals_at_eol_reg = ""
            evaluate_in_at_eol_reg = ""
            do_assign_to_variable_at_eol = ""
            do_add_at_eol = ""
            do_create_to_variable_at_eol = ""
            token = ""
            c = ""
        end
        if c == ")" then
            out = "; FUNCTION call at rparen: "
            out = out + function_call
            print(out)
            if token == "" then
            else
                -- This is a variable.
                out = "; READ var at rparen: "
                out = out + token
                print(out)
                last_expression_reg = token
                int_var_lookup = ":"
                int_var_lookup = int_var_lookup + token
                int_var_lookup = int_var_lookup + ":"
                if int_var_lookup in int_var_names then
                    last_expression_type = "int"
                else
                    list_var_lookup = ":"
                    list_var_lookup = list_var_lookup + token
                    list_var_lookup = list_var_lookup + ":"
                    if list_var_lookup in list_var_names then
                        last_expression_type = "list"
                    else
                        last_expression_type = "string"
                    end
                end
            end
            if function_call == "list" then
                -- We don't need to actually create anything here.
                -- That's done when the corresponding variable is created.
                last_expression_reg = "<not set>"
                last_expression_type = "list"
            end
            if function_call == "push" then
                -- First, copy the current expression value (which is expected to be a string).
                out = "%.pushcopy_alloc"
                out = out + id
                out = out + " = alloca [500 x i8]\n" 
                out = out + "%.pushcopy" 
                out = out + id
                out = out + " = bitcast [500 x i8]* %.pushcopy_alloc"
                out = out + id
                out = out + " to i8*\n"
                out = out + "%.pushstrcopy"
                out = out + id
                out = out + " = call i8* (i8*, i8*) @strcpy(i8* %.pushcopy"
                out = out + id
                out = out + ", i8* %."
                out = out + last_expression_reg
                out = out + ")\n"
                -- Get the current length of the array.
                out = out + "%.pushlen_"
                out = out + id
                out = out + " = load i32, i32* %.len_"
                out = out + obj_var
                -- And store the copied value at the index.
                out = out + "\n%.elmptr"
                out = out + id
                out = out + " = getelementptr [10000 x i8*], [10000 x i8*]* %." 
                out = out + obj_var
                out = out + ", i32 0, i32 %.pushlen_"
                out = out + id
                out = out + "\nstore i8* %.pushcopy"
                out = out + id
                out = out + ", i8** %.elmptr"
                out = out + id
                -- At last, increment the length of the array.
                out = out + "\n%.inclen"
                out = out + id
                out = out + " = add i32 %.pushlen_"
                out = out + id
                out = out + ", 1\n"
                out = out + "store i32 %.inclen"
                out = out + id
                out = out + ", i32* %.len_"
                out = out + obj_var
                print(out)
            end
            if function_call == "pop" then
                -- First, decrement `length`.
                out = "%.getlen_"
                out = out + id
                out = out + " = load i32, i32* %.len_"
                out = out + obj_var
                out = out + "\n%.declen"
                out = out + id
                out = out + " = sub i32 %.getlen_"
                out = out + id
                out = out + ", 1\n"
                out = out + "store i32 %.declen"
                out = out + id
                out = out + ", i32* %.len_"
                out = out + obj_var
                -- Then read what's at the end.
                out = out + "\n%.atptr"
                out = out + id
                out = out + " = getelementptr [10000 x i8*], [10000 x i8*]* %." 
                out = out + obj_var
                out = out + ", i32 0, i32 %.declen"
                out = out + id
                out = out + "\n%.atload"
                out = out + id
                out = out + " = load i8*, i8** %.atptr"
                out = out + id
                print(out) 
                last_expression_reg = "atload" + id
                last_expression_type = "string"
            end
            if function_call == "clear" then
                -- Just set `length` to 0.
                out = "store i32 0"
                out = out + ", i32* %.len_"
                out = out + obj_var
                print(out) 
                last_expression_reg = "" 
                last_expression_type = ""
            end
            if function_call == "at" then
                out = "%.atidx"
                out = out + id
                out = out + " = load i32, i32* %."
                out = out + last_expression_reg
                out = out + "\n%.atptr"
                out = out + id
                out = out + " = getelementptr [10000 x i8*], [10000 x i8*]* %." 
                out = out + obj_var
                out = out + ", i32 0, i32 %.atidx"
                out = out + id
                out = out + "\n%.atload"
                out = out + id
                out = out + " = load i8*, i8** %.atptr"
                out = out + id
                print(out) 
                last_expression_reg = "atload" + id
                last_expression_type = "string"
            end
            if function_call == "length" then
                last_expression_type = "int"
                last_expression_reg = "len_" + obj_var
            end
            if function_call == "print" then
                if last_expression_type == "int" then
                    -- This is an integer - convert it to string first.
                    out = "%.putsiload"
                    out = out + id
                    out = out + " = load i32, i32* %."
                    out = out + last_expression_reg
                    out = out + "\n%.putsi"
                    out = out + id
                    out = out + " = call i32 (i8*, i8*, ...) @sprintf(i8* %.sprintf_buf"
                    out = out + ", i8* %.int_to_string_sprintf_format, i32 %.putsiload"
                    out = out + id
                    out = out + ")"
                    print(out)
                    last_expression_reg = "sprintf_buf"
                end
                out = "%.puts"
                out = out + id
                out = out + " = call i32 (i8*) @puts(i8* %."
                out = out + last_expression_reg
                out = out + ")"
                print(out)
                last_expression_reg = ""
            end
            if function_call == "to_string" then
                out = "%.tostrload"
                out = out + id
                out = out + " = load i32, i32* %."
                out = out + last_expression_reg
                out = out + "\n%.tostr"
                out = out + id
                out = out + " = call i32 (i8*, i8*, ...) @sprintf(i8* %.sprintf_buf"
                out = out + ", i8* %.int_to_string_sprintf_format, i32 %.tostrload"
                out = out + id
                out = out + ")"
                print(out)
                last_expression_reg = "sprintf_buf"
                last_expression_type = "string"
            end
            if function_call == "sizeof" then
                out = "%.strlen"
                out = out + id
                out = out + " = call i64 (i8*) @strlen(i8* %."
                out = out + last_expression_reg
                out = out + ")\n"
                out = out + "%.strlen3"
                out = out + id
                out = out + " = call i32 (i8*, i8*, ...) @sprintf(i8* %.sprintf_buf"
                out = out + ", i8* %.int_to_string_sprintf_format, i64 %.strlen"
                out = out + id
                out = out + ")"
                print(out)
                last_expression_reg = "sprintf_buf"
                last_expression_type = "string"
            end
            if function_call == "read" then
                -- Read from stdin. Evaluate to "" if we are at the end of input.
                out = "%.read"
                out = out + id
                out = out + " = call i32 (i32, i8*, i32) @read(i32 0, i8* %.read_buf, i32 1)\n"
                out = out + "%.read"
                out = out + id
                out = out + "_test = icmp eq i32 %.read"
                out = out + id
                out = out + " , 0\n"
                out = out + "br i1 %.read"
                out = out + id
                out = out + "_test, label %b"
                out = out + id
                out = out + "_true, label %b"
                out = out + id
                out = out + "_end\nb"
                out = out + id
                out = out + "_true:\n%b"
                out = out + id
                out = out + "_clear = call i32 @memset(i8* %.read_buf, i32 0, i32 2)\n"
                out = out + "br label %b"
                out = out + id
                out = out + "_end\nb"
                out = out + id
                out = out + "_end:"
                print(out)
                last_expression_reg = "read_buf"
            end
            function_call = ""
            obj_var = ""
            token = ""
            c = ""
        end
        if c == '"' then
            parsing_string = "true"
            parsing_string_delimiter = c
            string_size = "."
            c = ""
        end
        if c == "'" then
            parsing_string = "true"
            parsing_string_delimiter = c
            string_size = "."
            c = ""
        end
        token = token + c
    end
end
