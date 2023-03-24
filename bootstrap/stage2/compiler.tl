-- Bootstrap stage 2
--
-- This implementation is enhances stage1 by adding:
-- 
-- - `in` operator to find strings in strings
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
let size = ""
let do_create_to_variable_at_eol = ""
let add_at_eol_reg = ""
let do_add_at_eol = ""
let parsing_if = ""
let evaluate_equals_at_eol_reg = ""
let do_evaluate_if_at_eol = ""
let evaluate_in_at_eol_reg = ""

-- Implementing nested if / else blocks is kinda hard 
-- without a numeric type. 
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
let if_level_helper = ""
let sizeof_sprintf_format_str = "%ld"
let do_skip_to_eol = ""

-- We have to construct unique identifiers to generate
-- valid LLVM IR code. Again, without a numeric type
-- we have to get creative.
-- We cycle `id_prefix_1` from "a" to "j", wrap around
-- and add a character to `id_prefix_2`. The final `id`
-- is then constructed by concatenating these two strings,
let id_prefix_1 = "a"
let id_prefix_2 = "i"
let id = ""
let string_contstants_id = ""
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
print("define i8 @main()\n")
print("{\n")
print("entry:\n")
print("%.read_buf_alloc = alloca [2 x i8]\n")
print("%.read_buf = bitcast [2 x i8]* %.read_buf_alloc to i8*\n")
print("%.read_buf_clear = call i32 @memset(i8* %.read_buf, i32 0, i32 2)\n")
print("%.sprintf_buf_alloc = alloca [100 x i8]\n")
print("%.sprintf_buf = bitcast [100 x i8]* %.sprintf_buf_alloc to i8*\n")

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

    -- String parsing.
    if parsing_string == "true" then
        if c == parsing_string_delimiter then
            -- Generate that unique id.
            if id_prefix_1 == "j" then
                id_prefix_1 = "a"
                id_prefix_2 = id_prefix_2 + "i"
            else
                if id_prefix_1 == "i" then
                    id_prefix_1 = "j"
                end
                if id_prefix_1 == "h" then
                    id_prefix_1 = "i"
                end
                if id_prefix_1 == "g" then
                    id_prefix_1 = "h"
                end
                if id_prefix_1 == "f" then
                    id_prefix_1 = "g"
                end
                if id_prefix_1 == "e" then
                    id_prefix_1 = "f"
                end
                if id_prefix_1 == "d" then
                    id_prefix_1 = "e"
                end
                if id_prefix_1 == "c" then
                    id_prefix_1 = "d"
                end
                if id_prefix_1 == "b" then
                    id_prefix_1 = "c"
                end
                if id_prefix_1 == "a" then
                    id_prefix_1 = "b"
                end
            end
            id = id_prefix_1 + id_prefix_2
            parsing_string = ""

            -- We cannot use the size of `token` because of the string
            -- escaping rules we have to follow in order to create valid
            -- LLVM IR string constants. See below where we handle escape
            -- sequences.
            size = sizeof(string_size)

            -- Create the string constant as a global constant in LLVM IR.
            out_string_constants = out_string_constants + "@"
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
            out = out + " x i8]* @"
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
        if c == " " then
            if token == "then" then
                token = ""
            end
            if token == "in" then
                evaluate_in_at_eol_reg = last_expression_reg
                do_evaluate_if_at_eol = "true"
            end
            if token == "--" then
                do_skip_to_eol = "true"
            end
            if token == "let" then
                do_create_to_variable_at_eol = "true"
                token = ""
            end
            if token == "if" then
                parsing_if = "true" 
            end
            if token == "==" then
                evaluate_equals_at_eol_reg = last_expression_reg
                do_evaluate_if_at_eol = "true"
            end
            if token == "loop" then
                -- In LLVM IR each block of code must end with a
                -- jump to the next block, even if it is the next.
                print("br label %loop")
                print("loop:")
            end
            if token == "" then
            else
                last_expression_reg = token
                last_token = token
            end
            token = ""
            c = ""
        end
        if c == "\n" then
            -- Again, creative id generation at work.
            if id_prefix_1 == "j" then
                id_prefix_1 = "a"
                id_prefix_2 = id_prefix_2 + "i"
            else
                if id_prefix_1 == "i" then
                    id_prefix_1 = "j"
                end
                if id_prefix_1 == "h" then
                    id_prefix_1 = "i"
                end
                if id_prefix_1 == "g" then
                    id_prefix_1 = "h"
                end
                if id_prefix_1 == "f" then
                    id_prefix_1 = "g"
                end
                if id_prefix_1 == "e" then
                    id_prefix_1 = "f"
                end
                if id_prefix_1 == "d" then
                    id_prefix_1 = "e"
                end
                if id_prefix_1 == "c" then
                    id_prefix_1 = "d"
                end
                if id_prefix_1 == "b" then
                    id_prefix_1 = "c"
                end
                if id_prefix_1 == "a" then
                    id_prefix_1 = "b"
                end
            end
            id = id_prefix_1 + id_prefix_2
            if token == "break" then
                print("br label %end")
            end
            if last_expression_reg == "" then
                last_expression_reg = token
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
                    print("br label %loop")
                end
                out = "br label %end"
                out = out + if_level_helper
                out = out + "\nend"
                out = out + if_level_helper
                out = out + ":"
                print(out)
            end
            if do_create_to_variable_at_eol == "true" then
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
                    var_size = "58000"
                end
                if assign_to_variable_at_eol == "out" then
                    -- ... and another one.
                    var_size = "3000"
                end
                out = "%._"
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
            end
            if do_assign_to_variable_at_eol == "true" then
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
            end
            if do_evaluate_if_at_eol == "true" then
                -- The following code handles the expression evaluation
                -- as well as branching.
                if_branch_label = if_branch_label + "1"
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
                    print(out)
                else
                    -- `==`
                    out = "%.strcmp"
                    out = out + id
                    out = out + " = call i32 (i8*, i8*) @strcmp(i8* %."
                    out = out + evaluate_equals_at_eol_reg
                    out = out + ", i8* %."
                    out = out + last_expression_reg
                    out = out + ")\n"
                    out = out + "%.strcmp2"
                    out = out + id
                    out = out + " = icmp eq i32 %.strcmp"
                    out = out + id
                    out = out + " , 0\n"
                    out = out + "br i1 %.strcmp2" 
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
            if token == "" then
            else
                last_expression_reg = token
            end
            if function_call == "print" then
                out = "%.puts"
                out = out + id
                out = out + " = call i32 (i8*) @puts(i8* %."
                out = out + last_expression_reg
                out = out + ")"
                print(out)
                last_expression_reg = ""
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
                out = out + ", i8* %.sizeof_sprintf_format_str, i64 %.strlen"
                out = out + id
                out = out + ")"
                print(out)
                last_expression_reg = "sprintf_buf"
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
