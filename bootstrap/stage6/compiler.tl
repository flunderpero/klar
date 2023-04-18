-- Bootstrap stage 6
--
-- Replacing all string concatenations using the operator `+` with `.push()` 
-- since we will not support the former one in the final language.
--
-- But it will still be a bit messy.

-- All variables need to be declared at the top.
let token_types = list()
let token_values = list()
let token_value = ""
let token_type = ""
let next_token_type = ""
let next_token_value = ""
let add_token = 0
let parsing_int = 0
let parsing_string = 0
let parsing_string_length = 0
let parsing_string_delimiter = "" 
let parsing_string_escape_sequence = 0
let finish_token = 0
let skip_to_eol = 0
let c = ""
let next_c = ""
let c_is_int = 0
let compiling_function = 0
let functions = list()

-- These are just general helper variables.
let s = ""
let i = 0

-- Variables used by the code generation.
let id_counter = 0
let token_index = 0
let id = ""
let string_constant_declarations = ""
let constant_definitions = ""
let out = ""
let function_out = ""
let expression_reg = ""
let expression_type = ""
let handle = ""
let call_function = ""
let call_args = ""
let string_vars = list()
let int_vars = list()
let list_vars = list()
let expression_op = ""
let expression_op_rhs_reg = ""
let create_variable_at_eol = ""
let assign_variable_at_eol = ""
let obj_var = ""
let nesting_types = list()
let nesting_ids = list()
let nesting_loop_ids = list()

--
-- The lexer turns the input into lists of token types and values.
--
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
            constant_definitions.push(", i32* %.")
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
            constant_definitions.push(" = bitcast [")
            constant_definitions.push(parsing_string_length.to_string())
            constant_definitions.push(" x i8]* @.")
            constant_definitions.push(id)
            constant_definitions.push(" to i8*")
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
            token_types.push("identifier")
            token_values.push(token_value)
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
print("declare i32 @memset(i8*, i32, i32)")
print('@int_to_string_sprintf_format = internal constant [3 x i8] c"%d\\00"')
print("define i8 @main()")
print("{")
print("entry:")
print("%.read_buf_alloc = alloca [2 x i8]")
print("%.read_buf = bitcast [2 x i8]* %.read_buf_alloc to i8*")
print("%.read_buf_clear = call i32 @memset(i8* %.read_buf, i32 0, i32 2)")
print("%.sprintf_buf_alloc = alloca [100 x i8]")
print("%.sprintf_buf = bitcast [100 x i8]* %.sprintf_buf_alloc to i8*")
print("%.int_to_string_sprintf_format = getelementptr [3 x i8], [3 x i8]* @int_to_string_sprintf_format, i32 0, i32 0")
print(constant_definitions)

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
    out.push("\n; TOKEN #")
    out.push(token_index.to_string())
    out.push(": token_type=")
    out.push(token_type)
    out.push(": token_value=")
    out.push(token_value)

    if token_type == "identifier" then
        if token_value == "fn" then
            compiling_function = 1
            function_out.push("\ndefine void @")
            function_out.push(next_token_value)
            function_out.push("()\n{\n")
            function_out.push(constant_definitions)
            nesting_types.push("function")
            nesting_ids.push(next_token_value)  -- This is the function name.
            functions.push(next_token_value)
            -- Skip the next tokens to the body of the function.
            token_index = token_index + 6
            continue
        end
    end

    if next_token_type == "dot" then
        if token_type == "rparen" then
            -- We use the same `obj_var` when chaining methods.
        else
            if call_function == "" then
                obj_var = token_value
            end
        end
        s = token_value
        token_index = token_index + 1
        token_value = token_values.at(token_index)
        if token_value == "to_string" then
            -- We shortcut this here, so we can use `int.to_string()` in
            -- string concatenations.
            out.push("\n%.tostrload")
            out.push(id)
            out.push(" = load i32, i32* %.")
            out.push(s)
            out.push("\n%.tostr")
            out.push(id)
            out.push(" = call i32 (i8*, i8*, ...) @sprintf(i8* %.sprintf_buf")
            out.push(", i8* %.int_to_string_sprintf_format, i32 %.tostrload")
            out.push(id)
            out.push(")")
            expression_op_rhs_reg = "sprintf_buf"
            expression_type = "string"
            token_index = token_index + 2
            continue
        end
        if token_value == "length" then
            -- We shortcut this here, so we can use `list.length()` in
            -- string concatenations.
            out.push("\n%.loadlen")
            out.push(id)
            out.push(" = load i32, i32* %.len_")
            out.push(s)
            expression_op_rhs_reg = "loadlen" + id
            expression_type = "int"
            token_index = token_index + 2
            continue
        end
        if token_value == "pop" then
            out.push("\n; CALLING method=pop")
            out.push(", obj_var=")
            out.push(s)
            -- First, decrement `length`.
            out.push("\n%.getlen_")
            out.push(id)
            out.push(" = load i32, i32* %.len_")
            out.push(s)
            out.push("\n%.declen")
            out.push(id)
            out.push(" = sub i32 %.getlen_")
            out.push(id)
            out.push(", 1\n")
            out.push("store i32 %.declen")
            out.push(id)
            out.push(", i32* %.len_")
            out.push(s)
            -- Then read what's at the end.
            out.push("\n%.atptr")
            out.push(id)
            out.push(" = getelementptr [10000 x i8*], [10000 x i8*]* %." )
            out.push(s)
            out.push(", i32 0, i32 %.declen")
            out.push(id)
            out.push("\n%.atload")
            out.push(id)
            out.push(" = load i8*, i8** %.atptr")
            out.push(id)
            expression_op_rhs_reg = "atload" + id
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
            out.push(" = load i32, i32* %.")
            out.push(token_value)
            expression_op_rhs_reg = "loadintvar_" + id
            expression_type = "int"
        end
        if token_value in list_vars then
            expression_op_rhs_reg = token_value
            expression_type = "list"
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
                function_out.push("\n")
                function_out.push(out)
                function_out.push("\nret void\n}")
                compiling_function = 0
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
    end
    if token_type == "string" then
        expression_op_rhs_reg = token_value
        expression_type = "string"
    end
    if token_type == "int" then
        out.push("\n%.loadintconst_")
        out.push(id)
        out.push(" = load i32, i32* %.")
        out.push(token_value)
        expression_op_rhs_reg = "loadintconst_" + id
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
                    out.push(" = call i8* (i8*, i8*) @strcat(i8* %.")
                    out.push(assign_variable_at_eol)
                    out.push(", i8* %.")
                    out.push(expression_op_rhs_reg)
                    out.push(")\n")
                    expression_reg = "addstr" + id 
                else
                    -- If we are not assigning the expression to a variable, 
                    -- create a new string with enough room. 
                    out.push("\n%.add_alloca")
                    out.push(id)
                    out.push(" = alloca [500 x i8]\n")
                    out.push("%.add" )
                    out.push(id)
                    out.push(" = bitcast [500 x i8]* %.add_alloca")
                    out.push(id)
                    out.push(" to i8*\n")
                    -- Copy the last expression_reg into the new string.
                    out.push("%.addcpy")
                    out.push(id)
                    out.push(" = call i8* (i8*, i8*) @strcpy(i8* %.add")
                    out.push(id)
                    out.push(", i8* %.")
                    out.push(expression_reg)
                    out.push(")")
                    -- Now concatenate this with expression_op_rhs_reg.
                    out.push("\n%.addadd")
                    out.push(id)
                    out.push(" = call i8* (i8*, i8*) @strcat(i8* %.add")
                    out.push(id)
                    out.push(", i8* %.")
                    out.push(expression_op_rhs_reg)
                    out.push(")\n")
                    expression_reg = "add" + id
                end
            end
            if expression_type == "int" then
                out.push("\n%.add")
                out.push(id)
                out.push(" = add i32 %.")
                out.push(expression_reg)
                out.push(", %.")
                out.push(expression_op_rhs_reg)
                expression_reg = "add" + id
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
                expression_reg = "sub" + id
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
                expression_reg = "eqint" + id
            end
            if expression_type == "string" then
                out.push("\n%.strcmp")
                out.push(id)
                out.push(" = call i32 (i8*, i8*) @strcmp(i8* %.")
                out.push(expression_reg)
                out.push(", i8* %.")
                out.push(expression_op_rhs_reg)
                out.push(")\n")
                out.push("%.eqstr")
                out.push(id)
                out.push(" = icmp eq i32 %.strcmp")
                out.push(id)
                out.push(", 0")
                expression_reg = "eqstr" + id
            end
        end
        if expression_op == "in" then
            -- `in
            if expression_type == "string" then
                out.push("\n%.instrstr"  )
                out.push(id)
                out.push(" = call i8* (i8*, i8*) @strstr(i8* %."  )
                out.push(expression_op_rhs_reg)
                out.push(", i8* %.")
                out.push(expression_reg)
                out.push(")\n")
                out.push("%.instrcmp")
                out.push(id)
                out.push(" = icmp eq i8* %.instrstr")
                out.push(id)
                out.push(", null")
                out.push("%.instr")
                out.push(id)
                out.push(" = sub i1 1, %.instrcmp")
                out.push(id)
                expression_reg = "instr" + id
            end
            if expression_type == "list" then
                out.push("\n%.in_pos")
                out.push(id)
                out.push(" = alloca i32\n")
                out.push("%.getlen_")
                out.push(id)
                out.push(" = load i32, i32* %.len_")
                out.push(expression_op_rhs_reg)
                out.push("\nstore i32 %.getlen_")
                out.push(id)
                out.push(", i32* %.in_pos")
                out.push(id)
                out.push("\nbr label %in_loop")
                out.push(id)
                -- The loop.
                out.push("\nin_loop")
                out.push(id)
                out.push(":\n")
                out.push("%.idx_load")
                out.push(id)
                out.push(" = load i32, i32* %.in_pos")
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
                out.push(", i32* %.in_pos")
                out.push(id)
                -- Read what's at the current position.
                out.push("\n%.atptr")
                out.push(id)
                out.push(" = getelementptr [10000 x i8*], [10000 x i8*]* %." )
                out.push(expression_op_rhs_reg)
                out.push(", i32 0, i32 %.idx")
                out.push(id)
                out.push("\n%.atload")
                out.push(id)
                out.push(" = load i8*, i8** %.atptr")
                out.push(id)
                out.push("\n%.strcmp")
                out.push(id)
                out.push(" = call i32 (i8*, i8*) @strcmp(i8* %.atload")
                out.push(id)
                out.push(", i8* %.")
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
                expression_reg = "listin" + id
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
                if call_args == "" then
                else
                    call_args.push(", ")
                end
                if expression_type == "string" then
                    call_args.push("i8* " )
                end
                call_args.push("%." )
                call_args.push(expression_reg )
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
            expression_type = "list"
        end
        if call_function == "read" then
            -- Read from stdin. Evaluate to "" if we are at the end of input.
            out.push("\n%.read")
            out.push(id)
            out.push(" = call i32 (i32, i8*, i32) @read(i32 0, i8* %.read_buf, i32 1)\n")
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
            out.push("_clear = call i32 @memset(i8* %.read_buf, i32 0, i32 2)\n")
            out.push("br label %b")
            out.push(id)
            out.push("_end\nb")
            out.push(id)
            out.push("_end:")
            expression_reg = "read_buf"
            expression_type = "string"
        end
        if call_function == "push" then
            if obj_var in list_vars then
                -- First, copy the current expression value (which is expected to be a string).
                out.push("\n%.pushcopy_alloc")
                out.push(id)
                out.push(" = alloca [500 x i8]\n" )
                out.push("%.pushcopy" )
                out.push(id)
                out.push(" = bitcast [500 x i8]* %.pushcopy_alloc")
                out.push(id)
                out.push(" to i8*\n")
                out.push("%.pushstrcopy")
                out.push(id)
                out.push(" = call i8* (i8*, i8*) @strcpy(i8* %.pushcopy")
                out.push(id)
                out.push(", ")
                out.push(call_args)
                out.push(")\n")
                -- Get the current length of the array.
                out.push("%.pushlen_")
                out.push(id)
                out.push(" = load i32, i32* %.len_")
                out.push(obj_var)
                -- And store the copied value at the index.
                out.push("\n%.elmptr")
                out.push(id)
                out.push(" = getelementptr [10000 x i8*], [10000 x i8*]* %." )
                out.push(obj_var)
                out.push(", i32 0, i32 %.pushlen_")
                out.push(id)
                out.push("\nstore i8* %.pushcopy")
                out.push(id)
                out.push(", i8** %.elmptr")
                out.push(id)
                -- At last, increment the length of the array.
                out.push("\n%.inclen")
                out.push(id)
                out.push(" = add i32 %.pushlen_")
                out.push(id)
                out.push(", 1\n")
                out.push("store i32 %.inclen")
                out.push(id)
                out.push(", i32* %.len_")
                out.push(obj_var)
            end
            if obj_var in string_vars then
                out.push("\n%.strpush")
                out.push(id)
                out.push(" = call i8* (i8*, i8*) @strcat(i8* %.")
                out.push(obj_var)
                out.push(", " )
                out.push(call_args)
                out.push(")\n")
                expression_reg = "strpush" + id
                expression_type = "string"
            end
        end
        if call_function == "at" then
            out.push("\n%.atptr")
            out.push(id)
            out.push(" = getelementptr [10000 x i8*], [10000 x i8*]* %." )
            out.push(obj_var)
            out.push(", i32 0, i32 ")
            out.push(call_args)
            out.push("\n%.atload")
            out.push(id)
            out.push(" = load i8*, i8** %.atptr")
            out.push(id)
            expression_reg = "atload" + id
            expression_type = "string"
        end
        if call_function == "clear" then
            -- Just set `length` to 0.
            out.push("\nstore i32 0")
            out.push(", i32* %.len_")
            out.push(obj_var)
            expression_reg = "" 
        end
        if call_function in functions then
            out.push("\ncall void @")
            out.push(call_function)
            out.push("()")
        end
        call_function = ""
    end

    -- Create and assign variables. 
    if next_token_type == "newline" then
        if create_variable_at_eol == "" then
        else
            -- Create variable.
            if expression_type == "int" then
                out.push("\n%.")
                out.push(create_variable_at_eol)
                out.push(" = alloca i32, align 4")
                int_vars.push(create_variable_at_eol)
            end
            if expression_type == "list" then
                -- This is a list.
                out.push("\n%.")
                out.push(create_variable_at_eol)
                out.push(" = alloca [10000 x i8*]")
                out.push("\n%.len_")
                out.push(create_variable_at_eol)
                out.push(" = alloca i32\n")
                out.push("store i32 0, i32* %.len_")
                out.push(create_variable_at_eol)
                list_vars.push(create_variable_at_eol)
            end
            if expression_type == "string" then
                -- Our main goal for now is to be able to compile this compiler
                -- with itself. We don't care about memory usage at this point.
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
                if create_variable_at_eol == "function_out" then
                    -- ... and another one.
                    i = 100000
                end
                out.push("\n; CREATE STRING: var=")
                out.push(create_variable_at_eol)
                out.push(", size=")
                out.push(i.to_string())
                out.push("\n%._")
                out.push(create_variable_at_eol)
                out.push(" = alloca [")
                out.push(i.to_string())
                out.push(" x i8]\n")
                out.push("%." )
                out.push(create_variable_at_eol)
                out.push(" = bitcast [")
                out.push(i.to_string())
                out.push(" x i8]* %._")
                out.push(create_variable_at_eol)
                out.push(" to i8*\n")
                out.push("%." )
                out.push(create_variable_at_eol)
                out.push("_clear = call i32 @memset(i8* %.")
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
                out.push(", i32* %.")
                out.push(assign_variable_at_eol)
            end
            if expression_type == "string" then
                out.push("\n%.strcpy")
                out.push(id)
                out.push(" = call i8* (i8*, i8*) @strcpy(i8* %.")
                out.push(assign_variable_at_eol)
                out.push(", i8* %.")
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
        if compiling_function == 1 then
            function_out.push("\n")
            function_out.push(out)
        else
            print(out)
        end
        out = ""
    end
end

print("ret i8 0")
print("}")
print(function_out)
print(string_constant_declarations)
