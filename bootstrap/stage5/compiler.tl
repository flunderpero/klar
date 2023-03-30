-- Bootstrap stage 5
--
-- We finally have enough syntax to separate the lexer
-- from the parser and overall produce code that does
-- not look like spaghetti thrown at the wall.
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
            constant_definitions = constant_definitions + "\n%."
            constant_definitions = constant_definitions + id
            constant_definitions = constant_definitions + " = alloca i32, align 4\n"
            constant_definitions = constant_definitions + "store i32 "
            constant_definitions = constant_definitions + token_value
            constant_definitions = constant_definitions + ", i32* %."
            constant_definitions = constant_definitions + id
            token_types.push("int")
            token_values.push(id)
            token_value = ""
            parsing_int = 0
        else
            token_value = token_value + c
            continue
        end
    end
    c_is_int = 0

    if parsing_string == 1 then
        if c == parsing_string_delimiter then
            id_counter = id_counter + 1
            id = id_counter.to_string()
            string_constant_declarations = string_constant_declarations + "@."
            string_constant_declarations = string_constant_declarations + id
            string_constant_declarations = string_constant_declarations + " = internal constant ["
            string_constant_declarations = string_constant_declarations + parsing_string_length.to_string()
            string_constant_declarations = string_constant_declarations + ' x i8] c"'
            string_constant_declarations = string_constant_declarations + token_value
            string_constant_declarations = string_constant_declarations + "\\"
            string_constant_declarations = string_constant_declarations + "00"
            string_constant_declarations = string_constant_declarations + '"\n'
            constant_definitions = constant_definitions + "\n%." 
            constant_definitions = constant_definitions + id
            constant_definitions = constant_definitions + " = bitcast ["
            constant_definitions = constant_definitions + parsing_string_length.to_string()
            constant_definitions = constant_definitions + " x i8]* @."
            constant_definitions = constant_definitions + id
            constant_definitions = constant_definitions + " to i8*"
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
                    token_value = token_value + "\\0a"
                    parsing_string_length = parsing_string_length + 1
                end
                if c == "\\" then
                    token_value = token_value + "\\\\"
                    parsing_string_length = parsing_string_length + 1
                end
                parsing_string_escape_sequence = 0
                continue
            end
            if c == '"' then
                -- This is a LLVM IR string escape sequence.
                token_value = token_value + "\\22"
                parsing_string_length = parsing_string_length + 1
                parsing_string_escape_sequence = 0
                continue
            end
            if c == "\\" then
                parsing_string_escape_sequence = 1
                continue
            end
            token_value = token_value + c
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
        token_value = token_value + c
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
    out = out + "\n; TOKEN #"
    out = out + token_index.to_string()
    out = out + ": token_type="
    out = out + token_type
    out = out + ": token_value="
    out = out + token_value
    
    if next_token_type == "dot" then
        obj_var = token_value
        token_index = token_index + 1
        token_value = token_values.at(token_index)
        if token_value == "to_string" then
            -- We shortcut this here, so we can use `int.to_string()` in
            -- string concatenations.
            out = out + "\n%.tostrload"
            out = out + id
            out = out + " = load i32, i32* %."
            out = out + obj_var
            out = out + "\n%.tostr"
            out = out + id
            out = out + " = call i32 (i8*, i8*, ...) @sprintf(i8* %.sprintf_buf"
            out = out + ", i8* %.int_to_string_sprintf_format, i32 %.tostrload"
            out = out + id
            out = out + ")"
            expression_op_rhs_reg = "sprintf_buf"
            expression_type = "string"
            token_index = token_index + 2
            continue
        end
        if token_value == "length" then
            -- We shortcut this here, so we can use `list.length()` in
            -- string concatenations.
            out = out + "\n%.loadlen"
            out = out + id
            out = out + " = load i32, i32* %.len_"
            out = out + obj_var
            expression_op_rhs_reg = "loadlen" + id
            expression_type = "int"
            token_index = token_index + 2
            continue
        end
        if token_value == "pop" then
            out = out + "\n; CALLING method=pop"
            out = out + ", obj_var="
            out = out + obj_var
            -- First, decrement `length`.
            out = out + "\n%.getlen_"
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
            out = out + "\n%.loadintvar_"
            out = out + id
            out = out + " = load i32, i32* %."
            out = out + token_value
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
            out = out + "\nbr i1 %." 
            out = out + expression_reg
            out = out + ", label %if" 
            out = out + id
            out = out + ", label %else" 
            out = out + id
            out = out + "\nif"
            out = out + id
            out = out + ":"
            continue
        end
        if token_value == "loop" then
            nesting_types.push("loop")
            nesting_ids.push(id)
            nesting_loop_ids.push(id)
            out = out + "\nbr label %loop" 
            out = out + id
            out = out + "\nloop"
            out = out + id
            out = out + ":"
            continue
        end
        if token_value == "break" then
            id = nesting_loop_ids.pop()
            nesting_loop_ids.push(id)
            out = out + "\nbr label %loop_end" 
            out = out + id
            continue
        end
        if token_value == "continue" then
            id = nesting_loop_ids.pop()
            nesting_loop_ids.push(id)
            out = out + "\nbr label %loop" 
            out = out + id
            continue
        end
        if token_value == "end" then
            s = nesting_types.pop()
            id = nesting_ids.pop()
            if s == "if" then
                out = out + "\nbr label %else"
                out = out + id
                out = out + "\nelse"
                out = out + id
                out = out + ":"
            end
            if s == "else" then
                out = out + "\nbr label %else_end"
                out = out + id
                out = out + "\nelse_end"
                out = out + id
                out = out + ":"
            end
            if s == "loop" then
                out = out + "\nbr label %loop"
                out = out + id
                out = out + "\nloop_end"
                out = out + id
                out = out + ":"
            end
            continue
        end
        if token_value == "else" then
            s = nesting_types.pop()
            id = nesting_ids.pop()
            nesting_types.push("else")
            nesting_ids.push(id)
            out = out + "\nbr label %else_end"
            out = out + id
            out = out + "\nelse"
            out = out + id
            out = out + ":"
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
        out = out + "\n%.loadintconst_"
        out = out + id
        out = out + " = load i32, i32* %."
        out = out + token_value
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
            out = out + "\n\n; ADD expression_reg="
            out = out + expression_reg
            out = out + ", expression_op_rhs_reg="
            out = out + expression_op_rhs_reg
            out = out + ", expression_type="
            out = out + expression_type
            if expression_type == "string" then
                if assign_variable_at_eol == expression_reg then
                    -- Concatenate with expression_op_rhs_reg.
                    out = out + "\n%.addstr"
                    out = out + id
                    out = out + " = call i8* (i8*, i8*) @strcat(i8* %."
                    out = out + assign_variable_at_eol
                    out = out + ", i8* %."
                    out = out + expression_op_rhs_reg
                    out = out + ")\n"
                    expression_reg = "addstr" + id 
                else
                    -- If we are not assigning the expression to a variable, 
                    -- create a new string with enough room. 
                    out = out + "\n%.add_alloca"
                    out = out + id
                    out = out + " = alloca [500 x i8]\n"
                    out = out + "%.add" 
                    out = out + id
                    out = out + " = bitcast [500 x i8]* %.add_alloca"
                    out = out + id
                    out = out + " to i8*\n"
                    -- Copy the last expression_reg into the new string.
                    out = out + "%.addcpy"
                    out = out + id
                    out = out + " = call i8* (i8*, i8*) @strcpy(i8* %.add"
                    out = out + id
                    out = out + ", i8* %."
                    out = out + expression_reg
                    out = out + ")"
                    -- Now concatenate this with expression_op_rhs_reg.
                    out = out + "\n%.addadd"
                    out = out + id
                    out = out + " = call i8* (i8*, i8*) @strcat(i8* %.add"
                    out = out + id
                    out = out + ", i8* %."
                    out = out + expression_op_rhs_reg
                    out = out + ")\n"
                    expression_reg = "add" + id
                end
            end
            if expression_type == "int" then
                out = out + "\n%.add"
                out = out + id
                out = out + " = add i32 %."
                out = out + expression_reg
                out = out + ", %."
                out = out + expression_op_rhs_reg
                expression_reg = "add" + id
            end
        end
        if expression_op == "subtract" then
            out = out + "\n\n; SUBTRACT expression_reg="
            out = out + expression_reg
            out = out + ", expression_op_rhs_reg="
            out = out + expression_op_rhs_reg
            out = out + ", expression_type="
            out = out + expression_type
            if expression_type == "int" then
                out = out + "\n%.sub"
                out = out + id
                out = out + " = sub i32 %."
                out = out + expression_reg
                out = out + ", %."
                out = out + expression_op_rhs_reg
                expression_reg = "sub" + id
            end
        end
        if expression_op == "eq" then
            if expression_type == "int" then
                out = out + "\n%.eqint"
                out = out + id
                out = out + " = icmp eq i32 %."
                out = out + expression_reg
                out = out + ", %."
                out = out + expression_op_rhs_reg 
                expression_reg = "eqint" + id
            end
            if expression_type == "string" then
                out = out + "\n%.strcmp"
                out = out + id
                out = out + " = call i32 (i8*, i8*) @strcmp(i8* %."
                out = out + expression_reg
                out = out + ", i8* %."
                out = out + expression_op_rhs_reg
                out = out + ")\n"
                out = out + "%.eqstr"
                out = out + id
                out = out + " = icmp eq i32 %.strcmp"
                out = out + id
                out = out + ", 0"
                expression_reg = "eqstr" + id
            end
        end
        if expression_op == "in" then
            -- `in
            if expression_type == "string" then
                out = out + "\n%.instrstr"  
                out = out + id
                out = out + " = call i8* (i8*, i8*) @strstr(i8* %."  
                out = out + expression_op_rhs_reg
                out = out + ", i8* %."
                out = out + expression_reg
                out = out + ")\n"
                out = out + "%.instrcmp"
                out = out + id
                out = out + " = icmp eq i8* %.instrstr"
                out = out + id
                out = out + ", null"
                out = out + "%.instr"
                out = out + id
                out = out + " = sub i1 1, %.instrcmp"
                out = out + id
                expression_reg = "instr" + id
            end
            if expression_type == "list" then
                out = out + "\n%.in_pos"
                out = out + id
                out = out + " = alloca i32\n"
                out = out + "%.getlen_"
                out = out + id
                out = out + " = load i32, i32* %.len_"
                out = out + expression_op_rhs_reg
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
                out = out + ", label %in_end" 
                out = out + id
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
                out = out + expression_op_rhs_reg
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
                out = out + expression_reg
                out = out + ")\n"
                out = out + "%.strcmpb"
                out = out + id
                out = out + " = icmp eq i32 %.strcmp"
                out = out + id
                out = out + ", 0\n"
                out = out + "br i1 %.strcmpb" 
                out = out + id
                out = out + ", label %in_end" 
                out = out + id
                out = out + ", label %in_loop" 
                out = out + id
                out = out + "\n"
                out = out + "in_end"
                out = out + id
                out = out + ":"
                out = out + "%.listin"
                out = out + id
                out = out + " = sub i1 1, %.idxcmp"
                out = out + id
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
                out = out + "\n; SETTING CALLARGS: expression_reg="
                out = out + expression_reg
                out = out + ", call_function="
                out = out + call_function
                if call_args == "" then
                else
                    call_args = call_args + ", "
                end
                if expression_type == "string" then
                    call_args = call_args + "i8* " 
                end
                call_args = call_args + "%." 
                call_args = call_args + expression_reg 
                expression_reg = ""
            end
        end
        handle = ""
    end

    -- Call functions.
    if token_type == "rparen" then
        out = out + "\n; CALLING: call_function="
        out = out + call_function
        out = out + ", call_args="
        out = out + call_args
        if call_function == "print" then
            out = out + "\n; PRINT call_args="
            out = out + call_args
            out = out + ", expression_type="
            out = out + expression_type
            out = out + "\n%.puts"
            out = out + id
            out = out + " = call i32 (i8*) @puts("
            out = out + call_args
            out = out + ")"
        end
        if call_function == "list" then
            expression_type = "list"
        end
        if call_function == "read" then
            -- Read from stdin. Evaluate to "" if we are at the end of input.
            out = out + "\n%.read"
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
            expression_reg = "read_buf"
            expression_type = "string"
        end
        if call_function == "push" then
            -- First, copy the current expression value (which is expected to be a string).
            out = out + "\n%.pushcopy_alloc"
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
            out = out + ", "
            out = out + call_args
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
        end
        if call_function == "at" then
            out = out + "\n%.atptr"
            out = out + id
            out = out + " = getelementptr [10000 x i8*], [10000 x i8*]* %." 
            out = out + obj_var
            out = out + ", i32 0, i32 "
            out = out + call_args
            out = out + "\n%.atload"
            out = out + id
            out = out + " = load i8*, i8** %.atptr"
            out = out + id
            expression_reg = "atload" + id
            expression_type = "string"
        end
        if call_function == "clear" then
            -- Just set `length` to 0.
            out = out + "\nstore i32 0"
            out = out + ", i32* %.len_"
            out = out + obj_var
            expression_reg = "" 
        end
        call_function = ""
    end

    -- Create and assign variables. 
    if next_token_type == "newline" then
        if create_variable_at_eol == "" then
        else
            -- Create variable.
            if expression_type == "int" then
                out = out + "\n%."
                out = out + create_variable_at_eol
                out = out + " = alloca i32, align 4"
                int_vars.push(create_variable_at_eol)
            end
            if expression_type == "list" then
                -- This is a list.
                out = out + "\n%."
                out = out + create_variable_at_eol
                out = out + " = alloca [10000 x i8*]"
                out = out + "\n%.len_"
                out = out + create_variable_at_eol
                out = out + " = alloca i32\n"
                out = out + "store i32 0, i32* %.len_"
                out = out + create_variable_at_eol
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
                out = out + "\n; CREATE STRING: var="
                out = out + create_variable_at_eol
                out = out + ", size="
                out = out + i.to_string()
                out = out + "\n%._"
                out = out + create_variable_at_eol
                out = out + " = alloca ["
                out = out + i.to_string()
                out = out + " x i8]\n"
                out = out + "%." 
                out = out + create_variable_at_eol
                out = out + " = bitcast ["
                out = out + i.to_string()
                out = out + " x i8]* %._"
                out = out + create_variable_at_eol
                out = out + " to i8*\n"
                out = out + "%." 
                out = out + create_variable_at_eol
                out = out + "_clear = call i32 @memset(i8* %."
                out = out + create_variable_at_eol
                out = out + ", i32 0, i32 "
                out = out + i.to_string()
                out = out + ")"
                string_vars.push(create_variable_at_eol)
            end
            create_variable_at_eol = ""
        end
        if assign_variable_at_eol == "" then
        else
            out = out + "\n; ASSIGN assign_variable_at_eol="
            out = out + assign_variable_at_eol
            out = out + ", expression_reg="
            out = out + expression_reg
            out = out + ", expression_type="
            out = out + expression_type
            if expression_type == "int" then
                out = out + "\nstore i32 %."
                out = out + expression_reg
                out = out + ", i32* %."
                out = out + assign_variable_at_eol
            end
            if expression_type == "string" then
                out = out + "\n%.strcpy"
                out = out + id
                out = out + " = call i8* (i8*, i8*) @strcpy(i8* %."
                out = out + assign_variable_at_eol
                out = out + ", i8* %."
                out = out + expression_reg
                out = out + ")"
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

print("ret i8 0")
print("}")
print(string_constant_declarations)
