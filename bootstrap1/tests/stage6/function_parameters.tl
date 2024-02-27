fn append_str(mut out string, s string) void begin
    out.push(s)
end

fn add(a i32, b i32) i32 begin
    let c = a + b
    return c
end

fn return_str() string begin
    let res = "returns a string" 
    return res
end

fn print_list(list List<string>) void begin
    let i = 0
    let s = ""
    loop
        if i == list.length() then
            break
        end
        s = list.at(i) 
        print(s)
        i = i + 1
    end
end

fn main() i32 begin
    print("add():")
    let my_num = add(2, 3)
    print(my_num.to_string())

    print("\nappend_str():")
    let my_string = "Hi"
    append_str(my_string, " there!")
    print(my_string)

    print("\nreturn_str():")
    let s = return_str()
    my_string.push(" ").push(s)
    print(my_string)

    print("\nprint_list():")
    let my_list = list()
    my_list.push("first")
    my_list.push("second")
    print_list(my_list)
    return 0
end
