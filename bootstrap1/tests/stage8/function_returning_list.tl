fn create_list() List<string> begin
    let res = list()
    res.push("Hello")
    let i = res.length()
    let s = res.at(0)
    print(i.to_string())
    print(s)
    return res
end

fn main() i32 begin
    let my_list = create_list()
    my_list.push("World")
    let s = my_list.at(1)
    let i = my_list.length()
    print(i.to_string())
    print(s)
    return 0
end
