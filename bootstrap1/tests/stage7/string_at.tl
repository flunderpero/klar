fn main() i32 begin
    let s = "Hello, world!"
    let s2 = ""
    let i = 0
    loop
        if i == s.length() then
            break
        end
        s2  = s.at(i)
        print(s2)
        i = i + 1
    end
    return 0
end
