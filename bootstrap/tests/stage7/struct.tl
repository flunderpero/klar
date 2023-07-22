struct MyStruct begin
    name string
    age i32
    hobbies List<string>
end

fn print_struct(my_struct MyStruct) void begin
    let s = ""
    let s2 = ""
    let i = 0
    let num_hobbies = my_struct.hobbies.length()
    print("MyStruct:")
    s = "  name: "
    s2 = my_struct.name
    s.push(s2)
    print(s)
    s = "  age: "
    s2 = my_struct.age.to_string()
    s.push(s2)
    print(s)
    s = "  hobbies: "
    i = 0
    loop
        if i == num_hobbies then
            break
        end
        if i == 0 then
        else
            s.push(", ")
        end
        s2 = my_struct.hobbies.at(i)
        s.push(s2)
        i = i + 1
    end
    print(s)
end

fn inc_age(my_struct MyStruct) void begin
    my_struct.age = my_struct.age + 1
end

fn main() i32 begin
    let s = ""
    let i = 1
    let my_struct = MyStruct()
    my_struct.name = "Peter"
    my_struct.age = 42
    my_struct.hobbies.push("Running")
    print(my_struct.name)
    print(my_struct.age.to_string())
    print(my_struct.hobbies.length().to_string())
    my_struct.age = my_struct.age + 1
    print(my_struct.age.to_string())
    if my_struct.age == 43 then
        print("Age is 43")
    end
    if my_struct.age == 69 then
    else
        print("Age is not 69")
    end
    if my_struct.name == "Peter" then
        print("Name is Peter")
    end
    if my_struct.name == "Paul" then
    else
        print("Name is not Paul")
    end
    if "et" in my_struct.name then
        print("'et' is in name")
    end
    if "ou" in my_struct.name then
    else
        print("'ou' is not in name")
    end
    if "Running" in my_struct.hobbies then
        print("Running is in hobbies")
    end
    if "Lifting" in my_struct.hobbies then
    else
        print("Lifting is not in hobbies")
    end
    s = my_struct.hobbies.pop()
    s.push(" has been removed from hobbies")
    print(s)
    print(my_struct.hobbies.length().to_string())
    print("clear:")
    my_struct.hobbies.push("Lifting")
    my_struct.hobbies.push("Running")
    print(my_struct.hobbies.length().to_string())
    my_struct.hobbies.clear()
    print(my_struct.hobbies.length().to_string())
    print("function argument:")
    my_struct.hobbies.push("Lifting")
    my_struct.hobbies.push("Running")
    print_struct(my_struct)
    inc_age(my_struct)
    print(my_struct.age.to_string())
    return 0
end
```
