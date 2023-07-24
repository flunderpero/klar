struct Person begin
    name string
    age i32
end

fn create_person() Person begin
    let p = Person()
    p.name = "John"
    p.age = 42
    return p
end

fn main() i32 begin
    let p = create_person()
    print(p.name)
    let i = p.age
    print(i.to_string())
    return 0
end
