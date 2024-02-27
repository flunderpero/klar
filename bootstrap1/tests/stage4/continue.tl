let i = 0
let s = ""

loop 
    i = i + 1
    if i == 4 then
        break
    end
    if i == 2 then
        continue
    end
    s = i.to_string()
    print(s)
end
