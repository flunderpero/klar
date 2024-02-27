let s = ""

loop 
    if s == "1111" then
        break
    end
    s = s + "1"
end

loop 
    if s == "1111222" then
        break
    end
    s = s + "2"
end

print(s)
