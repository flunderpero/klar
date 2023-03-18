let word = "Hello"

if word == "Hello" then
    print("OK: Matched")
else
    print("ERR: Matched but else-branch is run???")
end

if word == "world" then
    print("ERR: No match but if-branch is run???")
else
    print("OK: No match")
end
