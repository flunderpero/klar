let s = "Hi there"
let needle = "the"
let not_needle = "not"
if "the" in s then
    print("OK: 'the' has been found")
end
if "not" in s then
    print("ERR: 'not' has been found")
end
if needle in s then
    print("OK: 'the' (from var) has been found")
end
if not_needle in s then
    print("ERR: 'not' (from var) has been found")
end
