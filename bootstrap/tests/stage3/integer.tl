let num = 40
let ok = "OK: to_string() == "
let n = "variable name should not clash with `num`"
let s = ""

-- Addition
print("Addition")
num = num + 2
s = num.to_string()
print(s)

-- Subtraction
print("Subtraction")
num = num - 2
s = num.to_string()
print(s)

if num == 42 then
    print("OK: `num` is 42")
end

if num == 99 then
    print("ERR: `num` is not 99")
end

-- Stringify
ok = ok + num.to_string()
print(ok)
ok = num.to_string()
print(ok)

-- Test for variable name clash.
n = "OK: no variable name clash"
print(n)
