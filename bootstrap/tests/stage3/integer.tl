let num = 40
let s = "OK: to_string() == "
let n = "variable name should not clash with `num`"

-- Addition
print("Addition")
num = num + 2
print(num)

-- Subtraction
print("Subtraction")
num = num - 2
print(num)

if num == 42 then
    print("OK: `num` is 42")
end

if num == 99 then
    print("ERR: `num` is not 99")
end

-- Stringify
s = s + num.to_string()
print(s)
s = num.to_string()
print(s)

-- Test for variable name clash.
n = "OK: no variable name clash"
print(n)
