let my_list = list()
let len = 0
let i = 0
let s = ""

my_list.push("First")
my_list.push("Hello")
my_list.push("Third")

print("length()")
len = my_list.length()
print(len)
print("")

print("pop()")
s = my_list.pop()
print(s)
print("")

print("Iterate")
loop 
    if i == my_list.length() then
        break
    end
    s = my_list.at(i)
    print(s)
    i = i + 1
end
print("")

print("x in list")
if "First" in my_list then
    print("OK: `First` is in the list")
else
    print("ERR: `First` should be in the list")
end
if "not_there" in my_list then
    print("ERR: `not_there` should not be in the list")
else
    print("OK: `not_there` is not in the list")
end
print("")

print("clear()")
my_list.clear()
len = my_list.length()
print(len)
