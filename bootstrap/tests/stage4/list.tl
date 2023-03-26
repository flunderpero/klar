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

loop 
    if i == my_list.length() then
        break
    end
    s = my_list.at(i)
    print(s)
    i = i + 1
end
