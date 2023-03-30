let i = 0
let j = 0
let s = ""

loop
    if i == 3 then
        break
    end
    if "Hi" == "Hi" then
        print("OK: `Hi` matched")
        j = 0
        loop
            if j == 3 then 
                break
            end
            s = j.to_string() 
            s = s + " / " 
            s = s + i.to_string()
            print(s)
            j = j + 1
        end
    else
        print("ERR: `Hi` did not match")
    end
    i = i + 1
end
