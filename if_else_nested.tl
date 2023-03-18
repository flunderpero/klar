word = "Hello"

if word == "Hello" then
    print ( "OK: Level 1" )
    if word == "there" then
        print ( "ERR: Level 2")
        if word == "Hello" then
            print ( "ERR: Level 2 nested if")
        end
    else
        print ( "OK: Level 2" )
        if "Hello" == "Hello" then
            print ( "OK: Level 3" )
        else
            print ( "ERR: Level 3" )
        end
    end
else
    print ( "ERR: Level 1" )
end
