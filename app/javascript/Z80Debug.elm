module Z80Debug exposing (..)


debugEnabled =
    False


debugLog msg thingToLog thingToReturn =
    if debugEnabled then
        Debug.log msg thingToLog |> (\_ -> thingToReturn)

    else
        thingToReturn


debugTodo msg thingToLog thingToReturn =
    if debugEnabled then
        Debug.todo (msg ++ " " ++ thingToLog) |> (\_ -> thingToReturn)

    else
        thingToReturn
