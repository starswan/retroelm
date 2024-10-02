module Z80Debug exposing (..)


debugLog msg thingToLog thingToReturn =
        Debug.log msg thingToLog |> (\_ -> thingToReturn)


debugTodo msg thingToLog thingToReturn =
        Debug.todo (msg ++ " " ++ thingToLog) |> (\_ -> thingToReturn)
