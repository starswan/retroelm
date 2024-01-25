module Z80Debug exposing (..)

debug_log msg thingToLog thingToReturn =
   --Debug.log msg thingToLog |> (\_ -> thingToReturn)
   thingToReturn

debug_todo msg thingToLog thingToReturn =
   --Debug.todo (msg ++ " " ++ thingToLog) |> (\_ -> thingToReturn)
   thingToReturn
