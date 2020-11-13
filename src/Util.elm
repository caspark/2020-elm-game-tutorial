module Util exposing (..)

{-| A bunch of utility functions that may be handy.
-}


{-| Wrapper around Debug.log that only logs if the provided boolean is True.
-}
logIf : Bool -> String -> a -> a
logIf condition msg item =
    if condition then
        Debug.log msg item

    else
        item
