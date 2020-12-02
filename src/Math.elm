module Math exposing (..)

import Util exposing (map2Tuple)


wrap : ( comparable, comparable ) -> comparable -> comparable
wrap ( min, max ) val =
    if val < min then
        max

    else if val > max then
        min

    else
        val


clamp : ( comparable, comparable ) -> comparable -> comparable
clamp ( min, max ) val =
    Basics.min max (Basics.max min val)


{-| Restrict both values of a 2-tuple to be within the given range

Note that using this for e.g. clamping velocity in 2D physics is _technically_ wrong because it
makes it faster to move diagonally than horizontally or vertically:

`clamp2Tuple (-1, 1) (2, 2)` will give you a speed of `(1, 1)`, aka `sqrt(2)`, but
`clamp2Tuple (-1, 1) (4, 0)` will give you a speed of `(1, 0)`, aka `sqrt(1)`.

-}
clamp2Tuple : ( comparable, comparable ) -> ( comparable, comparable ) -> ( comparable, comparable )
clamp2Tuple ( min, max ) tuple =
    map2Tuple (clamp ( min, max )) tuple
