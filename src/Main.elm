module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--     https://guide.elm-lang.org/architecture/buttons.html
--
--
-- If Elm's syntax looks weird to you, read this first:
--     https://guide.elm-lang.org/core_language.html
-- and here's a reference that might be handy:
--     https://elm-lang.org/docs/syntax
--

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array


-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL

type alias Model = 
  {
    history : List Int,
    curr : Int
  }

init : Model
init =
  { history = [], curr = 0 }

-- UPDATE

type Msg
    = Increment
    | Decrement | Square | Reset


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model |
              history = (model.curr + 3) :: model.history,
              curr = model.curr + 3
            }

        Decrement ->
            { model |
              history = (model.curr - 1) :: model.history,
              curr = model.curr - 1
            }

        Square ->
            { model |
              history = (model.curr * model.curr) :: model.history,
              curr = model.curr * model.curr
            }

        Reset ->
            { model |
              history = List.drop 1 model.history,
              curr =  Maybe.withDefault 0 (List.head (List.drop 1 model.history))
            }




-- VIEW

rowItem: String -> Html Msg
rowItem id =
    div []
        [ text id ]

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt (model.curr)) ]
        , button [ onClick Increment  ] [ text "+" ]
        , button [ onClick Square ] [ text "^"]
        , button [ onClick Reset, disabled (List.isEmpty model.history) ] [ text "<-"]
        , div [] (List.map rowItem ( List.reverse (List.map String.fromInt model.history )))
        ]

