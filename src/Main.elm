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
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onKeyPress, onKeyUp)
import Browser.Navigation exposing (Key)
import Canvas exposing (rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, del, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Keyboard exposing (SupportedKey(..))
import Set exposing (Set)
import Util exposing (logIf)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { rotationSpeed : Float -- how fast should the square spin?
    , rotation : Float -- what's the current rotation of the square?
    , spinningPaused : Bool -- is the square's spinning paused?
    , keyboard : Keyboard.KeyboardState
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { rotationSpeed = 0.25
      , rotation = 0
      , spinningPaused = False
      , keyboard = Keyboard.init
      }
    , Cmd.none
    )


type Msg
    = Frame Float -- float = dt, aka delta time: the amount of time elapsed since the last frame
    | KeyPressed SupportedKey
    | RawKeyDowned String
    | RawKeyUpped String


{-| Contains the main game update logic.

updateFrame model dt = This function is called roughly once per monitor refresh (so 60 times a
second if your monitor runs at 60hz); if you trace back how this is called, you'll see that it's
hooked into the Elm equivalent of
<https://developer.mozilla.org/en-US/docs/Web/API/window/requestAnimationFrame>

The exact time between runs varies depending on CPU load, which is what the dt param is for: it
tracks the amount of time elapsed since this function was called last, so any time you move anything
in the game world, the movement needs to be multiplied by dt. That way you won't have e.g. a
character run slower on a slow computer than a fast computer.

-}
updateFrame : Model -> Float -> Model
updateFrame model dt =
    let
        adjustedRotationSpeed =
            if model.spinningPaused then
                0

            else if Keyboard.isKeyDown model.keyboard EnterKey then
                model.rotationSpeed / 4

            else
                model.rotationSpeed
    in
    { model | rotation = model.rotation + adjustedRotationSpeed * dt }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatedModel =
            case msg of
                Frame deltaTime ->
                    updateFrame model deltaTime

                KeyPressed key ->
                    case Keyboard.logKeyboardEvent "Key was pressed" key of
                        SpaceKey ->
                            { model | spinningPaused = not model.spinningPaused }

                        _ ->
                            model

                RawKeyDowned keyName ->
                    { model | keyboard = Keyboard.handleKeyDowned model.keyboard keyName }

                RawKeyUpped keyName ->
                    { model | keyboard = Keyboard.handleKeyUpped model.keyboard keyName }
    in
    ( updatedModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta Frame
        , onKeyPress (Decode.map KeyPressed Keyboard.decodeKeyEventToSupportedKey)
        , onKeyDown (Decode.map RawKeyDowned Keyboard.decodeKeyEventToKeyName)
        , onKeyUp (Decode.map RawKeyUpped Keyboard.decodeKeyEventToKeyName)
        ]


width : number
width =
    800


height : number
height =
    600


centerX : Float
centerX =
    width / 2


centerY : Float
centerY =
    height / 2


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ Canvas.toHtml
            ( width, height )
            [ style "border" "10px solid rgba(0,0,0,0.1)" ]
            [ clearScreen
            , render model
            ]
        ]


clearScreen : Canvas.Renderable
clearScreen =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) width height ]


render : Model -> Canvas.Renderable
render model =
    let
        rectSize =
            width / 3

        x =
            -(rectSize / 2)

        y =
            -(rectSize / 2)

        rotation =
            degrees model.rotation

        hue =
            toFloat (model.rotation / 4 |> floor |> modBy 100) / 100
    in
    -- Read the elm-canvas docs to understand how to use `shapes`:
    -- https://package.elm-lang.org/packages/joakin/elm-canvas/latest/
    shapes
        [ transform
            [ translate centerX centerY
            , rotate rotation
            ]
        , fill (Color.hsl hue 0.3 0.7)
        ]
        [ rect ( x, y ) rectSize rectSize ]
