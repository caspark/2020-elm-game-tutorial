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
import Canvas
import Canvas.Settings as CRender
import Canvas.Settings.Advanced exposing (rotate, transform, translate)
import Color
import Debug exposing (log)
import Dict exposing (Dict)
import Html exposing (Html, del, div)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Keyboard exposing (SupportedKey(..))
import Math exposing (clamp, clamp2Tuple, wrap)
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


shipRotationSpeed : Float
shipRotationSpeed =
    -- expressed in radians, picked by trial and error
    (Basics.pi / 2) / 200


shipThrustSpeed : Float
shipThrustSpeed =
    0.07


shipDrag : Float
shipDrag =
    0.98


shipMaxSpeed : Float
shipMaxSpeed =
    1


type alias Ship =
    { rotation : Float
    , position : ( Float, Float )
    , velocity : ( Float, Float )
    }


initShip : Ship
initShip =
    { rotation = Basics.pi * 3 / 2
    , position = ( centerX, centerY )
    , velocity = ( 0, 0 )
    }


type alias Model =
    { keyboard : Keyboard.KeyboardState
    , ship : Ship
    }


init : flags -> ( Model, Cmd msg )
init _ =
    ( { keyboard = Keyboard.init
      , ship = initShip
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
        isKeyDown =
            -- convenience function to avoid having to pass in model.keyboard all the time
            Keyboard.isKeyDown model.keyboard

        newShip =
            let
                ship =
                    model.ship

                newRotSpeed =
                    if isKeyDown LeftKey then
                        -shipRotationSpeed

                    else if isKeyDown RightKey then
                        shipRotationSpeed

                    else
                        0

                ( x, y ) =
                    ship.position

                ( vx, vy ) =
                    ship.velocity

                newVelocity =
                    Math.clamp2Tuple
                        ( -shipMaxSpeed
                        , shipMaxSpeed
                        )
                        (if isKeyDown UpKey then
                            ( vx + shipThrustSpeed * Basics.cos ship.rotation, vy + shipThrustSpeed * Basics.sin ship.rotation )

                         else
                            ( vx * shipDrag, vy * shipDrag )
                        )

                newPosition =
                    let
                        ( newX, newY ) =
                            ( x + vx * dt, y + vy * dt )
                    in
                    ( wrap ( 0, width ) newX
                    , wrap ( 0, height ) newY
                    )
            in
            { ship
                | rotation = ship.rotation + newRotSpeed * dt
                , position = newPosition
                , velocity = newVelocity
            }
    in
    { model
        | ship = newShip
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatedModel =
            case msg of
                Frame deltaTime ->
                    updateFrame model deltaTime

                KeyPressed key ->
                    case Keyboard.logKeyboardEvent "Key was pressed" key of
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
            (List.concat
                [ renderStarfield
                , renderShip model.ship
                ]
            )
        ]


renderStarfield : List Canvas.Renderable
renderStarfield =
    [ -- for now, the vastness of space is dark and empty
      Canvas.shapes [ CRender.fill Color.black ] [ Canvas.rect ( 0, 0 ) width height ]
    ]


renderShip : Ship -> List Canvas.Renderable
renderShip ship =
    let
        h =
            50

        w =
            h / 2

        ( x, y ) =
            ship.position

        rotation =
            ship.rotation
    in
    [ Canvas.shapes
        [ transform
            [ translate x y
            , rotate (rotation + Basics.pi / 2)
            , translate (-w / 2) (-h / 2)
            ]
        , CRender.fill
            (Color.rgb 255 255 255)
        ]
        [ triangle ( 0, 0 ) w h
        ]
    ]


triangle : ( Float, Float ) -> Float -> Float -> Canvas.Shape
triangle ( x, y ) w h =
    Canvas.path ( 0, 0 )
        [ Canvas.moveTo ( w / 2, 0 )
        , Canvas.lineTo
            ( w, h )
        , Canvas.lineTo ( 0, h )
        ]
