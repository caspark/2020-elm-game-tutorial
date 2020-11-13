module Keyboard exposing
    ( KeyboardState
    , SupportedKey(..)
    , decodeKeyEventToKeyName
    , decodeKeyEventToSupportedKey
    , handleKeyDowned
    , handleKeyUpped
    , init
    , isKeyDown
    , logKeyboardEvent
    )

import Dict exposing (Dict)
import Json.Decode as Decode
import Set exposing (Set)
import Util exposing (logIf)


type alias KeyboardState =
    { keysDown : Set String -- which keys are currently being held down? expected to be used with
    }


{-| Whether to log debug information about keyboard key events.

Change this to True if you need to debug your input handling.

-}
shouldLogKeyboardEvents : Bool
shouldLogKeyboardEvents =
    False


logKeyboardEvent : String -> a -> a
logKeyboardEvent msg =
    logIf shouldLogKeyboardEvents ("Keyboard: " ++ msg)


init : KeyboardState
init =
    { keysDown = Set.empty
    }


handleKeyDowned : KeyboardState -> String -> KeyboardState
handleKeyDowned keyboard keyName =
    let
        loggedKeyName =
            logIf (shouldLogKeyboardEvents && not (isKeyNameDown keyboard keyName)) "Keyboard: Key is now down" (stringToKey keyName)
    in
    { keyboard | keysDown = Set.insert keyName keyboard.keysDown }


handleKeyUpped : KeyboardState -> String -> KeyboardState
handleKeyUpped keyboard keyName =
    let
        loggedKeyName =
            logKeyboardEvent "Key is now released" (stringToKey keyName)
    in
    { keyboard | keysDown = Set.remove keyName keyboard.keysDown }


isKeyNameDown : KeyboardState -> String -> Bool
isKeyNameDown keyboardState keyName =
    Set.member keyName keyboardState.keysDown


isKeyDown : KeyboardState -> SupportedKey -> Bool
isKeyDown keyboardState key =
    isKeyNameDown keyboardState (keyToString key)


decodeKeyEventToSupportedKey : Decode.Decoder SupportedKey
decodeKeyEventToSupportedKey =
    Decode.map stringToKey (Decode.field "key" Decode.string)


decodeKeyEventToKeyName : Decode.Decoder String
decodeKeyEventToKeyName =
    Decode.field "key" Decode.string


type SupportedKey
    = SpaceKey
    | EnterKey
    | UnknownKey


keyNamesToKeys : Dict String SupportedKey
keyNamesToKeys =
    Dict.fromList
        [ ( " ", SpaceKey )
        , ( "Enter", EnterKey )
        ]


keyToString : SupportedKey -> String
keyToString key =
    -- because Elm _insists_ on not having any user-implementable typeclasses and also doesn't
    -- implement the `comparable` typeclass for us, we can't actually define equality for our
    -- `SupportedKey` type, which means we can't just look up the reverse of keyNamesToKeys to
    -- figure out what the string representation for a given SupportedKey is. So all keys need to be
    -- entered twice :(
    case key of
        SpaceKey ->
            " "

        EnterKey ->
            "Enter"

        _ ->
            "unknown key"


stringToKey : String -> SupportedKey
stringToKey rawKey =
    let
        maybeParsedKey =
            Dict.get rawKey keyNamesToKeys
    in
    Maybe.withDefault UnknownKey maybeParsedKey
