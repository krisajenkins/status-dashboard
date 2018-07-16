module Types exposing (..)

import Json.Decode exposing (Decoder, andThen, decodeString, field, string, succeed)
import Navigation exposing (Location)
import RemoteData exposing (RemoteData(Loading, Success))


type alias WebsocketData a =
    RemoteData String a


type Msg
    = WebsocketMsg (WebsocketData OverallStatus)
    | LocationChanged Location


type OverallStatus
    = Good
    | Bad
    | Unknown


type alias Model =
    { status : WebsocketData OverallStatus
    , location : Location
    }


decodeStatus : Decoder OverallStatus
decodeStatus =
    field "status" string
        |> andThen
            (\str ->
                case str of
                    "Good" ->
                        succeed Good

                    "Bad" ->
                        succeed Bad

                    "Unknown" ->
                        succeed Unknown

                    _ ->
                        succeed Unknown
            )
