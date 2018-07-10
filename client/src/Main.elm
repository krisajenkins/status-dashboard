module Main exposing (..)

import Html
import Json.Decode exposing (Decoder, decodeString, field, int)
import RemoteData exposing (RemoteData(Loading, Success))
import Types exposing (Model, Msg(WebsocketMsg), decodeStatus)
import View exposing (root)
import WebSocket exposing (keepAlive, listen)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WebsocketMsg status ->
            ( { model | status = status }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        server =
            "ws://localhost:8080/ws"
    in
    Sub.batch
        [ listen server (decodeString decodeStatus >> RemoteData.fromResult >> WebsocketMsg)
        , keepAlive server
        ]


init : ( Model, Cmd Msg )
init =
    ( { status = Loading }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = root
        , update = update
        }
