module Main exposing (..)

import Json.Decode exposing (Decoder, decodeString, field, int)
import Navigation exposing (Location)
import RemoteData exposing (RemoteData(Loading, Success))
import Types exposing (Model, Msg(LocationChanged, WebsocketMsg), decodeStatus)
import View exposing (root)
import WebSocket exposing (keepAlive, listen)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WebsocketMsg status ->
            ( { model | status = status }
            , Cmd.none
            )

        LocationChanged location ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        scheme =
            case model.location.protocol of
                "https:" ->
                    "wss:"

                _ ->
                    "ws:"

        uri =
            case model.location.hostname of
                "localhost" ->
                    "/ws"

                "127.0.0.1" ->
                    "/ws"

                _ ->
                    "/healthcheck/ws"

        server =
            scheme ++ "//" ++ model.location.host ++ uri
    in
    Sub.batch
        [ listen server (decodeString decodeStatus >> RemoteData.fromResult >> WebsocketMsg)
        , keepAlive server
        ]


init : Location -> ( Model, Cmd Msg )
init location =
    ( { status = Loading
      , location = location
      }
    , Cmd.none
    )


locationParser : Location -> Msg
locationParser location =
    LocationChanged location


main : Program Never Model Msg
main =
    Navigation.program locationParser
        { init = init
        , subscriptions = subscriptions
        , view = root
        , update = update
        }
