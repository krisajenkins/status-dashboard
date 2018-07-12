module Main exposing (..)

import Html
import Json.Decode exposing (Decoder, decodeString, field, int)
import RemoteData exposing (RemoteData(Loading, Success))
import Types exposing (Model, Msg(WebsocketMsg, LocationChanged), decodeStatus)
import View exposing (root)
import WebSocket exposing (keepAlive, listen)
import Navigation exposing (Location)


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
        server =
            "ws://" ++ model.location.host ++ "/ws"
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
