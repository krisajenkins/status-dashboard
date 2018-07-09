module Main exposing (..)

import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (style)
import Json.Decode exposing (Decoder, decodeString, field, int)
import RemoteData exposing (RemoteData(Loading, Success))
import WebSocket exposing (keepAlive, listen)


type Msg
    = Msg (RemoteData String Int)


type alias Model =
    { counter : RemoteData String Int
    }


decodeCounter : Decoder Int
decodeCounter =
    field "counter" int


green : String
green =
    "#2dc937"


amber : String
amber =
    "#db7b2b"


red : String
red =
    "#cc3232"


view : Model -> Html Msg
view model =
    p
        [ style
            [ ( "padding", "10px" )
            , ( "margin", "10px" )
            , ( "color"
              , case model.counter of
                    Success n ->
                        if n < 3 then
                            green
                        else if n < 6 then
                            amber
                        else
                            red

                    _ ->
                        "black"
              )
            , ( "transition-property", "color" )
            , ( "transition-duration", "500ms" )
            ]
        ]
        [ text (toString model)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg counter ->
            ( { model
                | counter = counter
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        server =
            "ws://localhost:8080/ws"
    in
    Sub.batch
        [ listen server (decodeString decodeCounter >> RemoteData.fromResult >> Msg)
        , keepAlive server
        ]


init : ( Model, Cmd Msg )
init =
    ( { counter = Loading }, Cmd.none )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
