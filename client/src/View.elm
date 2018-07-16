module View exposing (root)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, classList, style)
import RemoteData exposing (RemoteData(..))
import Types exposing (Model, Msg, OverallStatus(..))


white : String
white =
    "#eee"


grey : String
grey =
    "#ccc"


green : String
green =
    "#2dc937"


amber : String
amber =
    "#db7b2b"


red : String
red =
    "#cc3232"


root : Model -> Html Msg
root model =
    let
        backgroundColor =
            case model.status of
                Success Good ->
                    green

                Success Unknown ->
                    amber

                Success Bad ->
                    red

                Failure _ ->
                    red

                Loading ->
                    grey

                NotAsked ->
                    white
    in
    div
        [ style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "background-color", backgroundColor )
            , ( "transition-property", "background-color" )
            , ( "transition-duration", "500ms" )
            ]
        , class "status"
        ]
        [ text " " ]
