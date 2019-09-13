module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Mixin
import Neat
import Neat.Layout
import Neat.Layout.Row exposing (defaultRow)
import Round
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = neatView
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { now : Time.Posix
    , termHistory : List Int
    , timer : Timer
    }


type Timer
    = Initial
    | Started Int Int
    | Stop Int Int Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { now = Time.millisToPosix 0
      , termHistory = []
      , timer = Initial
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | TimerStart
    | TimerEnd
    | TimerReset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model
                | now = newTime
              }
            , Cmd.none
            )

        TimerStart ->
            case model.timer of
                Initial ->
                    ( { model
                        | timer = Started 0 <| Time.posixToMillis model.now
                      }
                    , Cmd.none
                    )

                Stop splitTime started ended ->
                    ( { model
                        | timer = Started (splitTime - started + ended) (Time.posixToMillis model.now)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerEnd ->
            case model.timer of
                Started splitTime started ->
                    ( { model
                        | timer = Stop splitTime started (Time.posixToMillis model.now)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerReset ->
            case model.timer of
                Started splitTime started ->
                    ( { model
                        | timer = Initial
                        , termHistory =
                            List.append model.termHistory
                                [ splitTime - started + Time.posixToMillis model.now ]
                      }
                    , Cmd.none
                    )

                Stop splitTime started ended ->
                    ( { model
                        | timer = Initial
                        , termHistory =
                            List.append model.termHistory
                                [ splitTime - started + ended ]
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | timer = Initial
                      }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div [] <|
        List.append
            [ viewElapseTime (Time.posixToMillis model.now) model.timer
            , button [ class "button", class "is-success", onClick TimerStart ] [ text "start" ]
            , button [ class "button", onClick TimerEnd ] [ text "end" ]
            , button [ class "button", class "is-warning", onClick TimerReset ] [ text "reset" ]
            , div [ class "title" ] [ text "History" ]
            ]
        <|
            List.map
                (\x -> div [] [ text <| formatTerm x ])
                model.termHistory


viewElapseTime : Int -> Timer -> Html Msg
viewElapseTime now timer =
    case timer of
        Initial ->
            div [] [ text "0:0:0.0" ]

        Started splitTime started ->
            div [] [ text <| formatTerm <| (splitTime + now - started) ]

        Stop splitTime started ended ->
            div [] [ text <| formatTerm <| (splitTime + ended - started) ]


formatTerm : Int -> String
formatTerm time =
    let
        hours =
            String.fromInt <| time // 3600000

        minutes =
            String.fromInt <| modBy 3600000 time // 60000

        seconds =
            String.fromInt <| modBy 60000 time // 1000

        milliseconds =
            Round.floor 0 <| toFloat (modBy 1000 (time + 50)) / 100
    in
    String.join ":" [ hours, minutes, seconds ] ++ "." ++ milliseconds


type MyPadding
    = MyPadding


myPadding : Neat.IsPadding MyPadding
myPadding =
    Neat.IsPadding
        { rem = 0.6
        }


neatView : Model -> Html Msg
neatView model =
    div [] <|
        Neat.toPage <|
            Neat.setBoundary myPadding <|
                Neat.Layout.column <|
                    [ Neat.Layout.rowWith
                        { defaultRow
                            | horizontal = Neat.Layout.Row.HCenter
                        }
                        [ Neat.fromNoPadding myPadding <|
                            neatViewElapseTime (Time.posixToMillis model.now) model.timer
                        ]
                    , Neat.Layout.row <|
                        [ Neat.fromNoPadding myPadding <|
                            Neat.lift button
                                [ Mixin.class "button"
                                , Mixin.class "is-success"
                                , Mixin.fromAttribute <| style "width" "100px"
                                , Mixin.fromAttribute <| onClick TimerStart
                                ]
                                [ Neat.text "start" ]
                        , Neat.fromNoPadding myPadding <|
                            Neat.lift button
                                [ Mixin.class "button"
                                , Mixin.fromAttribute <| style "width" "100px"
                                , Mixin.fromAttribute <| onClick TimerEnd
                                ]
                                [ Neat.text "end" ]
                        , Neat.fromNoPadding myPadding <|
                            Neat.lift button
                                [ Mixin.class "button"
                                , Mixin.class "is-warning"
                                , Mixin.fromAttribute <| style "width" "100px"
                                , Mixin.fromAttribute <| onClick TimerReset
                                ]
                                [ Neat.text "reset" ]
                        ]
                    , Neat.Layout.row <|
                        [ Neat.fromNoPadding myPadding <|
                            Neat.div [ Mixin.class "title" ] [ Neat.text "History" ]
                        ]
                    , Neat.Layout.row <|
                        List.map
                            (\x -> Neat.fromNoPadding myPadding <| Neat.div [] [ Neat.text <| formatTerm x ])
                            model.termHistory
                    ]


neatViewElapseTime : Int -> Timer -> Neat.View Neat.NoPadding Msg
neatViewElapseTime now timer =
    case timer of
        Initial ->
            Neat.div [] [ Neat.text "0:0:0.0" ]

        Started splitTime started ->
            Neat.div [] [ Neat.text <| formatTerm <| (splitTime + now - started) ]

        Stop splitTime started ended ->
            Neat.div [] [ Neat.text <| formatTerm <| (splitTime + ended - started) ]
