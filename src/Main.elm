module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Round
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { time : Time.Posix
    , termHistory : List Int
    , timer : Timer
    }


type Timer
    = Initial
    | Started Int Int
    | Stop Int Int Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = Time.millisToPosix 0
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
                | time = newTime
              }
            , Cmd.none
            )

        TimerStart ->
            case model.timer of
                Initial ->
                    ( { model
                        | timer = Started 0 <| Time.posixToMillis model.time
                      }
                    , Cmd.none
                    )

                Stop splitTime started ended ->
                    ( { model
                        | timer = Started (splitTime - started + ended) (Time.posixToMillis model.time)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerEnd ->
            case model.timer of
                Started splitTime started ->
                    ( { model
                        | timer = Stop splitTime started (Time.posixToMillis model.time)
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
                                [ splitTime - started + Time.posixToMillis model.time ]
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
            [ viewElapseTime (Time.posixToMillis model.time) model.timer
            , button [ onClick TimerStart ] [ text "start" ]
            , button [ onClick TimerEnd ] [ text "end" ]
            , button [ onClick TimerReset ] [ text "reset" ]
            , h3 [] [ text "History" ]
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
