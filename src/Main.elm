module Main exposing (main)

import Browser
import Html exposing (Html, button, div)
import Html.Attributes exposing (contenteditable, style)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Json.Decode
import Mixin
import Neat
import Neat.Layout
import Neat.Layout.Row exposing (defaultRow)
import Time



-- MAIN


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { now : Time.Posix
    , termHistory : List TitledSplitTimes
    , mainSubTimer : MainSubTimer
    , title : String
    }


type alias SplitTimes =
    { mainSplitTime : Int
    , subSplitTime : Int
    }


type alias TitledSplitTimes =
    { title : String
    , mainSplitTime : Int
    , subSplitTime : Int
    }


type MainSubTimer
    = Initial
    | MainStarted SplitTimes Int
    | SubStarted SplitTimes Int
    | MainStop SplitTimes
    | SubStop SplitTimes


init : () -> ( Model, Cmd Msg )
init _ =
    ( { now = Time.millisToPosix 0
      , termHistory = []
      , mainSubTimer = Initial
      , title = "title"
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | UpdateTitle String
    | TimerStart
    | TimerStop
    | TimerReset
    | TimerSwitch
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nowPosix =
            Time.posixToMillis model.now

        timerStopSwitchStart =
            update TimerStart
                << (Tuple.first << update TimerSwitch)
                << (Tuple.first << update TimerStop)
    in
    case msg of
        Tick newTime ->
            ( { model | now = newTime }
            , Cmd.none
            )

        UpdateTitle newTitle ->
            ( { model | title = newTitle }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        TimerStart ->
            case model.mainSubTimer of
                Initial ->
                    ( { model
                        | mainSubTimer = MainStarted (SplitTimes 0 0) nowPosix
                      }
                    , Cmd.none
                    )

                MainStop splitTimes ->
                    ( { model | mainSubTimer = MainStarted splitTimes nowPosix }
                    , Cmd.none
                    )

                SubStop splitTimes ->
                    ( { model | mainSubTimer = SubStarted splitTimes nowPosix }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerSwitch ->
            case model.mainSubTimer of
                MainStarted _ _ ->
                    timerStopSwitchStart model

                SubStarted _ _ ->
                    timerStopSwitchStart model

                MainStop splitTimes ->
                    ( { model | mainSubTimer = SubStop splitTimes }
                    , Cmd.none
                    )

                SubStop splitTimes ->
                    ( { model | mainSubTimer = MainStop splitTimes }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerStop ->
            case model.mainSubTimer of
                MainStarted splitTimes started ->
                    ( { model
                        | mainSubTimer =
                            MainStop
                                { splitTimes | mainSplitTime = splitTimes.mainSplitTime - started + nowPosix }
                      }
                    , Cmd.none
                    )

                SubStarted splitTimes started ->
                    ( { model
                        | mainSubTimer =
                            SubStop
                                { splitTimes | subSplitTime = splitTimes.subSplitTime - started + nowPosix }
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerReset ->
            case model.mainSubTimer of
                MainStarted splitTimes started ->
                    let
                        mainSplitTime =
                            splitTimes.mainSplitTime - started + nowPosix
                    in
                    ( { model
                        | mainSubTimer = Initial
                        , termHistory =
                            model.termHistory
                                ++ [ TitledSplitTimes model.title mainSplitTime splitTimes.subSplitTime ]
                      }
                    , Cmd.none
                    )

                MainStop splitTimes ->
                    ( { model
                        | mainSubTimer = Initial
                        , termHistory =
                            model.termHistory
                                ++ [ TitledSplitTimes model.title splitTimes.mainSplitTime splitTimes.subSplitTime ]
                      }
                    , Cmd.none
                    )

                SubStarted splitTimes started ->
                    let
                        subSplitTime =
                            splitTimes.subSplitTime - started + nowPosix
                    in
                    ( { model
                        | mainSubTimer = Initial
                        , termHistory =
                            model.termHistory
                                ++ [ TitledSplitTimes model.title splitTimes.mainSplitTime subSplitTime ]
                      }
                    , Cmd.none
                    )

                SubStop splitTimes ->
                    ( { model
                        | mainSubTimer = Initial
                        , termHistory =
                            model.termHistory
                                ++ [ TitledSplitTimes model.title splitTimes.mainSplitTime splitTimes.subSplitTime ]
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | mainSubTimer = Initial
                      }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (toFloat timeUnit) Tick



-- VIEW


timeUnit : Int
timeUnit =
    100


formatTerm : Int -> String
formatTerm time =
    let
        millisec1Hour =
            3600000

        millisec1Minute =
            60000

        millisec1Second =
            1000

        hours =
            String.fromInt <| time // millisec1Hour

        minutes =
            String.fromInt <| modBy millisec1Hour time // millisec1Minute

        seconds =
            String.fromInt <| modBy millisec1Minute time // millisec1Second

        milliseconds =
            String.fromInt <| modBy millisec1Second (time + timeUnit // 2) // timeUnit
    in
    String.join ":" [ hours, minutes, seconds ] ++ "." ++ milliseconds


type MyPadding
    = MyPadding


myPadding : Neat.IsPadding MyPadding
myPadding =
    Neat.IsPadding
        { rem = 0.6
        }


enterCode : Int
enterCode =
    13


isEnterCode : Int -> Bool
isEnterCode code =
    code == enterCode


view : Model -> Html Msg
view model =
    let
        addMyPadding =
            Neat.fromNoPadding myPadding
    in
    div [] <|
        Neat.toPage <|
            Neat.setBoundary myPadding <|
                Neat.Layout.column <|
                    [ Neat.Layout.rowWith
                        { defaultRow | horizontal = Neat.Layout.Row.HCenter }
                      <|
                        List.map addMyPadding
                            [ Neat.div
                                [ Mixin.fromAttributes
                                    [ contenteditable True
                                    , preventDefaultOn "keydown"
                                        (Json.Decode.map (\x -> ( NoOp, isEnterCode x )) <| Html.Events.keyCode)
                                    , on "blur" <|
                                        Json.Decode.map UpdateTitle
                                            (Json.Decode.at [ "target", "textContent" ] Json.Decode.string)
                                    ]
                                ]
                                [ Neat.text model.title ]
                            , viewElapseTime (Time.posixToMillis model.now) model.mainSubTimer
                            ]
                    , Neat.Layout.row <|
                        List.map addMyPadding
                            [ startButton, switchButton, endButton, resetButton ]
                    , Neat.Layout.row <|
                        List.map addMyPadding
                            [ historyTitle ]
                    , Neat.Layout.row [ addMyPadding <| Neat.div [] [ Neat.text "main sub" ] ]
                    ]
                        ++ List.map
                            (Neat.Layout.row << List.singleton << addMyPadding << termDiv)
                            model.termHistory


viewElapseTime : Int -> MainSubTimer -> Neat.View Neat.NoPadding Msg
viewElapseTime now timer =
    case timer of
        Initial ->
            Neat.div [] [ Neat.text <| String.join " " [ "0:0:0.0", "0:0:0.0" ] ]

        MainStarted { mainSplitTime, subSplitTime } started ->
            Neat.div []
                [ Neat.text <|
                    String.join " " <|
                        List.map formatTerm [ mainSplitTime + now - started, subSplitTime ]
                ]

        MainStop { mainSplitTime, subSplitTime } ->
            Neat.div []
                [ Neat.text <|
                    String.join " " <|
                        List.map formatTerm [ mainSplitTime, subSplitTime ]
                ]

        SubStarted { mainSplitTime, subSplitTime } started ->
            Neat.div []
                [ Neat.text <|
                    String.join " " <|
                        List.map formatTerm [ mainSplitTime, subSplitTime + now - started ]
                ]

        SubStop { mainSplitTime, subSplitTime } ->
            Neat.div []
                [ Neat.text <|
                    String.join " " <|
                        List.map formatTerm [ mainSplitTime, subSplitTime ]
                ]


termDiv : TitledSplitTimes -> Neat.View Neat.NoPadding Msg
termDiv splitTimes =
    Neat.div []
        [ Neat.text <|
            String.join " " <|
                [ splitTimes.title, formatTerm splitTimes.mainSplitTime, formatTerm splitTimes.subSplitTime ]
        ]


historyTitle : Neat.View Neat.NoPadding Msg
historyTitle =
    Neat.div [ Mixin.class "title" ] [ Neat.text "History" ]


startButton : Neat.View Neat.NoPadding Msg
startButton =
    Neat.lift button
        [ Mixin.class "button"
        , Mixin.class "is-success"
        , Mixin.fromAttribute <| style "width" "100px"
        , Mixin.fromAttribute <| onClick TimerStart
        ]
        [ Neat.text "start" ]


switchButton : Neat.View Neat.NoPadding Msg
switchButton =
    Neat.lift button
        [ Mixin.class "button"
        , Mixin.fromAttribute <| style "width" "100px"
        , Mixin.fromAttribute <| onClick TimerSwitch
        ]
        [ Neat.text "switch" ]


endButton : Neat.View Neat.NoPadding Msg
endButton =
    Neat.lift button
        [ Mixin.class "button"
        , Mixin.fromAttribute <| style "width" "100px"
        , Mixin.fromAttribute <| onClick TimerStop
        ]
        [ Neat.text "end" ]


resetButton : Neat.View Neat.NoPadding Msg
resetButton =
    Neat.lift button
        [ Mixin.class "button"
        , Mixin.class "is-warning"
        , Mixin.fromAttribute <| style "width" "100px"
        , Mixin.fromAttribute <| onClick TimerReset
        ]
        [ Neat.text "reset" ]
