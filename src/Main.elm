module Main exposing (main)

import Browser
import Html exposing (Html, button, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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
    , termHistory : List Int
    , mainSubTimer : MainSubTimer
    }


type MainSubTimer
    = Initial
    | MainStarted ( Int, Int ) Int
    | SubStarted ( Int, Int ) Int
    | MainStop ( Int, Int )
    | SubStop ( Int, Int )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { now = Time.millisToPosix 0
      , termHistory = []
      , mainSubTimer = Initial
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | TimerStart
    | TimerEnd
    | TimerReset
    | TimerSwitch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nowPosix =
            Time.posixToMillis model.now
    in
    case msg of
        Tick newTime ->
            ( { model | now = newTime }
            , Cmd.none
            )

        TimerStart ->
            case model.mainSubTimer of
                Initial ->
                    ( { model
                        | mainSubTimer = MainStarted ( 0, 0 ) nowPosix
                      }
                    , Cmd.none
                    )

                MainStop splitTime ->
                    ( { model | mainSubTimer = MainStarted splitTime nowPosix }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerSwitch ->
            ( model, Cmd.none )

        TimerEnd ->
            case model.mainSubTimer of
                MainStarted ( mainSplitTime, subSplitTime ) started ->
                    ( { model | mainSubTimer = MainStop ( mainSplitTime - started + nowPosix, subSplitTime ) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TimerReset ->
            case model.mainSubTimer of
                MainStarted ( splitTime, _ ) started ->
                    ( { model
                        | mainSubTimer = Initial
                        , termHistory =
                            List.append model.termHistory
                                [ splitTime - started + nowPosix ]
                      }
                    , Cmd.none
                    )

                MainStop ( splitTime, _ ) ->
                    ( { model
                        | mainSubTimer = Initial
                        , termHistory =
                            List.append model.termHistory
                                [ splitTime ]
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
                            [ viewElapseTime (Time.posixToMillis model.now) model.mainSubTimer ]
                    , Neat.Layout.row <|
                        List.map addMyPadding
                            [ startButton, endButton, resetButton ]
                    , Neat.Layout.row <|
                        List.map addMyPadding
                            [ historyTitle ]
                    , Neat.Layout.row <|
                        List.map
                            (addMyPadding << termDiv)
                            model.termHistory
                    ]


viewElapseTime : Int -> MainSubTimer -> Neat.View Neat.NoPadding Msg
viewElapseTime now timer =
    case timer of
        Initial ->
            Neat.div [] [ Neat.text "0:0:0.0" ]

        MainStarted ( splitTime, _ ) started ->
            Neat.div [] [ Neat.text <| formatTerm <| (splitTime + now - started) ]

        MainStop ( splitTime, _ ) ->
            Neat.div [] [ Neat.text <| formatTerm splitTime ]

        _ ->
            Neat.div [] []


termDiv : Int -> Neat.View Neat.NoPadding Msg
termDiv x =
    Neat.div [] [ Neat.text <| formatTerm x ]


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
        , Mixin.class "is-success"
        , Mixin.fromAttribute <| style "width" "100px"
        , Mixin.fromAttribute <| onClick TimerSwitch
        ]
        [ Neat.text "switch" ]


endButton : Neat.View Neat.NoPadding Msg
endButton =
    Neat.lift button
        [ Mixin.class "button"
        , Mixin.fromAttribute <| style "width" "100px"
        , Mixin.fromAttribute <| onClick TimerEnd
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
