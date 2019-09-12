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
-- タイムゾーン　タイマー開始時刻　現在時刻


type alias Model =
    { zone : Time.Zone
    , startedTime : Maybe Int
    , endedTime : Maybe Int
    , time : Time.Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { zone = Time.utc
      , startedTime = Nothing
      , endedTime = Nothing
      , time = Time.millisToPosix 0
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | TimerStart
    | TimerEnd
    | TimerReset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        TimerStart ->
            ( { model | startedTime = Just <| Time.posixToMillis model.time }
            , Cmd.none
            )

        TimerEnd ->
            ( { model | endedTime = Just <| Time.posixToMillis model.time }
            , Cmd.none
            )

        TimerReset ->
            ( { model | startedTime = Nothing, endedTime = Nothing }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewElapseTime (Time.posixToMillis model.time) model.startedTime model.endedTime
        , button [ onClick TimerStart ] [ text "start" ]
        , button [ onClick TimerEnd ] [ text "end" ]
        , button [ onClick TimerReset ] [ text "reset" ]
        ]


viewElapseTime : Int -> Maybe Int -> Maybe Int -> Html Msg
viewElapseTime now mStarted mEnded =
    case ( mStarted, mEnded ) of
        ( Nothing, _ ) ->
            div [] []

        ( Just started, Nothing ) ->
            div [] [ text <| Round.round 1 <| calcSecDiff now started ]

        ( Just started, Just ended ) ->
            div [] [ text <| Round.round 1 <| calcSecDiff ended started ]


calcSecDiff : Int -> Int -> Float
calcSecDiff ended started =
    toFloat (ended - started) / 1000
