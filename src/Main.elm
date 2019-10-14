module Main exposing (Model)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (..)
import Html.Attributes exposing (class, href, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List
import Random exposing (Generator)
import Task
import Time exposing (Posix)


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- Lower bound for how short the time limit can become


timerFuzz : Int
timerFuzz =
    500


timeLimit : Int -> Int
timeLimit rounds =
    2500 - rounds


type Screen
    = Home
    | About
    | Game
    | GameOver


type alias Model =
    { screen : Screen
    , rounds : Int
    , gameStart : Posix
    , gameEnd : Posix
    , timerStart : Posix
    , timerLatest : Posix
    , currentInteger : Int
    , keyboardHint : Bool
    }


type Msg
    = ShowHome
    | ShowAbout
    | EnterGame
    | ChoosePrime
    | ChooseNotPrime
    | StartTimer Posix
    | UpdateTimer Posix
    | HandleKeyboardEvent String
    | StartRound Int
    | StartGame Posix
    | FinishGame Posix


timeDifference : Posix -> Posix -> Int
timeDifference start finish =
    let
        startMs =
            Time.posixToMillis start

        finishMs =
            Time.posixToMillis finish
    in
    finishMs - startMs


timeInSeconds : Posix -> Posix -> String
timeInSeconds start finish =
    let
        duration =
            timeDifference start finish

        seconds =
            duration // 1000
    in
    String.fromInt seconds


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { screen = Home
      , rounds = 0
      , gameStart = Time.millisToPosix 0
      , gameEnd = Time.millisToPosix 0
      , timerStart = Time.millisToPosix 0
      , timerLatest = Time.millisToPosix 0
      , currentInteger = 1
      , keyboardHint = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.screen == Game then
        Sub.batch
            [ onKeyPress (Decode.map HandleKeyboardEvent keyDecoder)
            , Time.every 50 UpdateTimer
            ]

    else
        Sub.none


isPrimeBy : Int -> Int -> Bool
isPrimeBy i n =
    if i >= n then
        True

    else if modBy i n == 0 then
        False

    else
        isPrimeBy (i + 1) n


isPrime : Int -> Bool
isPrime int =
    if int == 1 then
        False

    else if int < 4 then
        True

    else
        isPrimeBy 2 int


finishGame : Model -> ( Model, Cmd Msg )
finishGame model =
    ( { model | screen = GameOver }, Task.perform FinishGame Time.now )


advanceGame : Model -> Bool -> ( Model, Cmd Msg )
advanceGame model prime =
    if prime == isPrime model.currentInteger then
        generateRound { model | rounds = model.rounds + 1 }

    else
        finishGame model


generateRound : Model -> ( Model, Cmd Msg )
generateRound model =
    ( model
    , Cmd.batch
        [ model.rounds
            + 1
            |> Random.int 1
            |> Random.generate StartRound
        , Task.perform StartTimer Time.now
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowHome ->
            ( { model | screen = Home }, Cmd.none )

        ShowAbout ->
            ( { model | screen = About }, Cmd.none )

        EnterGame ->
            ( { model | screen = Game, rounds = 0 }, Task.perform StartGame Time.now )

        ChoosePrime ->
            advanceGame model True

        ChooseNotPrime ->
            advanceGame model False

        HandleKeyboardEvent key ->
            case key of
                "d" ->
                    update ChooseNotPrime model

                "s" ->
                    update ChoosePrime model

                _ ->
                    ( { model | keyboardHint = True }, Cmd.none )

        StartRound int ->
            ( { model | currentInteger = int }, Cmd.none )

        StartTimer timerStart ->
            ( { model | timerStart = timerStart }, Cmd.none )

        UpdateTimer timerLatest ->
            if Time.posixToMillis timerLatest - Time.posixToMillis model.timerStart > (timeLimit model.rounds + timerFuzz) then
                finishGame model

            else
                ( { model | timerLatest = timerLatest }, Cmd.none )

        StartGame gameStart ->
            generateRound { model | gameStart = gameStart }

        FinishGame gameEnd ->
            ( { model | gameEnd = gameEnd }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.screen of
        Home ->
            div [ class "overlay" ]
                [ div
                    [ class "overlay-content home" ]
                    [ h1 [ class "title" ]
                        [ text "Prime Time" ]
                    , p [ class "button-set" ]
                        [ button [ class "button button-primary", onClick EnterGame ] [ text "Start Priming" ]
                        , button [ class "button button-secondary", onClick ShowAbout ] [ text "Huh?" ]
                        ]
                    ]
                ]

        About ->
            aboutView

        Game ->
            let
                percentElapsed =
                    model.timerLatest
                        |> timeDifference model.timerStart
                        |> (\x -> toFloat x / toFloat (timeLimit model.rounds))
                        |> (\x -> x * 100)
                        |> round
            in
            div [ class "game" ]
                [ div
                    [ class "count-down"
                    , style "height" (String.fromInt percentElapsed ++ "%")
                    ]
                    []
                , p [ class "subject" ] [ text (String.fromInt model.currentInteger) ]
                , p [ class "button-set" ]
                    [ button [ class "button button-primary", onClick ChoosePrime ] [ text "Prime" ]
                    , button [ class "button button-secondary", onClick ChooseNotPrime ] [ text "Not Prime" ]
                    ]
                , if model.keyboardHint then
                    p [ class "hint" ] [ text "You can also use your keyboard! S = Prime, D = Not Prime" ]

                  else
                    text ""
                ]

        GameOver ->
            let
                seconds =
                    timeInSeconds model.gameStart model.gameEnd

                secondsUnit =
                    if seconds == "1" then
                        "second"

                    else
                        "seconds"

                rounds =
                    String.fromInt model.rounds

                integerUnit =
                    if model.rounds == 1 then
                        "integer"

                    else
                        "integers"
            in
            div [ class "overlay" ]
                [ div
                    [ class "overlay-content home" ]
                    [ h1 [ class "score" ]
                        [ text ("You correctly classified " ++ rounds ++ " " ++ integerUnit)
                        , br [] []
                        , text ("in " ++ seconds ++ " " ++ secondsUnit ++ ".")
                        ]
                    , p [ class "button-set" ]
                        [ button [ class "button button-primary", onClick EnterGame ] [ text "Try again" ]
                        , button [ class "button button-secondary", onClick ShowHome ] [ text "Return home" ]
                        ]
                    ]
                ]


aboutView : Html Msg
aboutView =
    div [ class "info overlay" ]
        [ header [ onClick ShowHome ] [ text "← Back to home screen" ]
        , article []
            [ h1 [] [ text "Prime Time" ]
            , h2 [] [ text "Determining whether some number is prime is an interesting challenge. It seems simple enough: a prime number is a positive integer that is only divisible by itself and one. However, the old math dudes and now computers have trouble with it. Test your prime picking skills against a ticking clock." ]
            , p []
                [ text "Prime Time is written by "
                , a [ href "https://jew.ski/" ] [ text "Chris Andrejewski" ]
                , text ". The source code is "
                , a [ href "https://github.com/andrejewski/prime-time" ] [ text "open source" ]
                , text "."
                ]
            ]
        ]


main : Program Int Model Msg
main =
    Browser.element
        { init = init, subscriptions = subscriptions, update = update, view = view }
