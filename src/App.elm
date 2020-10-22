module App exposing (Model, Msg(..), init, main, subscriptions, update, view)

import AppState
import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import AppState exposing (State(..))
import Task
import Time


type Msg
    = StartGame
    | Roll
    | Next
    | CardSelected AppState.Card
    | NewSequenceItem Int
    | Time
    | Tick Time.Posix
    | RequestInstructions


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 1000 Tick ]


type alias Model =
    { appState : AppState.GameState }


init : () -> ( Model, Cmd Msg )
init flags =
    ( Model AppState.landing
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )

        ( nextAppState, cmd ) =
            {--for debugging, replace `msg` with `Debug.log "update" msg` --}
            case ( model.appState, msg ) of
                ( AppState.Landing (State m), Roll ) ->
                    case AppState.initRolls of
                        r :: rolls ->
                            case r > 0 of
                                True ->
                                    let
                                        newRolls =
                                            (r - 1) :: rolls
                                    in
                                    ( { model | appState = AppState.toDeckShow (AppState.Clock 61000 0) 0 AppState.Alpha [] newRolls (State m) }
                                    , Random.generate NewSequenceItem (Random.int 0 1)
                                    )

                                False ->
                                    ( model, Cmd.none )

                        [] ->
                            ( model, Cmd.none )

                ( AppState.Landing (State m), RequestInstructions ) ->
                    ( { model | appState = AppState.toInstructions (State m) }
                    , Cmd.none
                    )

                ( AppState.DeckShow (State m), Roll ) ->
                    case m.rolls of
                        r :: rolls ->
                            case r > 0 of
                                True ->
                                    let
                                        newRolls =
                                            (r - 1) :: rolls
                                    in
                                    ( { model | appState = AppState.toDeckShow m.clock m.floor m.cardVariant m.sequence newRolls (State m) }
                                    , Random.generate NewSequenceItem (Random.int 0 1)
                                    )

                                False ->
                                    let
                                        newSequence =
                                            List.reverse m.sequence
                                    in
                                    ( { model | appState = AppState.toBoard (AppState.Clock 61000 m.clock.cumulative) m.floor m.cardVariant newSequence rolls AppState.neutralCards (State m) }
                                    , Cmd.none
                                    )

                        [] ->
                            ( model, Cmd.none )

                ( AppState.DeckShow (State m), NewSequenceItem num ) ->
                    let
                        binary =
                            AppState.intToBinary num

                        newSequence =
                            binary :: m.sequence

                        newCardVariant =
                            AppState.toggleCardVariant m.cardVariant
                    in
                    ( { model | appState = AppState.toDeckShow m.clock m.floor newCardVariant newSequence m.rolls (State m) }
                    , Cmd.none
                    )

                ( AppState.Board (State m), Tick newTime ) ->
                    case m.clock.remaining <= 1000 of
                        True ->
                            ( { model | appState = AppState.toOver m.clock m.floor (State m) }, Cmd.none )

                        False ->
                            let
                                newClock =
                                    AppState.Clock (m.clock.remaining - 1000) m.clock.cumulative
                            in
                            ( { model | appState = AppState.toBoard newClock m.floor m.cardVariant m.sequence m.rolls m.cards (State m) }
                            , Cmd.none
                            )

                ( AppState.Board (State m), CardSelected card ) ->
                    case m.sequence of
                        [ c ] ->
                            case AppState.cardBinary card == c of
                                True ->
                                    case m.rolls of
                                        r :: rolls ->
                                            case r > 0 of
                                                True ->
                                                    let
                                                        newRolls =
                                                            (r - 1) :: rolls

                                                        newFloor =
                                                            m.floor + 1

                                                        newCumulative =
                                                            m.clock.cumulative + (60000 - m.clock.remaining)

                                                        newClock =
                                                            AppState.Clock m.clock.remaining newCumulative
                                                    in
                                                    ( { model | appState = AppState.toDeckShow newClock newFloor m.cardVariant [] newRolls (State m) }
                                                    , Random.generate NewSequenceItem (Random.int 0 1)
                                                    )

                                                False ->
                                                    ( model, Cmd.none )

                                        [] ->
                                            ( { model | appState = AppState.toCompleted m.floor m.clock (State m) }, Cmd.none )

                                False ->
                                    ( { model | appState = AppState.toOver m.clock m.floor (State m) }, Cmd.none )

                        s :: sequence ->
                            case AppState.cardBinary card == s of
                                True ->
                                    let
                                        newCards =
                                            List.map
                                                (\crd ->
                                                    if AppState.cardBinary crd == AppState.cardBinary card then
                                                        AppState.Correct (AppState.cardBinary crd)

                                                    else
                                                        AppState.Neutral (AppState.cardBinary crd)
                                                )
                                                m.cards

                                        newCardVariant =
                                            AppState.toggleCardVariant m.cardVariant
                                    in
                                    ( { model | appState = AppState.toBoard m.clock m.floor newCardVariant sequence m.rolls newCards (State m) }
                                    , Cmd.none
                                    )

                                False ->
                                    ( { model | appState = AppState.toOver m.clock m.floor (State m) }, Cmd.none )

                        [] ->
                            ( model, Cmd.none )

                ( AppState.Over (State m), Next ) ->
                    case AppState.initRolls of
                        r :: rolls ->
                            case r > 0 of
                                True ->
                                    let
                                        newRolls =
                                            (r - 1) :: rolls
                                    in
                                    ( { model | appState = AppState.toDeckShow (AppState.Clock 61000 0) 0 AppState.Alpha [] newRolls (State m) }
                                    , Random.generate NewSequenceItem (Random.int 0 1)
                                    )

                                False ->
                                    ( model, Cmd.none )

                        [] ->
                            ( model, Cmd.none )

                ( AppState.Completed (State m), Next ) ->
                    ( Model AppState.landing, Cmd.none )

                ( AppState.Instructions (State m), Next ) ->
                    ( Model AppState.landing, Cmd.none )

                ( _, _ ) ->
                    noop
    in
    ( nextAppState, cmd )


view : Model -> Html Msg
view model =
    case model.appState of
        AppState.Landing (State m) ->
            div []
                [ div [ class "top", onClick RequestInstructions ]
                    [ div [ class "question-mark-display" ]
                        [ text "?" ]
                    ]
                , div [ class "single-panel", onClick Roll ]
                    [ div [ class "title" ] [ text "mem" ]
                    , div [ class "start" ] [ text "START GAME" ]
                    ]
                ]

        AppState.DeckShow (State m) ->
            case m.sequence of
                s :: sequence ->
                    let
                        binaryInt =
                            AppState.binaryToInt s

                        binaryWord =
                            AppState.binaryToWord s

                        variantString =
                            AppState.variantToString m.cardVariant
                    in
                    div [ class "single-panel", onClick Roll ]
                        [ div [ class ("sequence-card-" ++ binaryWord ++ "-" ++ variantString) ] [ text (String.fromInt binaryInt) ] ]

                [] ->
                    div [] [ text "empty sequences!" ]

        AppState.Board (State m) ->
            let
                variant =
                    case m.cardVariant of
                        AppState.Alpha ->
                            "alpha"

                        AppState.Beta ->
                            "beta"

                drawItem card =
                    case card of
                        AppState.Neutral s ->
                            div [ class "board-panel", onClick (CardSelected card) ] [ div [ class ("item-neutral-" ++ variant) ] [ text (String.fromInt (AppState.cardInt card)) ] ]

                        AppState.Incorrect s ->
                            div [ class "board-panel", onClick (CardSelected card) ] [ div [ class ("item-incorrect-" ++ variant) ] [ text (String.fromInt (AppState.cardInt card)) ] ]

                        AppState.Correct s ->
                            div [ class "board-panel", onClick (CardSelected card) ] [ div [ class ("item-correct-" ++ variant) ] [ text (String.fromInt (AppState.cardInt card)) ] ]

                floorDisplay =
                    div [ class "floor-display" ] [ text (String.fromInt m.floor ++ "F") ]

                secondsLeft =
                    m.clock.remaining // 1000

                secondsLeftDisplay =
                    div [ class "seconds-left-display" ] [ text (String.fromInt secondsLeft) ]
            in
            case secondsLeft of
                61 ->
                    div [] []

                _ ->
                    div [] [ div [ class "top" ] [ floorDisplay, secondsLeftDisplay ], div [ class "container" ] (List.map drawItem m.cards) ]

        AppState.Over (State m) ->
            case m.floor of
                0 ->
                    div [ class "over-container", onClick Next ] [ div [ class "try-again-display" ] [ text "TRY AGAIN?" ] ]

                _ ->
                    let
                        floorString =
                            String.fromInt m.floor

                        seconds =
                            toFloat m.clock.cumulative / 1000

                        secondsString =
                            String.fromInt (round seconds)
                    in
                    div [ class "over-container", onClick Next ] [ div [ class "over-floor-display" ] [ text ("[" ++ floorString ++ "F" ++ "," ++ secondsString ++ "S" ++ "]") ], div [ class "try-again-display" ] [ text "TRY AGAIN?" ] ]

        AppState.Completed (State m) ->
            let
                floorString =
                    String.fromInt m.floor

                seconds =
                    toFloat m.clock.cumulative / 1000

                secondsString =
                    String.fromInt (round seconds)
            in
            div [ class "over-container", onClick Next ] [ div [ class "over-floor-display" ] [ text ("[" ++ floorString ++ "F" ++ "," ++ secondsString ++ "S" ++ "]") ], div [ class "try-again-display" ] [ text "GAME COMPLETED!" ] ]

        AppState.Instructions (State m) ->
            div [ class "instructions-container", onClick Next ]
                [ div [ class "instructions" ]
                    [ text "YOU WILL BE PRESENTED WITH A SEQUENCE OF NUMBERS, EITHER 0 OR 1. TAP THE SCREEN TO PROGRESS TO THE NEXT NUMBER IN THE SEQUENCE."
                    ]
                , div [ class "instructions" ]
                    [ text "WHEN THE SEQUENCE HAS BEEN EXHAUSTED, YOU WILL BE TAKEN TO A BOARD THAT CONTAINS THE NUMBERS 0 AND 1."
                    ]
                , div [ class "instructions" ]
                    [ text "TRY AND RECREATE THE SEQUENCE YOU WERE PRESENTED WITH, WITHIN THE DISPLAYED TIME LEFT."
                    ]
                ]
