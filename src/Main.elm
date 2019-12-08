module Main exposing (..)

-- elm install elm/browser

import Browser
import Browser.Dom as Dom
import Html exposing (Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import String exposing (append, replace)
import Task
import Update.Extra exposing (andThen)



--Main


type alias Model =
    { counter : Int
    , showInput : Bool
    , inputCounter : String
    }


type alias Flags =
    { noop : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init =
    always ( { counter = 0, showInput = False, inputCounter = "0" }, Cmd.none )


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Noop
    | Increment
    | Decrement
    | Reset
    | ShowInput
    | ConfirmNumber
    | OnInputChange String
    | FocusOnInput



--Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Increment ->
            ( { model | counter = model.counter + 1, inputCounter = String.fromInt (model.counter + 1) }, Cmd.none )

        Decrement ->
            if model.counter <= 0 then
                ( { model | counter = 0, inputCounter = "0" }, Cmd.none )

            else
                ( { model | counter = model.counter - 1, inputCounter = String.fromInt (model.counter - 1) }, Cmd.none )

        Reset ->
            ( { model
                | counter = 0
                , inputCounter = "0"
              }
            , Cmd.none
            )

        ShowInput ->
            ( { model
                | showInput =
                    if model.showInput == False then
                        True

                    else
                        False
                , inputCounter = String.fromInt model.counter
              }
            , Cmd.none
            )
                |> andThen update FocusOnInput

        ConfirmNumber ->
            ( { model
                | showInput =
                    if model.showInput == False then
                        True

                    else
                        False
                , counter =
                    let
                        val =
                            replace "+" "" model.inputCounter |> toInt |> mayBeIntToInt
                    in
                    if val >= 0 then
                        val

                    else
                        0
              }
            , Cmd.none
            )

        OnInputChange content ->
            ( { model
                | inputCounter = content
                , counter =
                    let
                        val =
                            content |> toInt |> mayBeIntToInt
                    in
                    if val >= 0 then
                        val

                    else
                        0
              }
            , Cmd.none
            )

        FocusOnInput ->
            ( model, Task.attempt (always Noop) (Dom.focus "counter-input") )



--Helper Functions


toInt string =
    String.toInt string


mayBeIntToInt maybeInt =
    case maybeInt of
        Just value ->
            value

        Nothing ->
            0



--View


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ div [ class "flex justify-content-center align-items-center" ]
            [ button
                [ onClick Decrement
                , class
                    (append "primary btn font title "
                        (if model.counter <= 0 then
                            " disable"

                         else
                            ""
                        )
                    )
                ]
                [ text "-" ]
            , if model.showInput == True then
                div [ class "py-md" ]
                    [ input [ placeholder "some text", id "counter-input", value model.inputCounter, onInput OnInputChange ] []
                    ]

              else
                div
                    [ class "text-center font text-secondary counter display-inline py-md pointer "
                    , onDoubleClick ShowInput
                    ]
                    [ text (String.fromInt model.counter) ]
            , button
                [ class
                    (append "btn font text-secondary "
                        (if model.showInput then
                            "show"

                         else
                            "hide"
                        )
                    )
                , onClick ConfirmNumber
                ]
                [ text "Confirm" ]
            , button
                [ onClick Increment
                , class
                    (append "primary btn font title"
                        (if model.showInput then
                            ""

                         else
                            ""
                        )
                    )
                ]
                [ text "+" ]
            , span [] [ text " " ]
            , button [ onClick Reset, class "btn primary font title" ] [ text "Reset" ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
