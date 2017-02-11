module Main
    exposing
        ( main
        )

import Eval
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Parser
import String


-- MODEL


type alias Command =
    { input : String
    , output : String
    }


type alias Model =
    { input : String
    , commandHistory : List Command
    }


initialModel : Model
initialModel =
    { input = ""
    , commandHistory = []
    }



-- MESSAGE


type Msg
    = Eval String
    | SetInput String



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style
            [ ( "width", "100%" )
            , ( "height", "100%" )
            , ( "display", "flex" )
            , ( "justify-content", "center" )
            , ( "align-items", "center" )
            , ( "flex-direction", "column" )
            ]
        ]
        [ Html.form
            [ Html.Events.onInput SetInput
            , Html.Attributes.style
                [ ( "margin-top", "100px" )
                ]
            ]
            [ Html.textarea
                [ Html.Attributes.value model.input
                , Html.Attributes.autofocus True
                , Html.Attributes.cols 40
                , model.input
                    |> String.lines
                    |> List.length
                    |> Html.Attributes.rows
                , Html.Attributes.style
                    [ ( "height", "auto" )
                    , ( "width", "auto" )
                    , ( "overflow", "auto" )
                    , ( "font-size", "18px" )
                    , ( "resize", "none" )
                    ]
                ]
                []
            , Html.button
                [ Html.Attributes.type_ "button"
                , Html.Events.onClick (Eval model.input)
                , Html.Attributes.disabled (model.input == "")
                , Html.Attributes.style
                    [ ( "display", "block" )
                    , ( "margin", "0 auto" )
                    , ( "width", "100%" )
                    ]
                ]
                [ Html.text "Evaluate!"
                ]
            ]
        , Html.ul
            [ Html.Attributes.style
                [ ( "height", "100%" )
                , ( "overflow-y", "scroll" )
                ]
            ]
          <|
            List.map viewCommand model.commandHistory
        ]


viewCommand : Command -> Html Msg
viewCommand command =
    Html.div
        []
        [ Html.code
            [ Html.Attributes.style
                [ ( "display", "block" )
                ]
            ]
            [ Html.text command.input
            ]
        , Html.code
            [ Html.Attributes.style
                [ ( "display", "block" )
                ]
            ]
            [ Html.text <| "-->  " ++ command.output
            ]
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Eval string ->
            case Parser.parse string of
                Ok res ->
                    let
                        results =
                            List.foldl (\a b -> (Eval.runEval a) ++ b) "" res
                    in
                        ( { model
                            | input = ""
                            , commandHistory =
                                { input = string, output = results } :: model.commandHistory
                          }
                        , Cmd.none
                        )

                Err failure ->
                    ( { model | commandHistory = { input = string, output = failure } :: model.commandHistory }, Cmd.none )

        SetInput string ->
            ( { model | input = string }, Cmd.none )



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
