module JsonExample exposing (..)

{-| This module exemplifies usage of JsonRPC library. It assumes an
example server, similar to the one from
`https://pypi.python.org/pypi/json-rpc`, is running on port 4000, and
provides methods `add`, `subtract`, `multiply`, `divide`.
-}

import Expr exposing (Tree(..))
import Html exposing (div, text, label, input, button, ul, li, Html)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onInput, onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRPC
import JsonRPC.Infix exposing ((|>>=), (|>>), (-:-))
import Task


type alias Model =
    JsonRPC.Context { expression : String, response : String }


{-| We need to init fields "inherited" from JsonRPC.Context too.
-}
initModel : Model
initModel =
    { url = "http://localhost:4000/jsonrpc", rpcId = 0, expression = "1+2", response = "resp" }


view : Model -> Html Msg
view model =
    div []
        [ div [ class "expression" ]
            [ label [] [ text "expression:" ]
            , input [ onInput UpdateExpression, value model.expression ] []
            , button [ onClick Start ] [ text "compute" ]
            ]
        , div [ class "response" ]
            [ text model.response ]
        ]


type Msg
    = UpdateExpression String
    | Start
    | Compute (Cmd Msg)
    | Display ( Result String Int, Model )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateExpression expr ->
            ( { model | expression = expr }, Cmd.none )

        Start ->
            ( model, Task.succeed model |> Task.perform compute )

        Compute cmd ->
            -- just pass on the command
            ( model, cmd )

        Display ( res, m ) ->
            ( updateResponse m res, Cmd.none )


updateResponse : Model -> Result String c -> Model
updateResponse model res =
    let
        response =
            case res of
                Err err ->
                    "failure: " ++ err

                Ok i ->
                    "result: " ++ toString i
    in
        { model | response = response }


{-| Parse the expression and create the RPC command chain.
-}
compute : Model -> Msg
compute model =
    case Expr.parse model.expression of
        Ok tree ->
            runComputation tree model Compute Display

        Err err ->
            Display ( Err err, model )


runComputation : Tree -> JsonRPC.Command Model Msg Int
runComputation tree =
    case tree of
        Int i ->
            JsonRPC.return i

        Add t1 t2 ->
            runOperation "add" t1 t2

        Subtract t1 t2 ->
            runOperation "subtract" t1 t2

        Multiply t1 t2 ->
            runOperation "multiply" t1 t2

        Divide t1 t2 ->
            runOperation "divide" t1 t2


runOperation : String -> Tree -> Tree -> JsonRPC.Command Model Msg Int
runOperation oper t1 t2 =
    runComputation t1
        |>>=
            (\a ->
                runComputation t2
                    |>>=
                        (\b ->
                            JsonRPC.request oper
                                [ ( "a", Encode.int a )
                                , ( "b", Encode.int b )
                                ]
                                Decode.int
                        )
            )


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
