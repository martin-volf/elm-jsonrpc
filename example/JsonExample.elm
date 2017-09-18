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
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import JsonRPC
import JsonRPC.Infix exposing ((|>>=), (|>>), (-:-))


type alias Model =
    { expression : String, response : String }


initModel : Model
initModel =
    { expression = "1+2", response = "resp" }


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


type Error
    = HttpErr Http.Error
    | ParseErr String


type alias State =
    ()


type Msg
    = UpdateExpression String
    | Start
    | Display (Result Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateExpression expr ->
            ( { model | expression = expr }, Cmd.none )

        Start ->
            ( model, compute model.expression )

        Display res ->
            ( updateResponse model res, Cmd.none )


updateResponse : Model -> Result Error Int -> Model
updateResponse model res =
    case res of
        Err (HttpErr err) ->
            { model | response = "RPC failure: " ++ toString err }

        Err (ParseErr err) ->
            { model | response = "Parsing failed: " ++ err }

        Ok i ->
            { model | response = "result: " ++ toString i }


{-| Parse the expression and create the RPC command chain.
-}
compute : String -> Cmd Msg
compute expr =
    (case Expr.parse expr of
        Ok tree ->
            runComputation tree

        Err err ->
            JsonRPC.fail (ParseErr err)
    )
        |> JsonRPC.run "http://localhost:4000/jsonrpc" () Display


runComputation : Tree -> JsonRPC.Command State Error Int
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


runOperation : String -> Tree -> Tree -> JsonRPC.Command State Error Int
runOperation oper t1 t2 =
    runComputation t1
        |>>= ((|>>=) (runComputation t2) << request oper)


request : String -> Int -> Int -> JsonRPC.Command State Error Int
request oper a b =
    JsonRPC.request oper
        [ ( "a", Encode.int a )
        , ( "b", Encode.int b )
        ]
        Decode.int
        |> JsonRPC.mapError HttpErr


main : Program Never Model Msg
main =
    Html.program
        { init = ( initModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
