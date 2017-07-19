module JsonRPC
    exposing
        ( Context
        , initCtx
        , mapCtx
        , withCtx
        , Command
        , unit
        , map
        , andThen
        , requestBody
        , noop
        , request
        , foldList
        , simpleParam
        )

import Result
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List


{-| RPC requests need state data; minimal state data need to contain
URL and request id.
-}
type alias Context m =
    { m | url : String, rpcId : Int }


initCtx : Context ctx -> Context ctx
initCtx ctx =
    { ctx | url = "/jsonrpc", rpcId = 0 }


type alias Conts r s a =
    (s -> r) -> (a -> r) -> r


unitC : a -> Conts r s a
unitC a _ =
    (|>) a


andThenC : (a -> Conts r s b) -> Conts r s a -> Conts r s b
andThenC next conta cnv resb =
    conta cnv (\a -> next a cnv resb)


mapC : (a -> b) -> Conts r s a -> Conts r s b
mapC fn conta cnv resb =
    conta cnv (resb << fn)


{-| The RPC command (query) in action.
-}
type alias Command ctx msg a =
    Context ctx -> Conts msg (Cmd msg) ( Result String a, Context ctx )


{-| The "OK" command (monadic return) - wrap a value into a dummy
command.
-}
unit : a -> Command ctx msg a
unit a ctx =
    unitC ( Ok a, ctx )


{-| Apply a function to the command result.
-}
map : (a -> b) -> Command ctx msg a -> Command ctx msg b
map fn cmd =
    mapC (\( res, ctx ) -> ( Result.map fn res, ctx )) << cmd


{-| Chain a command and a command-producing function.
-}
andThen : (a -> Command ctx msg b) -> Command ctx msg a -> Command ctx msg b
andThen next cmd =
    andThenStC (liftResult next) cmd


liftResult : (a -> Command ctx msg b) -> Result String a -> Command ctx msg b
liftResult next ra =
    case ra of
        Err err ->
            \ctx -> \_ -> \cnt -> cnt ( Err err, ctx )

        Ok a ->
            next a


andThenStC : (a -> m -> Conts r s ( b, m )) -> (m -> Conts r s ( a, m )) -> m -> Conts r s ( b, m )
andThenStC next stta m =
    stta m
        |> andThenC (uncurry next)


simpleParam : String -> String -> ( String, Encode.Value )
simpleParam param val =
    ( param, Encode.string val )


{-| Construct a JSON RPC request body from a name, the request id, and
a set of parameters and values.
-}
requestBody : String -> Int -> List ( String, Encode.Value ) -> Http.Body
requestBody method id params =
    let
        body =
            [ simpleParam "method" method
            , ( "params", Encode.object params )
            , ( "id", Encode.int id )
            , simpleParam "jsonrpc" "2.0"
            ]
    in
        Encode.object body |> Http.jsonBody


{-| Empty, do-nothing command.
-}
noop : Command ctx msg ()
noop =
    unit ()


{-| Given a function changing the context, construct the command that
does not produce any value but as a "side-effect" applies this
function.
-}
mapCtx : (Context ctx -> Context ctx) -> Command ctx msg ()
mapCtx mapf ctx ress =
    (|>) ( Ok (), mapf ctx )


{-| Construct a command from a method name, list of parameters, and a JSON decoder.
-}
request : String -> List ( String, Encode.Value ) -> Decode.Decoder a -> Command ctx msg a
request method params decoder ctx ress continue =
    let
        id =
            ctx.rpcId

        nctx =
            { ctx | rpcId = id + 1 }
    in
        Http.post ctx.url (requestBody method id params) (Decode.field "result" decoder)
            |> Http.send (\res -> continue ( Result.mapError toString res, nctx ))
            |> ress


{-| A dummy command that just produces the context (state) as its
value.
-}
withCtx : Command ctx msg (Context ctx)
withCtx ctx _ =
    (|>) ( Ok ctx, ctx )


{-| Apply the computation to all elements in the list, in a foldr-like
manner.
-}
foldList : (a -> Command ctx msg b) -> List a -> Command ctx msg (List b)
foldList fn alist =
    List.foldr (folded fn) (unit []) alist


folded : (a -> Command ctx msg b) -> a -> Command ctx msg (List b) -> Command ctx msg (List b)
folded fn a blistcmd =
    fn a
        |> andThen (\b -> map ((::) b) blistcmd)
