module JsonRPC
    exposing
        ( Context
        , mapCtx
        , readCtx
        , updateState
        , readState
        , Command
        , return
        , fail
        , map
        , map2
        , mapError
        , andThen
        , requestBody
        , noop
        , request
        , run
        , finalState
        , foldList
        , simpleParam
        )

{-| This library helps with building and chaining stateful JSON RPC
requests. While running a single RPC request (and handling all
potential outcomes) or simple chaining stateless requests is
straightforward using built-in means of Elm, it becomes more difficult
when several requests need to be run with one depending - both in
terms of the result and side-effects - on the previous.


# RPC context

Every RPC request (or RPC command, see below) needs an instance of
`Context`. In most cases you don't need to access the instance
itself, but you may access the state that the context instance wraps.
The state can be anything your application needs, from a single
integer to the full application model.

@docs Context


# The `Command` type

Every request is of the following type:

@docs Command

The three type variables are:

  - `state` - request state, anything that you need to pass between
    commands, see above
  - `err` - error type (typically `Http.Error` or something wrapping
    it)
  - `a` - the type that the request eventually evaluates to.

With `request` you can build individual commands, but other tools are
needed to chain commands (`andThen`) and to build special commands.

Finally, when your chain of commands is ready, launch it with `run`.

@docs request, return, fail, map, map2, andThen, noop, run


## Utility functions

@docs mapCtx, readCtx, updateState, readState, finalState, mapError

@docs foldList

@docs requestBody, simpleParam

-}

import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List
import Result
import Task exposing (Task)
import Tuple


{-| Minimal RPC state data need to contain URL and request id.
-}
type alias Context state =
    { url : String, rpcId : Int, state : state }


{-| The RPC command (request) in action.
-}
type alias Command state err a =
    Context state -> Task err ( Context state, a )


{-| The "OK" command (monadic return) - wrap a value into a dummy
command.
-}
return : a -> Command state err a
return a ctx =
    Task.succeed ( ctx, a )


{-| Apply a function to the command result.
-}
map : (a -> b) -> Command state err a -> Command state err b
map fn cmd =
    cmd >> Task.map (Tuple.mapSecond fn)


{-| Transform results of two successive commands.
-}
map2 : (a -> b -> c) -> Command state err a -> Command state err b -> Command state err c
map2 fn cmda cmdb =
    cmda
        |> andThen (\a -> (map (fn a) cmdb))


{-| Transform the error value. This can be useful when you need to
change the error type; for instance, you need to process two types of errors

    type AppError
        = HttpErr Http.Error
        | OtherErr String

But since `request` yields a command with `Http.Error`, you need to
transform the command like

    request ...
        |> mapError HttpErr

-}
mapError : (err1 -> err2) -> Command state err1 a -> Command state err2 a
mapError fn cmd =
    Task.mapError fn << cmd


{-| Chain a command and a command-producing function.
-}
andThen : (a -> Command state err b) -> Command state err a -> Command state err b
andThen next cmd =
    cmd >> Task.andThen (uncurry (flip next))


{-| Utility function to make a request parameter pair from two
strings.
-}
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
noop : Command state err ()
noop =
    return ()


{-| Always failing command.
-}
fail : err -> Command state err a
fail err _ =
    Task.fail err


{-| Given a function changing the context, construct the command that
does not produce any value but as a "side-effect" applies this
function.
-}
mapCtx : (Context state -> Context state) -> Command state err ()
mapCtx fn ctx =
    Task.succeed ( fn ctx, () )


{-| Given a function changing the context, construct the command that
does not produce any value but as a "side-effect" applies this
function.
-}
updateState : (state -> state) -> Command state err ()
updateState fn =
    mapCtx (\ctx -> { ctx | state = fn ctx.state })


{-| Construct a command from a method name, list of parameters, and a JSON decoder.
-}
request : String -> List ( String, Encode.Value ) -> Decode.Decoder a -> Command state Http.Error a
request method params decoder ctx =
    let
        id =
            ctx.rpcId

        nctx =
            { ctx | rpcId = id + 1 }
    in
        Http.post ctx.url (requestBody method id params) (Decode.field "result" decoder)
            |> Http.toTask
            |> Task.map (\a -> ( nctx, a ))


{-| Run the chain of commands. You need to provide the initial
state as well as the URL to be used for requests. The final state is
lost; if you are interested in it, either use `readState`, or
`finalState` if you need both the state as well as the final value.
-}
run : String -> state -> (Result error a -> msg) -> Command state error a -> Cmd msg
run url initState display command =
    let
        initCtx =
            { rpcId = 0, url = url, state = initState }
    in
        command initCtx
            |> Task.attempt (Result.map Tuple.second >> display)


{-| A dummy command that just produces the context as its value.
-}
readCtx : Command state err (Context state)
readCtx ctx =
    Task.succeed ( ctx, ctx )


{-| A dummy command that just produces the state as its value.
-}
readState : Command state err state
readState =
    readCtx >> Task.map (Tuple.mapSecond .state)


{-| Similar to `readState`, but produce both the last value as well as
state. Might be useful as the last in the chain of commands.
-}
finalState : a -> Command state err ( state, a )
finalState value =
    readState |> andThen (\state -> return ( state, value ))


{-| Apply the computation to all elements in the list, in a foldr-like
manner.
-}
foldList : (a -> Command state err b) -> List a -> Command state err (List b)
foldList fn alist =
    List.foldr (folded fn) (return []) alist


folded : (a -> Command state err b) -> a -> Command state err (List b) -> Command state err (List b)
folded fn a blistcmd =
    fn a
        |> andThen (\b -> map ((::) b) blistcmd)
