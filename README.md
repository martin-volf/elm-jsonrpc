# elm-jsonrpc

`elm-jsonrpc` is a simple JSON RPC request building and, more importantly,
request chaining tool.  You will not need such tool if you do not plan to send
more than one or two RPC requests in a batch.  On the other hand, if you need
to send several requests one depending on the other, handling errors and
passing results may become troublesome.


## Installation

```bash
elm package install martin-volf/elm-jsonrpc
```


## Example

```elm
requestOp : String -> Int -> Int -> JsonRPC.Command Ctx Msg Int
requestOp op a b =
    JsonRPC.request
        op
        [ JsonRPC.simpleParam "a" (toString a), JsonRPC.simpleParam "b" (toString b) ]
        Decode.int


requestPlus : Int -> Int -> JsonRPC.Command Ctx Msg Int
requestPlus =
    requestOp "plus"


requestMinus : Int -> Int -> JsonRPC.Command Ctx Msg Int
requestMinus =
    requestOp "minus"


requestTimes : Int -> Int -> JsonRPC.Command Ctx Msg Int
requestTimes =
    requestOp "times"


requestDiv : Int -> Int -> JsonRPC.Command Ctx Msg Int
requestDiv =
    requestOp "div"


{-| Let the server compute (1 + 2) * (3 - 4)
-}
compoundRequest : JsonRPC.Command Ctx Msg Int
compoundRequest =
    requestPlus 1 2
        |> JsonRPC.andThen
            (\sum ->
                requestMinus 3 4
                    |> JsonRPC.andThen (requestTimes sum)
            )
```
