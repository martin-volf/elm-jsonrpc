module JsonRPC.Infix
    exposing
        ( (|>>=)
        , (|>>)
        , (-:-)
        )

{-| Few infix operators to simplify chaining JsonRPC commands.

@docs (|>>=), (|>>), (-:-)
-}

import JsonRPC exposing (Command, andThen, simpleParam)
import Json.Encode as Encode


{-| Infix operator variant for `X |> JsonRPC.andThen FY`.
-}
(|>>=) : Command state msg err a -> (a -> Command state msg err b) -> Command state msg err b
(|>>=) cmd next =
    andThen next cmd


{-| Infix operator for `X |> JsonRPC.andThen (\_ -> Y)`.
-}
(|>>) : Command state msg err a -> Command state msg err b -> Command state msg err b
(|>>) cmd1 cmd2 =
    andThen (\_ -> cmd2) cmd1


infixl 0 |>>=


infixl 0 |>>


{-| Infix operator equivalent of `JsonRPC.simpleParam`.
-}
(-:-) : String -> String -> ( String, Encode.Value )
(-:-) =
    simpleParam
