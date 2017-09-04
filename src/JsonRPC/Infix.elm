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
(|>>=) : Command ctx msg a -> (a -> Command ctx msg b) -> Command ctx msg b
(|>>=) cmd next =
    andThen next cmd


{-| Infix operator for `X |> JsonRPC.andThen (\_ -> Y)`.
-}
(|>>) : Command ctx msg a -> Command ctx msg b -> Command ctx msg b
(|>>) cmd1 cmd2 =
    andThen (\_ -> cmd2) cmd1


infixl 0 |>>=


infixl 0 |>>


{-| Infix operator equivalent of `JsonRPC.simpleParam`.
-}
(-:-) : String -> String -> ( String, Encode.Value )
(-:-) =
    simpleParam
