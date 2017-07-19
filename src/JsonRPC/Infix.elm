module JsonRPC.Infix
    exposing
        ( (|>>=)
        , (|>>)
        , (-:-)
        )

{-| Few infix operators to simplify creating and chaining JsonRPC commands.
-}

import JsonRPC exposing (Command, andThen, simpleParam)
import Json.Encode as Encode


(|>>=) : Command ctx msg a -> (a -> Command ctx msg b) -> Command ctx msg b
(|>>=) cmd next =
    andThen next cmd


(|>>) : Command ctx msg a -> Command ctx msg b -> Command ctx msg b
(|>>) cmd1 cmd2 =
    andThen (\_ -> cmd2) cmd1


infixl 0 |>>=


infixl 0 |>>


(-:-) : String -> String -> ( String, Encode.Value )
(-:-) =
    simpleParam
