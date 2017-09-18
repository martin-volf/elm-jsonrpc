module Expr exposing (Tree(..), parse)

{- Simple usage of parser-combinators to construct an AST of an
   arithmetic expression.
-}

import Combine exposing (..)
import Combine.Num exposing (int)


type Tree
    = Add Tree Tree
    | Subtract Tree Tree
    | Multiply Tree Tree
    | Divide Tree Tree
    | Int Int


addop : Parser s (Tree -> Tree -> Tree)
addop =
    choice
        [ Add <$ string "+"
        , Subtract <$ string "-"
        ]


mulop : Parser s (Tree -> Tree -> Tree)
mulop =
    choice
        [ Multiply <$ string "*"
        , Divide <$ string "/"
        ]


expr : Parser s Tree
expr =
    let
        go () =
            chainl addop term
    in
        lazy go


term : Parser s Tree
term =
    let
        go () =
            chainl mulop factor
    in
        lazy go


factor : Parser s Tree
factor =
    whitespace *> (parens expr <|> (map Int int)) <* whitespace


{-| Compute the result of an expression.
-}
parse : String -> Result String Tree
parse s =
    case Combine.parse (expr <* end) s of
        Ok ( _, _, tree ) ->
            Ok tree

        Err ( _, _, ms ) ->
            Err (String.join " / " ms)
