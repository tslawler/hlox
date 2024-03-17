module HLox.Data.Expr (
    Expr(..), Literal(..)
) where

import HLox.Data.Token

data Expr
    = Binary Expr Token Expr
    | Unary Token Expr
    | Grouping Expr
    | Literal Literal
    deriving (Eq, Ord, Show)

data Literal
    = LitNum Double
    | LitStr String
    | LitTrue
    | LitFalse
    | LitNil
    deriving (Eq, Ord, Show)