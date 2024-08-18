module HLox.Data.Expr (
    Expr(..), Literal(..), fromLitToken
) where

import HLox.Data.Literal

data Expr
    = Binary Expr Token Expr
    | Logic Expr Token Expr
    | Unary Token Expr
    | Call Expr Token [Expr]
    | Get Expr Token
    | Set Expr Token Expr
    | Grouping Expr
    | Literal Literal
    | Variable Token
    | This Token
    | Super Token Token
    | Assign Token Expr
    deriving (Eq, Ord, Show)
