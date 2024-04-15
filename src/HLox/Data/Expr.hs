module HLox.Data.Expr (
    Expr(..), Literal(..), fromLitToken
) where

import HLox.Data.Token

data Expr
    = Binary Expr Token Expr
    | Logic Expr Token Expr
    | Unary Token Expr
    | Call Expr Token [Expr]
    | Grouping Expr
    | Literal Literal
    | Variable Token
    | Assign Token Expr
    deriving (Eq, Ord, Show)

data Literal
    = LitNum Double
    | LitStr String
    | LitTrue
    | LitFalse
    | LitNil
    deriving (Eq, Ord, Show)

fromLitToken :: LitToken -> Literal
fromLitToken (LT_Num n) = LitNum n
fromLitToken (LT_Str s) = LitStr s