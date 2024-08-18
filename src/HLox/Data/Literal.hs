module HLox.Data.Literal (Literal(..)) where

data Literal
    = LitNum Double
    | LitStr String
    | LitTrue
    | LitFalse
    | LitNil
    deriving (Eq, Ord, Show)
