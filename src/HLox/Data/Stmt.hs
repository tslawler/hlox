module HLox.Data.Stmt (
    Stmt(..)
) where

import HLox.Data.Expr
import HLox.Data.Token

data Stmt
    = Print Expr
    | Expr Expr
    deriving (Eq, Ord, Show)