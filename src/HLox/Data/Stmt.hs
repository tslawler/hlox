module HLox.Data.Stmt (
    Stmt(..)
) where

import HLox.Data.Expr
import HLox.Data.Token

data Stmt
    = Print Expr -- ^ print expr;
    | Expr Expr -- ^ expr;
    | Var Token (Maybe Expr) -- ^ var token [= expr];
    | Block [Stmt] -- ^ { stmt... }
    | If Expr Stmt (Maybe Stmt) -- ^ if (expr) thenStmt [else elseStmt]
    deriving (Eq, Ord, Show)