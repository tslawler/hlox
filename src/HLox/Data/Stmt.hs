module HLox.Data.Stmt (
    Stmt(..)
) where

import HLox.Data.Expr
import HLox.Data.Token

data Stmt
    = Print Expr -- ^ print expr;
    | Expr Expr -- ^ expr;
    | Var Token (Maybe Expr) -- ^ var token [= expr];
    | Fun Token [Token] [Stmt] -- ^ fun name(arg1,...,argN) { body }
    | Return Token Expr -- ^ return expr; -- The token is the 'return' keyword itself.
    | Block [Stmt] -- ^ { stmt... }
    | If Expr Stmt (Maybe Stmt) -- ^ if (expr) thenStmt [else elseStmt]
    | While Expr Stmt -- ^ while (expr) body
    deriving (Eq, Ord, Show)

instance Semigroup Stmt where
    (Block xs) <> (Block ys) = Block (xs <> ys)
    x <> (Block ys) = Block (x:ys)
    (Block xs) <> y = Block (xs ++ [y])
    x <> y = Block [x,y]