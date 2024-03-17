module HLox.Control.Pretty (
    prettify, print
) where

import Prelude hiding (print)
import Data.List (intercalate)

import HLox.Data.Expr
import HLox.Data.Token (_lexeme)

prettify :: Expr -> String
prettify (Literal LitTrue) = "true"
prettify (Literal LitFalse) = "false"
prettify (Literal LitNil) = "nil"
prettify (Literal (LitStr s)) = show s
prettify (Literal (LitNum n)) = show n
prettify (Grouping body) = parens ["group", prettify body]
prettify (Unary tok body) = parens [_lexeme tok, prettify body]
prettify (Binary lhs tok rhs) = parens [_lexeme tok, prettify lhs, prettify rhs]

print :: Expr -> IO ()
print expr = putStrLn (prettify expr)

parens :: [String] -> String
parens strs = "(" ++ intercalate " " strs ++ ")"