module HLox.Control.Pretty (
    prettify, print
) where

import Prelude hiding (print)

import HLox.Data.Expr
import HLox.Data.Token (_lexeme)

prettify :: Expr -> String
prettify (Literal LitTrue) = "true"
prettify (Literal LitFalse) = "false"
prettify (Literal LitNil) = "nil"
prettify (Literal (LitStr s)) = show s
prettify (Literal (LitNum d)) =
    let (n,k) = properFraction d
    in if k == 0.0 then show (n :: Integer) else show d
prettify (Grouping body) = parens ["group", prettify body]
prettify (Unary tok body) = parens [_lexeme tok, prettify body]
prettify (Variable tok) = _lexeme tok
prettify (Binary lhs tok rhs) = parens [_lexeme tok, prettify lhs, prettify rhs]
prettify (Assign tok expr) = parens ["assign", _lexeme tok, prettify expr]

print :: Expr -> IO ()
print expr = putStrLn (prettify expr)

parens :: [String] -> String
parens strs = "(" ++ unwords strs ++ ")"