module HLox.Control.Parser (
    parseExpr
) where

import Control.Monad
import Control.Monad.State

import HLox (HLox(), reportError')
import HLox.Data.Token
import HLox.Data.Expr

type Parser = StateT [Token] HLox

parseExpr :: [Token] -> HLox Expr
parseExpr = runParser expr

runParser :: Parser a -> [Token] -> HLox a
runParser = evalStateT

-- TODO: Panic properly.
panic :: Parser a
panic = lift.lift $ ioError (userError "Panic!")

parseError :: Token -> String -> Parser a
parseError tok msg = 
    let loc = case _type tok of
            EOF -> " at end"
            _ -> " at '" ++ _lexeme tok ++ "' (column " ++ show (_col tok) ++ ")"
    in do
        lift $ reportError' (_line tok) loc msg
        panic

peekToken :: Parser Token
peekToken = gets head

-- | If the next token is one of the given types, consume and return it.
-- | Otherwise, do nothing and return Nothing.
match :: [TokenType] -> Parser (Maybe Token)
match typs = do
    tok <- peekToken
    if (_type tok `elem` typs) then (advance >> return (Just tok)) else return Nothing

advance :: Parser ()
advance = modify tail

takeToken :: Parser Token
takeToken = do
    tok <- peekToken
    advance
    return tok

consume :: TokenType -> String -> Parser ()
consume typ msg = do
    tok <- takeToken
    unless (_type tok == typ) $ parseError tok msg

-- | Parses a sequence of `next` separated by `matchingTypes`
-- | 
binaryStream :: [TokenType] -> Parser Expr -> Parser Expr
binaryStream matchingTypes next = go
    where
    go = do
        lhs <- next
        mbTok <- match matchingTypes
        case mbTok of
            Nothing -> return lhs
            (Just tok) -> Binary lhs tok <$> go

expr :: Parser Expr
expr = equality

equality :: Parser Expr
equality = binaryStream [Operator O_EqualEqual, Operator O_BangEqual] comparison
comparison :: Parser Expr
comparison = binaryStream [Operator O_Less, Operator O_LessEqual, Operator O_Greater, Operator O_GreaterEqual] term
term :: Parser Expr
term = binaryStream [Operator O_Plus, Operator O_Minus] factor
factor :: Parser Expr
factor = binaryStream [Operator O_Slash, Operator O_Star] unary

unary :: Parser Expr
unary = do
    mbTok <- match [Operator O_Bang, Operator O_Minus]
    case mbTok of
        Nothing -> primary
        (Just tok) -> Unary tok <$> unary

primary :: Parser Expr
primary = do
    tok <- takeToken
    case _type tok of
        (LitToken lt) -> return.Literal $ fromLitToken lt
        (Reserved R_True) -> return.Literal $ LitTrue
        (Reserved R_False) -> return.Literal $ LitFalse
        (Reserved R_Nil) -> return.Literal $ LitNil
        (Open Paren) -> do
            e <- expr
            consume (Close Paren) "Expected ')' after expression."
            return $ Grouping e
        _ -> parseError tok "Expected an expression."
