module HLox.Control.Parser (
    parseExpr
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import HLox.Control.Error
import HLox.Data.Token
import HLox.Data.Expr
import Data.Either (fromRight)

data ParseError = ParseError Token deriving (Eq)

type Parser = ExceptT ParseError (StateT [Token] HLox)

parseExpr :: [Token] -> HLox Expr
parseExpr tokens = fromRight (Literal LitNil) <$> runParser expr tokens


runParser :: Parser a -> [Token] -> HLox (Either ParseError a)
runParser = evalStateT . runExceptT

-- | List of keywords which begin a new statement.
beginsStatement :: [Reserved]
beginsStatement = [R_Class, R_For, R_Fun, R_If, R_Print, R_Return, R_While, R_Var]

-- | Consume tokens until the start of a new statement or EOF.
synchronize :: Parser ()
synchronize = do
    tok <- peekToken
    case _type tok of
        Semicolon -> advance >> return ()
        (Reserved x) | x `elem` beginsStatement -> return ()
        EOF -> return ()
        _ -> advance >> synchronize


panic :: Token -> Parser a
panic tok = throwError (ParseError tok)

parseError' :: Token -> String -> Parser a
parseError' tok msg = do
    lift $ report (parseError tok msg)
    panic tok

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

-- | Parses a sequence of `next` separated by `matchingTypes`, left-associative.
infixLStream :: [TokenType] -> Parser Expr -> Parser Expr
infixLStream matchingTypes next = next >>= go
    where
    go acc = do
        mbTok <- match matchingTypes
        case mbTok of
            Nothing -> return acc
            (Just tok) -> next >>= go . Binary acc tok

-- | Parses a `next` preceded by any number of `matchingTypes`.
prefixStream :: [TokenType] -> Parser Expr -> Parser Expr
prefixStream matchingTypes next = go
    where
    go = do
        mbTok <- match matchingTypes
        case mbTok of
            Nothing -> next
            (Just tok) -> Unary tok <$> go

-- | List of infix operators, ordered low->high precedence.
-- | Operators of the same precedence are grouped.
infixOperators :: [[TokenType]]
infixOperators = map (map Operator) [
    -- Equality
    [O_EqualEqual, O_BangEqual],
    -- Comparison
    [O_Less, O_LessEqual, O_Greater, O_GreaterEqual],
    -- Addition
    [O_Plus, O_Minus],
    -- Multiplication
    [O_Slash, O_Star]]

-- | List of prefix operators.
-- | All prefix operators have the same precedence, so no need to sort them.
prefixOperators :: [TokenType]
prefixOperators = map Operator [O_Bang, O_Minus]

expr :: Parser Expr
expr = foldr infixLStream (prefixStream prefixOperators primary) infixOperators
    where
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
