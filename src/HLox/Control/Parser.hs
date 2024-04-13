{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module HLox.Control.Parser (
    parseExpr, parseProgram
) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe (isNothing)

import HLox.Control.Base (HLox, report)
import qualified HLox.Control.Base as Base
import HLox.Data.Token
import HLox.Data.Expr
import HLox.Data.Stmt
import Data.Either (fromRight)
import Data.Monoid

type ParseErrors = Dual [Token]

type ParserState = (ParseErrors, [Token])

type Parser = ExceptT () (StateT ParserState HLox)

tolerate :: Parser a -> Parser (Maybe a)
tolerate p = catchError (fmap Just p) (\_ -> return Nothing)

parseProgram :: [Token] -> HLox [Stmt]
parseProgram tokens = fromRight [] <$> runParser program tokens

parseExpr :: [Token] -> HLox Expr
parseExpr tokens = fromRight (Literal LitNil) <$> runParser expr tokens


runParser :: Parser a -> [Token] -> HLox (Either [Token] a)
runParser p toks = enforce <$> runStateT (runExceptT p) (mempty, toks)
    where
        enforce (Left _, _) = Left []
        enforce (Right a,(Dual [], _)) = Right a
        enforce (Right _,(Dual errs, _)) = Left (reverse errs)

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
panic tok = do
    modify (\(ev, toks) -> (ev <> Dual [tok], toks))
    throwError ()

parseError :: Token -> String -> Parser a
parseError tok msg = do
    lift.lift $ report (Base.parseError tok msg)
    panic tok

peekToken :: Parser Token
peekToken = gets (head.snd)

-- | If the next token is one of the given types, consume and return it.
-- | Otherwise, do nothing and return Nothing.
match :: [TokenType] -> Parser (Maybe Token)
match typs = do
    tok <- peekToken
    if _type tok `elem` typs then advance >> return (Just tok) else return Nothing

isAtEnd :: Parser Bool
isAtEnd = do
    tok <- peekToken
    return (_type tok == EOF)

advance :: Parser ()
advance = do
    b <- isAtEnd
    unless b $ modify (fmap tail)

takeToken :: Parser Token
takeToken = do
    tok <- peekToken
    advance
    return tok

consumeP :: (TokenType -> Bool) -> String -> Parser ()
consumeP predicate msg = do
    tok <- takeToken
    unless (predicate (_type tok)) $ parseError tok msg

consume :: TokenType -> String -> Parser ()
consume typ = consumeP (== typ)

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
            (Identifier _) -> return.Variable $ tok
            (LitToken lt) -> return.Literal $ fromLitToken lt
            (Reserved R_True) -> return.Literal $ LitTrue
            (Reserved R_False) -> return.Literal $ LitFalse
            (Reserved R_Nil) -> return.Literal $ LitNil
            (Open Paren) -> do
                e <- expr
                consume (Close Paren) "Expected ')' after expression."
                return $ Grouping e
            _ -> parseError tok "Expected an expression."

stmt :: Parser Stmt
stmt = do
    tok <- peekToken
    case _type tok of
        (Reserved R_Print) -> do
            advance
            e <- expr
            consume Semicolon "Expected ';' after value."
            return $ Print e
        _ -> do
            e <- expr
            consume Semicolon "Expected ';' after expression."
            return $ Expr e

decl' :: Parser Stmt
decl' = do
    start <- peekToken
    case _type start of
        (Reserved R_Var) -> do
            advance
            tok <- takeToken
            unless (isIdentifier (_type tok)) $ parseError tok "Expected identifier after 'var'."
            mexpr <- match [Operator O_Equal] >>= maybe (return Nothing) (\_ -> Just <$> expr)
            consume Semicolon "Expected ';' after variable declaration."
            return $ Var tok mexpr
        _ -> stmt

decl :: Parser (Maybe Stmt)
decl = do
    v <- tolerate decl'
    when (isNothing v) synchronize
    return v

program :: Parser [Stmt]
program = do
    tok <- peekToken
    case _type tok of
        EOF -> return []
        _ -> (maybe id (:) <$> decl) <*> program