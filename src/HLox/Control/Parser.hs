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
import Data.Functor

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

-- | Reports a parse error, but continues.
reportError :: Token -> String -> Parser ()
reportError tok msg = do
    lift.lift $ report (Base.parseError tok msg)
    modify (\(ev, toks) -> (ev <> Dual [tok], toks))

-- | Reports a parse error and panics
parseError :: Token -> String -> Parser a
parseError tok msg = do
    reportError tok msg
    throwError ()

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

consumeP' ::  (TokenType -> Bool) -> String -> Parser Token
consumeP' predicate msg = do
    tok <- takeToken
    unless (predicate (_type tok)) $ parseError tok msg
    return tok

consume' :: TokenType -> String -> Parser Token
consume' typ = consumeP' (== typ)

consumeP :: (TokenType -> Bool) -> String -> Parser ()
consumeP predicate = void . consumeP' predicate

consume :: TokenType -> String -> Parser ()
consume typ = consumeP (== typ)

data Operations =
    Prefix (Token -> Expr -> Expr) [TokenType]
    | InfixL (Expr -> Token -> Expr -> Expr) [TokenType]
    | InfixR (Expr -> Token -> Expr -> Expr) [TokenType]

type OpTable = [Operations]

-- | List of infix operators, ordered low->high precedence.
-- | Operators of the same precedence are grouped.
opTable :: OpTable
opTable = [
    -- Assignment is excluded, we handle it separately.
    -- Or
    InfixL Logic $ Reserved <$> [R_Or],
    -- And
    InfixL Logic $ Reserved <$> [R_And],
    -- Equality
    InfixL Binary $ Operator <$> [O_EqualEqual, O_BangEqual],
    -- Comparison
    InfixL Binary $ Operator <$> [O_Less, O_LessEqual, O_Greater, O_GreaterEqual],
    -- Addition
    InfixL Binary $ Operator <$> [O_Plus, O_Minus],
    -- Multiplication
    InfixL Binary $ Operator <$> [O_Slash, O_Star],
    -- Prefix
    Prefix Unary $ Operator <$> [O_Bang, O_Minus]]

operations :: Operations -> Parser Expr -> Parser Expr
operations (InfixL f matchingTypes) next = next >>= go
    where
        go acc = match matchingTypes >>= maybe (return acc) (\tok -> next >>= go . f acc tok)
operations (InfixR f matchingTypes) next = go
    where
        go = do
            lhs <- next
            match matchingTypes >>= maybe (return lhs) (\tok -> f lhs tok <$> go)
operations (Prefix f matchingTypes) next = go
    where
        go = match matchingTypes >>= maybe next (\tok -> f tok <$> go)

expr :: Parser Expr
expr = assignment

assignment :: Parser Expr
assignment = do
    lhs <- baseExpr
    match [Operator O_Equal] >>= maybe (return lhs) (assign' lhs)
    where
        assign' lhs eqTok = do
            rhs <- assignment
            case lhs of
                Variable name -> return $ Assign name rhs
                Get target token -> return $ Set target token rhs
                _ -> lhs <$ reportError eqTok ("Invalid assignment target: " ++ show lhs)

commaList :: Parser a -> String -> Parser [a]
commaList p kind = match [Close Paren] >>= maybe (more id) (\_ -> return [])
    where
        more acc = do
            e <- p
            let acc' = acc . (e:)
            match [Comma] >>= maybe (finish acc') (\_ -> more acc')
        finish acc = consume' (Close Paren) "Expected ')' after arguments." >>= checkLen (acc [])
        checkLen xs tok =
            when (length xs >= 255) (reportError tok $ "Can't have more than 255 " ++ kind) $> xs

baseExpr :: Parser Expr
baseExpr = foldr operations call opTable
    where
    call = primary >>= go
        where
        go acc = do
            tok <- peekToken
            case _type tok of
                Open Paren -> advance >> commaList expr "arguments" >>= go . Call acc tok
                Dot -> advance >> consumeP' isIdentifier "Expected property name after '.'" >>= go . Get acc
                _ -> return acc
    primary = do
        tok <- takeToken
        case _type tok of
            (Identifier _) -> return.Variable $ tok
            (LitToken lt) -> return.Literal $ fromLitToken lt
            (Reserved R_True) -> return.Literal $ LitTrue
            (Reserved R_False) -> return.Literal $ LitFalse
            (Reserved R_Nil) -> return.Literal $ LitNil
            (Reserved R_This) -> return.This $ tok
            (Open Paren) -> do
                e <- expr
                consume (Close Paren) "Expected ')' after expression."
                return $ Grouping e
            _ -> parseError tok "Expected an expression."

-- | Parses a block, not including the open brace at the start, but including the close brace at the end.
block :: Parser [Stmt]
block = do
    tok <- peekToken
    case _type tok of
        EOF -> parseError tok "Expected '}' at end of block." $> []
        (Close Brace) -> advance $> []
        _ -> maybe id (:) <$> decl <*> block

exprStmt :: Parser Stmt
exprStmt = do
    e <- expr
    consume Semicolon "Expected ';' after expression."
    return $ Expr e

-- | Desugars a `for` statement into a `while`.
forStmt :: Parser Stmt
forStmt = do
    consume (Open Paren) "Expected '(' after 'for'."
    initStart <- peekToken
    initializer <- case _type initStart of
        Semicolon -> advance $> Nothing
        (Reserved R_Var) -> advance *> (pure <$> varStmt)
        _ -> pure <$> exprStmt
    noCondition <- (== Semicolon) . _type <$> peekToken
    cond <- if noCondition then return (Literal LitTrue) else expr
    consume Semicolon "Expected ';' after for condition."
    incrStart <- peekToken
    increment <- case _type incrStart of
        (Close Paren) -> return Nothing
        _ -> pure.Expr <$> expr
    consume (Close Paren) "Expected ')' after for clauses."
    body <- stmt
    let body' = While cond (maybe body (body <>) increment)
    return $ maybe body' (<> body') initializer

stmt :: Parser Stmt
stmt = do
    tok <- peekToken
    case _type tok of
        (Reserved R_If) -> do
            advance
            consume (Open Paren) "Expected '(' after 'if'."
            cond <- expr
            consume (Close Paren) "Expected ')' after condition."
            thn <- stmt
            match [Reserved R_Else] >>= maybe (return $ If cond thn Nothing) (\_ -> If cond thn . Just <$> stmt)
        (Reserved R_While) -> do
            advance
            consume (Open Paren) "Expected '(' after 'while'."
            cond <- expr
            consume (Close Paren) "Expected ')' after condition."
            While cond <$> stmt
        (Reserved R_For) -> advance *> forStmt
        (Reserved R_Print) -> do
            advance
            e <- expr
            consume Semicolon "Expected ';' after value."
            return $ Print e
        (Reserved R_Return) -> do
            returnToken <- takeToken
            e <- expr
            consume Semicolon "Expected ';' after return value."
            return $ Return returnToken e
        (Open Brace) -> do
            advance
            Block <$> block
        _ -> exprStmt

-- | Parse the portion of a 'Var' statement after the initial 'var'.
varStmt :: Parser Stmt
varStmt = do
    tok <- consumeP' isIdentifier "Expected identifier after 'var'."
    mexpr <- match [Operator O_Equal] >>= maybe (return Nothing) (\_ -> Just <$> expr)
    consume Semicolon "Expected ';' after variable declaration."
    return $ Var tok mexpr

identifier :: Parser Token
identifier = consumeP' isIdentifier "Expected identifier."

-- | Parse the portion of a 'Fun' statement after the initial 'fun'.
funStmt :: String -> Parser FunDecl
funStmt kind = do
    tok <- consumeP' isIdentifier $ "Expected " ++ kind ++ " name."
    consume (Open Paren) $ "Expected '(' after " ++ kind ++ " name."
    params <- commaList identifier "parameters"
    consume (Open Brace) $ "Expected '{' after " ++ kind ++ " parameters."
    F tok params <$> block

classStmt :: Parser Stmt
classStmt = do
    tok <- consumeP' isIdentifier "Expected class name."
    -- TODO: inheritance
    consume (Open Brace) "Expected '{' after class name."
    methods <- go id
    consume (Close Brace) "Expected '}' at end of class."
    return $ Class tok methods
  where
    go f = do
        tok <- peekToken
        case _type tok of
            EOF -> return $ f []
            (Close Brace) -> return $ f []
            _ -> do
                method <- funStmt "method"
                go (f.(method:))

decl' :: Parser Stmt
decl' = do
    start <- peekToken
    case _type start of
        (Reserved R_Fun) -> advance *> (Fun <$> funStmt "function")
        (Reserved R_Class) -> advance *> classStmt
        (Reserved R_Var) -> advance *> varStmt
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