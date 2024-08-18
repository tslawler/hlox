module HLox.Control.Scanner (
    lex
) where

import Prelude hiding (lex)
import Text.Megaparsec.Pos
import Data.Monoid
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import qualified Data.List as L
import Control.Monad
import Control.Monad.RWS

import HLox.Control.Base (HLox, report)
import qualified HLox.Control.Base as Base
import HLox.Data.Token

data ScannerState = S {
    _data :: [Char], -- ^ Source data yet to be processed.
    _acc :: [Char], -- ^ Characters read for the current token, in reverse order.
    _pos :: !SourcePos, -- ^ Position of the current character.
    _startPos :: !SourcePos -- ^ Position of the start of the current token.
} deriving (Eq, Ord, Show)

initState :: FilePath -> [Char] -> ScannerState
initState filename cs = S cs [] (initialPos filename) pos1

type Scanner = RWST () (Endo [Token]) ScannerState HLox

-- | The main entry point to the scanner. Given a string, produces a list of tokens.
lex :: [Char] -> HLox [Token]
lex source = do
    (_, Endo tokens) <- evalRWST lexer () (initState source)
    return $ tokens []

-- | The main loop. Repeatedly scans tokens.
lexer :: Scanner ()
lexer = do
    s <- get
    put s{_startPos = _pos s, _acc = []}
    if isAtEnd s then return () else do
        scanToken
        lexer

-- | Scan a single token.
scanToken :: Scanner ()
scanToken = do
    c <- advance
    case c of
        '(' -> addToken (Open Paren)
        ')' -> addToken (Close Paren)
        '{' -> addToken (Open Brace)
        '}' -> addToken (Close Brace)
        ',' -> addToken Comma
        '.' -> addToken Dot
        ';' -> addToken Semicolon
        '-' -> addToken (Operator O_Minus)
        '+' -> addToken (Operator O_Plus)
        '*' -> addToken (Operator O_Star)
        '!' -> do
            op <- match '=' O_BangEqual O_Bang
            addToken (Operator op)
        '=' -> do
            op <- match '=' O_EqualEqual O_Equal
            addToken (Operator op)
        '<' -> do
            op <- match '=' O_LessEqual O_Less
            addToken (Operator op)
        '>' -> do
            op <- match '=' O_GreaterEqual O_Greater
            addToken (Operator op)
        '/' -> join $ match '/' comment (addToken (Operator O_Slash))
        '\n' -> modify newline
        x | isSpace x -> return ()
        '"' -> stringLiteral
        x | isDigit x -> numberLiteral
        x | isAlpha x -> identifier
        _ -> scanError $ "Unexpected character: " ++ [c]

-- | Emits a scanner error.
scanError :: String -> Scanner ()
scanError msg = do
    s <- get
    lift $ report (Base.scanError (_line s) (_col s) msg)

-- | Returns true if we've hit end of file.
isAtEnd :: ScannerState -> Bool
isAtEnd s = null $ _data s

isSpace :: Char -> Bool
isSpace c = c `elem` " \r\t"

isAlpha :: Char -> Bool
isAlpha c = isAsciiLower c || isAsciiUpper c || c == '_'
isAlphanumeric :: Char -> Bool
isAlphanumeric c = isAlpha c || isDigit c

-- | Consumes and returns one character. Assumes that we are not at the end.
advance :: Scanner Char
advance = do
    c <- gets (head . _data)
    modify (\s -> s{_data = tail (_data s), _acc = c:_acc s, _pos = newcol (_pos s)})
    return c

-- | Increments source pos by 1 column.
newcol :: SourcePos -> SourcePos
newcol (SourcePos file line col) = SourcePos file line (col <> pos1)

-- | Increments source pos by 1 line.
newline :: SourcePos -> SourcePos
newline (SourcePos file line _) = SourcePos file (line <> pos1) pos1

-- | Returns one character past the current.
peek :: Scanner (Maybe Char)
peek = do
    cs <- gets _data
    case cs of
        [] -> return Nothing
        (c:_) -> return (Just c)

-- | Returns two characters past the current.
peek2 :: Scanner (Maybe Char, Maybe Char)
peek2 = do
    cs <- gets _data
    case cs of
        [] -> return (Nothing, Nothing)
        [c] -> return (Just c, Nothing)
        (c:d:_) -> return (Just c, Just d)

-- | If the next character matches the given one, advance and return `yes`, otherwise return `no`.
match :: Char -> a -> a -> Scanner a
match c yes no = peek >>= maybe (return no) (\c' ->
    if c == c' then advance >> return yes else return no)

-- | Adds a token of the given type to the scanner.
addToken :: TokenType -> Scanner ()
addToken tokType = addToken' (const tokType)

-- | Adds a token to the scanner, with type determined by the accumulated string.
addToken' :: (String -> TokenType) -> Scanner ()
addToken' mkType = do
    s <- get
    let val = reverse (_acc s)
    let token = WithPos (_startPos s) (_pos s) (length val) (mkType val)
    tell $ Endo (token:)
    return ()

-- | Helper function that consumes characters as long as `predicate` is true. Stops when it hits end of file.
eatWhile :: (Char -> Bool) -> Scanner ()
eatWhile predicate = go
    where
    go = do
        c <- peek
        when (maybe False predicate c) (advance >> go)

comment :: Scanner ()
comment = eatWhile (/= '\n')

stringLiteral :: Scanner ()
stringLiteral = do
    eatWhile (/= '"')
    c <- peek
    case c of
        Nothing -> scanError "Unterminated string"
        -- Guaranteed to be '"'
        (Just _) -> advance >> addToken' (TokLit . LitStr . tail . init)

numberLiteral :: Scanner ()
numberLiteral = do
    eatWhile isDigit
    (c,d) <- peek2
    when (c == Just '.' && maybe False isDigit d) (advance >> eatWhile isDigit)
    addToken' (TokLit . LitNum . read)

identifier :: Scanner ()
identifier = do
    eatWhile isAlphanumeric
    addToken' (\word -> case L.lookup word reserved of
        Nothing -> TokId word
        (Just res) -> res)