{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-missing-signatures #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
module HLox.Control.Parser2 (Parser(), ParseErr(..), expr, stmt, decl, program, convert) where

import Text.Megaparsec
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified HLox.Data.Token as HT
import Data.Proxy
import HLox.Data.Token (Reserved(..), OpToken(..))

type Parser = Parsec ParseErr MyStream

data ParseErr = AssignErr Expr
    deriving (Eq, Ord)

instance Show ParseErr where
    show (AssignErr lhs) = "Cannot assign to left-hand side: " ++ show lhs
instance ShowErrorComponent ParseErr where
    showErrorComponent = show

program :: Parser Program
program = many (withRecovery synchronize decl) <* eof

synchronize err = registerParseError err *> takeWhileP Nothing (\x -> tokenVal x /= Semicolon) *> semicolon *> decl

decl :: Parser Decl
decl = choice [
    reserved R_Fun *> (Fun <$> funDecl "function"),
    reserved R_Class *> classDecl <?> "class declaration",
    reserved R_Var *> (varDecl <?> "variable declaration") <* semicolon,
    Stmt <$> stmt]

varDecl, classDecl :: Parser Decl
varDecl = Var <$> identifier <*> optional (op O_Equal *> expr <?> "initializer")
classDecl = Class
    <$> (identifier <?> "class name")
    <*> optional (op O_Less *> identifier <?> "superclass name")
    <*> braces (many (funDecl "method"))
funDecl :: String -> Parser FunDecl
funDecl kind = F
    <$> (identifier <?> kind ++ " name")
    <*> parens (identifier `sepBy` comma <?> "parameter list")
    <*> braces (many stmt <?> kind ++ " body")
    <?> kind ++ " declaration"

stmt :: Parser Stmt
stmt = choice [
    Block <$> braces (many decl) <?> "block",
    reserved R_Print *> (Print <$> expr) <* semicolon,
    reserved R_Return *> (Return <$> expr) <* semicolon,
    reserved R_If *> (If <$> parens expr <*> stmt <*> optional (reserved R_Else *> stmt)),
    reserved R_While *> (While <$> parens expr <*> stmt),
    reserved R_For *> forStmt,
    Expr <$> expr <* semicolon] <?> "statement"

-- | Syntactic sugar for the "for" loop
forStmt :: Parser Stmt
forStmt = do
    void openparen
    initializer <- optional $ choice [reserved R_Var *> (Block . singleton <$> varDecl), Expr <$> expr]
    void semicolon
    test <- choice [Literal LitTrue <$ lookAhead semicolon, expr]
    void semicolon
    update <- optional (Expr <$> expr)
    void closeparen
    body <- stmt
    let body' = maybe body (body <>) update
    let loop = While test body'
    return $ maybe loop (<> loop) initializer

expr :: Parser Expr
expr = makeExprParser term optable <?> "expression"

-- | Operator table, ordered by DESCENDING precedence.
optable :: [Operations Parser Expr]
optable = [
    postfix' [call <$ lookAhead openparen, get <$ dot],
    prefix [Unary UMinus <$ op O_Minus, Unary Not <$ op O_Bang],
    infixL [Binary Mult <$ op O_Star, Binary Div <$ op O_Slash],
    infixL [Binary Plus <$ op O_Plus, Binary Minus <$ op O_Minus],
    infixL [Binary LessEq <$ op O_LessEqual, Binary Less <$ op O_Less,
            Binary GreaterEq <$ op O_GreaterEqual, Binary Greater <$ op O_Greater],
    infixL [Binary Equals <$ op O_EqualEqual, Binary NotEquals <$ op O_BangEqual],
    infixL [Logic And <$ reserved R_And],
    infixL [Logic Or <$ reserved R_Or],
    infixR' [assignment <$ op O_Equal]]

assignment :: Expr -> Expr -> Parser Expr
assignment (Variable name) rhs = return $ Assign name rhs
assignment (Get target name) rhs = return $ Set target name rhs
assignment lhs _ = customFailure (AssignErr lhs)

call, get :: Expr -> Parser Expr
call x = Call x <$> parens (sepBy expr comma <?> "argument list")
get x = Get x <$> (identifier <?> "property name")

term :: Parser Expr
term = choice [
    parens expr,
    This <$ reserved R_This,
    Super <$> (reserved R_Super *> dot *> identifier),
    Literal <$> literal,
    Variable <$> identifier] <?> "term"

identifier :: Parser MyToken
identifier = satisfy (isIdentifier.tokenVal) <?> "identifier"
    where isIdentifier (TokId _) = True
          isIdentifier _ = False
literal :: Parser Literal
literal = token (test.tokenVal) mempty <?> "literal"
    where test (TokLit x) = Just x
          test _ = Nothing
op :: HT.OpToken -> Parser MyToken
op typ = satisfy (\x -> tokenVal x == TokOp typ)
reserved :: HT.Reserved -> Parser MyToken
reserved typ = satisfy (\x -> tokenVal x == TokRes typ)

semicolon = satisfy (\x -> tokenVal x == Semicolon) <?> ";"
dot = satisfy (\x -> tokenVal x == Dot) <?> "."
comma = satisfy (\x -> tokenVal x == Comma) <?> ","
openparen :: Parser MyToken
openparen = satisfy (\x -> tokenVal x == Open HT.Paren) <?> "("
closeparen :: Parser MyToken
closeparen = satisfy (\x -> tokenVal x == Close HT.Paren) <?> ")"
openbrace :: Parser MyToken
openbrace = satisfy (\x -> tokenVal x == Open HT.Brace) <?> "("
closebrace :: Parser MyToken
closebrace = satisfy (\x -> tokenVal x == Close HT.Brace) <?> ")"
parens = between openparen closeparen
braces = between openbrace closebrace



--------

newtype Identifier = Identifier String
    deriving (Eq, Ord, Show)

convert :: [HT.Token] -> [MyToken]
convert xs = typechange <$> takeWhile (\x -> HT._type x /= HT.EOF) xs
typechange :: HT.Token -> MyToken
typechange (HT.Token typ lexeme lin col) = WithPos start end (length lexeme) token'
  where
    start = SourcePos "<file>" (mkPos lin) (mkPos col)
    end = SourcePos "<file>" (mkPos lin) (mkPos $ col + length lexeme)
    token' = case typ of
        (HT.Reserved R_True) -> TokLit LitTrue
        (HT.Reserved R_False) -> TokLit LitFalse
        (HT.Reserved R_Nil) -> TokLit LitNil
        (HT.Reserved r) -> TokRes r
        (HT.LitToken (HT.LT_Str s)) -> TokLit (LitStr s)
        (HT.LitToken (HT.LT_Num d)) -> TokLit (LitNum d)
        (HT.Identifier nm) -> TokId (Identifier nm)
        (HT.Operator o) -> TokOp o
        (HT.Open b) -> Open b
        (HT.Close b) -> Close b
        HT.Comma -> Comma
        HT.Dot -> Dot
        HT.Semicolon -> Semicolon
        HT.EOF -> error "eof"

printToken :: MyToken' -> String
printToken (TokLit LitTrue) = "true"
printToken (TokLit LitFalse) = "false"
printToken (TokLit LitNil) = "nil"
printToken (TokLit (LitStr s)) = show s
printToken (TokLit (LitNum d)) = show d
printToken (TokId (Identifier x)) = x
printToken (TokRes typ) = show typ
printToken (TokOp typ) = show typ
printToken (Open HT.Paren) = "("
printToken (Open HT.Brace) = "{"
printToken (Close HT.Paren) = ")"
printToken (Close HT.Brace) = "}"
printToken Semicolon = ";"
printToken Dot = "."
printToken Comma = ","

data MyStream = MyStream {
    input :: String,
    lexed :: [WithPos MyToken']
} deriving (Eq, Ord, Show)

pxy :: Proxy MyStream
pxy = Proxy

instance Stream MyStream where
    type Token MyStream = WithPos MyToken'
    type Tokens MyStream = [WithPos MyToken']
    tokenToChunk Proxy x = [x]
    tokensToChunk Proxy xs = xs
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ (MyStream _ []) = Nothing
    take1_ (MyStream s (x:xs)) = Just (x, MyStream (drop (tokenLength x) s) xs)
    takeN_ n str@(MyStream s xs)
        | n <= 0 = Just ([], str)
        | null xs = Nothing
        | otherwise = 
            let (taken, dropped) = splitAt n xs
                len = tokensLength pxy $ head taken :| tail taken
            in Just (taken, MyStream (drop len s) dropped)
    takeWhile_ prop (MyStream s xs) =
        let (taken, dropped) = span prop xs
            len = if null taken then 0 else tokensLength pxy $ head taken :| tail taken
        in (taken, MyStream (drop len s) dropped)

instance VisualStream MyStream where
    showTokens Proxy = unwords . NE.toList . fmap (printToken . tokenVal)
    tokensLength Proxy = sum . NE.toList . fmap tokenLength

instance TraversableStream MyStream where
    reachOffset o PosState{..} =
        ( Just (pfix ++ restOfLine)
        , PosState
            { pstateInput = MyStream
                { input = postStr
                , lexed = post
                }
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = pfix
            }
        )
        where
        pfix =
            if sameLine
            then pstateLinePrefix ++ preLine
            else preLine
        sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
        newSourcePos =
            case post of
            [] -> case lexed pstateInput of
                [] -> pstateSourcePos
                xs -> endPos (last xs)
            (x:_) -> startPos x
        (pre, post) = splitAt (o - pstateOffset) (lexed pstateInput)
        (preStr, postStr) = splitAt tokensConsumed (input pstateInput)
        preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
        tokensConsumed =
            case NE.nonEmpty pre of
            Nothing -> 0
            Just nePre -> tokensLength pxy nePre
        restOfLine = takeWhile (/= '\n') postStr

----------

data Expr
    = Binary BinOp Expr Expr  -- lhs op rhs
    | Logic LogicOp Expr Expr -- lhs op rhs
    | Unary UnOp Expr         -- op rhs
    | Literal Literal         -- value
    | Variable MyToken     -- var
    | Assign MyToken Expr  -- var = rhs
    | This                    -- this
    | Super MyToken        -- super.method
    | Call Expr [Expr]        -- callee(args)
    | Get Expr MyToken     -- object.property
    | Set Expr MyToken Expr -- object.field = rhs
    deriving (Eq, Ord, Show)

data LogicOp = And | Or
    deriving (Eq, Ord, Show)

data BinOp = Equals | NotEquals | Less | LessEq | Greater | GreaterEq | Plus | Minus | Div | Mult
    deriving (Eq, Ord, Show)

data UnOp = Not | UMinus
    deriving (Eq, Ord, Show)

data Literal
    = LitNum Double
    | LitStr String
    | LitTrue
    | LitFalse
    | LitNil
    deriving (Eq, Ord, Show)

data FunDecl = F MyToken [MyToken] [Stmt] -- ^ fun name(arg1,...,argN) { body }
    deriving (Eq, Ord, Show)

type Program = [Decl]

data Decl
    = Fun FunDecl -- ^ fun name(arg1,...,argN) { body }
    | Class MyToken (Maybe MyToken) [FunDecl] -- ^ class name [< superclass] { body }
    | Var MyToken (Maybe Expr) -- ^ var token [= expr];
    | Stmt Stmt
    deriving (Eq, Ord, Show)

data Stmt
    = Print Expr -- ^ print expr;
    | Return Expr -- ^ return expr;
    | Block [Decl] -- ^ { stmt... }
    | If Expr Stmt (Maybe Stmt) -- ^ if (expr) thenStmt [else elseStmt]
    | While Expr Stmt -- ^ while (expr) body
    | Expr Expr -- ^ expr;
    deriving (Eq, Ord, Show)

instance Semigroup Stmt where
    (Block xs) <> (Block ys) = Block (xs <> ys)
    x <> (Block ys) = Block (Stmt x:ys)
    (Block xs) <> y = Block (xs ++ [Stmt y])
    x <> y = Block [Stmt x, Stmt y]