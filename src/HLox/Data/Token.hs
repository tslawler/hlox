{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module HLox.Data.Token (
    Token(..), TokenType(..), Brace(..), OpToken(..), Reserved(..), reserved, isIdentifier
) where

import HLox.Data.Literal
import Text.Megaparsec.Pos

data WithPos a = WithPos {
    startPos :: SourcePos,
    endPos :: SourcePos,
    tokenLength :: Int,
    tokenVal :: a
} deriving (Eq, Ord, Show)

type Token = WithPos TokenType

data TokenType
    = TokLit Literal
    | TokId String
    | TokRes Reserved
    | TokOp OpToken
    | Open Brace
    | Close Brace
    | Semicolon
    | Dot
    | Comma
    deriving (Eq, Ord, Show)

isIdentifier :: TokenType -> Bool
isIdentifier (TokId _) = True
isIdentifier _ = False

data Brace = Paren | Brace deriving (Eq, Ord, Show, Bounded)

data OpToken
    = O_Plus
    | O_Minus
    | O_Star
    | O_Slash
    | O_Bang
    | O_Equal
    | O_EqualEqual
    | O_BangEqual
    | O_Less
    | O_LessEqual
    | O_Greater
    | O_GreaterEqual
    deriving (Eq, Ord, Show, Bounded)

data Reserved
    = R_And
    | R_Or
    | R_If
    | R_Else
    | R_For
    | R_While
    | R_Print
    | R_Var
    | R_Fun
    | R_Return
    | R_Class
    | R_Super
    | R_This
    | R_True
    | R_False
    | R_Nil
    deriving (Eq, Ord, Show, Bounded)

reserved :: [(String, TokenType)]
reserved =
    [ ("true", TokLit LitTrue)
    , ("false", TokLit LitFalse)
    , ("nil", TokLit LitNil) 
    , ("and", TokRes R_And)
    , ("or", TokRes R_Or)
    , ("if", TokRes R_If)
    , ("else", TokRes R_Else)
    , ("for", TokRes R_For)
    , ("while", TokRes R_While)
    , ("print", TokRes R_Print)
    , ("var", TokRes R_Var)
    , ("fun", TokRes R_Fun)
    , ("return", TokRes R_Return)
    , ("class", TokRes R_Class)
    , ("super", TokRes R_Super)
    , ("this", TokRes R_This)
    ]