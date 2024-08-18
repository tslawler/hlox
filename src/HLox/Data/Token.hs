{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module HLox.Data.Token (
    Token(..), TokenType(..), LitToken(..), Brace(..), OpToken(..), Reserved(..), reserved, isIdentifier
) where

data Token = Token {
    _type :: TokenType,
    _lexeme :: String,
    _line :: Int,
    _col :: Int
} deriving (Eq, Ord, Show)

data TokenType
    = Reserved Reserved
    | LitToken LitToken
    | Identifier String
    | Operator OpToken
    | Open Brace
    | Close Brace
    | Comma
    | Dot
    | Semicolon
    | EOF
    deriving (Eq, Ord, Show)

isIdentifier :: TokenType -> Bool
isIdentifier (Identifier _) = True
isIdentifier _ = False

data LitToken
    = LT_Str String
    | LT_Num Double
    deriving (Eq, Ord, Show)

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

reserved :: [(String, Reserved)]
reserved =
    [ ("true", R_True)
    , ("false", R_False)
    , ("nil", R_Nil)
    , ("and", R_And)
    , ("or", R_Or)
    , ("if", R_If)
    , ("else", R_Else)
    , ("for", R_For)
    , ("while", R_While)
    , ("print", R_Print)
    , ("var", R_Var)
    , ("fun", R_Fun)
    , ("return", R_Return)
    , ("class", R_Class)
    , ("super", R_Super)
    , ("this", R_This)
    ]