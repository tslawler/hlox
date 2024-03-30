{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module HLox.Control.Interpreter (
    interpret
) where

import HLox.Control.Base
import HLox.Data.Expr
import HLox.Data.Value
import HLox.Data.Token
import Control.Monad (unless)

fromLiteral :: Literal -> Value
fromLiteral (LitNum d) = VNum d
fromLiteral (LitStr s) = VStr s
fromLiteral LitFalse = VBool False
fromLiteral LitTrue = VBool True
fromLiteral LitNil = VNil

checkBool :: Value -> Token -> HLox Bool
checkBool (VBool b) _ = return b
checkBool v tok = throw (runtimeError tok ("Operand must be a Boolean: " ++ show v))

checkNum :: Value -> Token -> HLox Double
checkNum (VNum n) _ = return n
checkNum v tok = throw (runtimeError tok ("Operand must be a Number: " ++ show v))

checkNumOrStr :: Value -> Token -> HLox ()
checkNumOrStr (VNum _) _ = return ()
checkNumOrStr (VStr _) _ = return ()
checkNumOrStr v tok = throw (runtimeError tok ("Operand must be a Number or String: " ++ show v))

checkTypesMatch :: Value -> Value -> Token -> HLox ()
checkTypesMatch l r tok = unless (typeMatch l r) $
    throw (runtimeError tok ("Expected types to match: " ++ show l ++ " and " ++ show r))

equality :: (Eq a) => TokenType -> a -> a -> Bool
equality (Operator O_EqualEqual) = (==)
equality (Operator O_BangEqual) = (/=)

comparison :: (Ord a) => TokenType -> a -> a -> Bool
comparison (Operator O_Less) = (<)
comparison (Operator O_LessEqual) = (<=)
comparison (Operator O_Greater) = (>)
comparison (Operator O_GreaterEqual) = (>=)

numeric :: (Fractional a) => TokenType -> a -> a -> a
numeric (Operator O_Minus) = (-)
numeric (Operator O_Star) = (*)
numeric (Operator O_Slash) = (/)

plus :: Value -> Value -> Value
plus (VNum a) (VNum b) = VNum (a + b)
plus (VStr a) (VStr b) = VStr (a ++ b)


interpret :: Expr -> HLox Value
interpret (Literal l) = return (fromLiteral l)
interpret (Grouping expr) = interpret expr
interpret (Unary tok expr) 
    | _type tok == Operator O_Bang = do {
        v <- interpret expr;
        b <- checkBool v tok;
        return $ VBool (not b)
    }
    | _type tok == Operator O_Minus = do {
        v <- interpret expr;
        n <- checkNum v tok;
        return $ VNum (negate n)
    }
    | otherwise = throw . unimplemented $ "sorry, but the operator " ++ _lexeme tok ++ " is not implemented yet :<"
interpret (Binary lhs tok rhs)
    | _type tok `elem` (Operator <$> [O_EqualEqual, O_BangEqual]) = do {
        l <- interpret lhs;
        r <- interpret rhs;
        return $ VBool (equality (_type tok) l r)
    }
    | _type tok `elem` (Operator <$> [O_Less, O_LessEqual, O_Greater, O_GreaterEqual]) = do {
        l <- interpret lhs;
        r <- interpret rhs;
        vl <- checkNum l tok;
        vr <- checkNum r tok;
        return $ VBool (comparison (_type tok) vl vr)
    }
    | _type tok `elem` (Operator <$> [O_Minus, O_Star, O_Slash]) = do {
        l <- interpret lhs;
        r <- interpret rhs;
        vl <- checkNum l tok;
        vr <- checkNum r tok;
        return $ VNum (numeric (_type tok) vl vr)
    }
    | _type tok == Operator O_Plus = do {
        l <- interpret lhs;
        checkNumOrStr l tok;
        r <- interpret rhs;
        checkTypesMatch l r tok;
        return (plus l r)
    }
    | otherwise = throw . unimplemented $ "sorry, but the operator " ++ _lexeme tok ++ " is not implemented yet :<"