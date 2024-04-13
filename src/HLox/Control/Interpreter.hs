{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module HLox.Control.Interpreter (
    Runtime, withRuntime, eval, exec, runProgram
) where

import Prelude hiding (lookup)
import qualified HLox.Control.Base as Base
import HLox.Data.Expr
import HLox.Data.Stmt
import HLox.Data.Value
import HLox.Data.Token
import Control.Monad.RWS
import Data.Foldable
import qualified Data.Map as M

type Env = ()
type State = M.Map String Value
type Runtime = RWST Env () State Base.HLox

initEnv :: Env
initEnv = ()
initState :: State
initState = M.empty

withRuntime :: Runtime a -> Base.HLox a
withRuntime rt = fst <$> evalRWST rt initEnv initState

runtimeError :: Token -> String -> Runtime a
runtimeError tok msg = lift $ Base.throw (Base.runtimeError tok msg)

unimplemented :: String -> Runtime a
unimplemented msg = lift $ Base.throw (Base.unimplemented msg)

fromLiteral :: Literal -> Value
fromLiteral (LitNum d) = VNum d
fromLiteral (LitStr s) = VStr s
fromLiteral LitFalse = VBool False
fromLiteral LitTrue = VBool True
fromLiteral LitNil = VNil

checkBool :: Value -> Token -> Runtime Bool
checkBool (VBool b) _ = return b
checkBool v tok = runtimeError tok ("Operand must be a Boolean: " ++ show v)

checkNum :: Value -> Token -> Runtime Double
checkNum (VNum n) _ = return n
checkNum v tok = runtimeError tok ("Operand must be a Number: " ++ show v)

checkNumOrStr :: Value -> Token -> Runtime ()
checkNumOrStr (VNum _) _ = return ()
checkNumOrStr (VStr _) _ = return ()
checkNumOrStr v tok = runtimeError tok ("Operand must be a Number or String: " ++ show v)

checkTypesMatch :: Value -> Value -> Token -> Runtime ()
checkTypesMatch l r tok = unless (typeMatch l r) $
    runtimeError tok ("Expected types to match: " ++ show l ++ " and " ++ show r)

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

define :: String -> Value -> Runtime ()
define name val = modify (M.insert name val)

lookup :: Token -> Runtime Value
lookup tok = 
    gets (M.lookup (_lexeme tok)) >>= maybe (runtimeError tok $ "Undefined variable '" ++ _lexeme tok ++ "'.") return


eval :: Expr -> Runtime Value
eval (Literal l) = return (fromLiteral l)
eval (Variable tok) = lookup tok
eval (Grouping e) = eval e
eval (Unary tok e) 
    | _type tok == Operator O_Bang = do {
        v <- eval e;
        b <- checkBool v tok;
        return $ VBool (not b)
    }
    | _type tok == Operator O_Minus = do {
        v <- eval e;
        n <- checkNum v tok;
        return $ VNum (negate n)
    }
    | otherwise = error "Unreachable"
eval (Binary lhs tok rhs)
    | _type tok `elem` (Operator <$> [O_EqualEqual, O_BangEqual]) = do {
        l <- eval lhs;
        r <- eval rhs;
        return $ VBool (equality (_type tok) l r)
    }
    | _type tok `elem` (Operator <$> [O_Less, O_LessEqual, O_Greater, O_GreaterEqual]) = do {
        l <- eval lhs;
        r <- eval rhs;
        vl <- checkNum l tok;
        vr <- checkNum r tok;
        return $ VBool (comparison (_type tok) vl vr)
    }
    | _type tok `elem` (Operator <$> [O_Minus, O_Star, O_Slash]) = do {
        l <- eval lhs;
        r <- eval rhs;
        vl <- checkNum l tok;
        vr <- checkNum r tok;
        return $ VNum (numeric (_type tok) vl vr)
    }
    | _type tok == Operator O_Plus = do {
        l <- eval lhs;
        checkNumOrStr l tok;
        r <- eval rhs;
        checkTypesMatch l r tok;
        return (plus l r)
    }
    | otherwise = unimplemented $ "sorry, but the operator " ++ _lexeme tok ++ " is not implemented yet :<"

exec :: Stmt -> Runtime ()
exec (Print e) = do
    v <- eval e
    lift.lift $ print v
exec (Expr e) = void (eval e)
exec (Var var Nothing) = define (_lexeme var) VNil
exec (Var var (Just e)) = do
    val <- eval e
    define (_lexeme var) val

runProgram :: [Stmt] -> Runtime ()
runProgram = traverse_ exec