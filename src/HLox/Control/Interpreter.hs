module HLox.Control.Interpreter (
    Runtime, withRuntime, eval, exec, runProgram
) where

import Prelude hiding (lookup)
import qualified HLox.Control.Base as Base
import HLox.Control.Env
import HLox.Data.Expr
import HLox.Data.Stmt
import HLox.Data.Value
import HLox.Data.Token
import Control.Monad.RWS
import Data.Foldable

newtype State = S {
    _env :: Env
}
type Runtime = RWST () () State Base.HLox

getEnv :: Runtime Env
getEnv = gets _env

putEnv :: Env -> Runtime ()
putEnv e = modify (\(S _) -> S e)

modifyEnv :: (Env -> Env) -> Runtime ()
modifyEnv f = modify (\(S e) -> S (f e))

initState :: State
initState = S globalEnv

withRuntime :: Runtime a -> Base.HLox a
withRuntime rt = fst <$> evalRWST rt () initState

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
equality _ = error "Bug in equality"

comparison :: (Ord a) => TokenType -> a -> a -> Bool
comparison (Operator O_Less) = (<)
comparison (Operator O_LessEqual) = (<=)
comparison (Operator O_Greater) = (>)
comparison (Operator O_GreaterEqual) = (>=)
comparison _ = error "Bug in comparison"

numeric :: (Fractional a) => TokenType -> a -> a -> a
numeric (Operator O_Minus) = (-)
numeric (Operator O_Star) = (*)
numeric (Operator O_Slash) = (/)
numeric _ = error "Bug in numeric"

plus :: Value -> Value -> Value
plus (VNum a) (VNum b) = VNum (a + b)
plus (VStr a) (VStr b) = VStr (a ++ b)
plus _ _ = error "Bug in plus"

assign' :: Token -> Value -> Runtime Value
assign' tok val = let name = _lexeme tok in do
    env <- getEnv
    let mbEnv = assign name val env
    case mbEnv of
        Nothing -> runtimeError tok $ "Undefined variable '" ++ name ++ "'."
        (Just env') -> putEnv env'
    return val

lookup' :: Token -> Runtime Value
lookup' tok = let name = _lexeme tok in do
    mbVal <- lookup name <$> getEnv
    case mbVal of
        Nothing -> runtimeError tok $ "Undefined variable '" ++ name ++ "'."
        (Just val) -> return val

eval :: Expr -> Runtime Value
eval (Literal l) = return (fromLiteral l)
eval (Variable tok) = lookup' tok
eval (Assign tok e) = do
    v <- eval e
    assign' tok v
eval (Grouping e) = eval e
eval (Unary tok e)
    | _type tok == Operator O_Bang = do {
        v <- eval e;
        return $ VBool (not (truthy v))
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
eval (Logic lhs tok rhs)
    | _type tok == Reserved R_And = do {
        lv <- eval lhs;
        if truthy lv then eval rhs else return (VBool False)
    }
    | _type tok == Reserved R_Or = do {
        lv <- eval lhs;
        if truthy lv then return (VBool True) else eval rhs
    }
    | otherwise = error "Invalid logic operator?!"

exec :: Stmt -> Runtime ()
exec (Print e) = do
    v <- eval e
    lift.lift $ print v
exec (Expr e) = void (eval e)
exec (Var var Nothing) = modifyEnv (define (_lexeme var) VNil)
exec (Var var (Just e)) = do
    val <- eval e
    modifyEnv (define (_lexeme var) val)
exec (Block stmts) = do
    modifyEnv newScope
    traverse_ exec stmts
    modifyEnv dropScope
exec (If cond thn els) = do
    val <- eval cond
    if truthy val then exec thn else traverse_ exec els
exec (While cond body) = loop
    where loop = do {
        val <- eval cond;
        when (truthy val) $ exec body *> loop
    }

runProgram :: [Stmt] -> Runtime ()
runProgram = traverse_ exec