module HLox.Control.Interpreter (
    Runtime, withRuntime, eval, exec, runProgram, liftHLox
) where

import Prelude hiding (lookup)
import qualified HLox.Control.Base as Base
import HLox.Control.Env
import HLox.Data.Expr
import HLox.Data.Stmt
import HLox.Data.Value
import HLox.Data.Token
import Control.Monad.State
import Data.Foldable
import Data.Time.Clock.System (SystemTime(..), getSystemTime)
import Control.Monad.Except
import Data.Functor
import Data.Void
import qualified Data.Map as M
import Data.IORef
import Control.Applicative
import Data.Maybe

-- | The global environment.
globalEnv :: IO (Env (IORef Value))
globalEnv = define "clock" (VFun (FFI Clock)) emptyEnv

data Return = ReturnErr Token Value
    deriving Show

newtype RuntimeState = S {
    _env :: Env (IORef Value)
}
type Runtime = StateT RuntimeState (Base.HLox' Return)

getEnv :: Runtime (Env (IORef Value))
getEnv = gets _env

setEnv :: Env (IORef Value) -> Runtime ()
setEnv e = modify (\(S _) -> S e)

modifyEnv :: (Env (IORef Value) -> Env (IORef Value)) -> Runtime ()
modifyEnv f = modify (\(S e) -> S (f e))

modifyEnv' :: (Env (IORef Value) -> IO (Env (IORef Value))) -> Runtime ()
modifyEnv' f = do
    e <- getEnv
    t <- liftIO $ f e
    setEnv t

initState :: IO RuntimeState
initState = S <$> globalEnv

liftHLox :: Base.HLox a -> Runtime a
liftHLox = lift . withExceptT (fmap absurd)

withRuntime :: Runtime a -> Base.HLox a
withRuntime rt = Base.withError badReturn $ liftIO initState >>= evalStateT rt
    where
        badReturn (ReturnErr tok _) = Base.runtimeError tok "Cannot return from outside a function."

runtimeError :: Token -> String -> Runtime a
runtimeError tok msg = liftHLox $ Base.throw (Base.runtimeError tok msg)

unimplemented :: String -> Runtime a
unimplemented msg = liftHLox $ Base.throw (Base.unimplemented msg)

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
    success <- liftIO $ assign name val env
    unless success (runtimeError tok $ "Undefined variable '" ++ name ++ "'.")
    return val

lookup' :: Token -> String -> Runtime Value
lookup' tok msg = do
    env <- getEnv
    let mbVal = lookup (_lexeme tok) env
    case mbVal of
        Nothing -> runtimeError tok msg
        (Just ref) -> liftIO $ readIORef ref

callFun :: LoxCallable -> [Value] -> Runtime Value
callFun (FFI Clock) _ = do
    (MkSystemTime sec _) <- liftHLox.lift $ getSystemTime
    return (VNum $ fromIntegral sec)
callFun (CallableFun (LoxFun _ isInitializer params body closure)) args = do
    oldEnv <- getEnv
    setEnv closure
    modifyEnv newScope
    traverse_ (\(p,v) -> modifyEnv' (define (_lexeme p) v)) (zip params args)
    out <- Base.catchCustom (traverse_ exec body $> VNil) (\(ReturnErr _ v) -> return v)
    setEnv oldEnv
    if isInitializer then do
        let ref = M.findWithDefault (error "HLOX BUG: no bound 'this' in initializer???") "this" (peekScope closure)
        liftIO $ readIORef ref
    else return out

findMethod :: String -> LoxClass -> Maybe LoxFun
findMethod name (LoxClass _ methods super) = M.lookup name methods <|> (super >>= findMethod name)

bindMethod :: Value -> LoxFun -> Runtime Value
bindMethod this (LoxFun nm isInit par bod closure) = do
    newClosure <- liftIO $ define "this" this closure
    return $ VFun (CallableFun (LoxFun nm isInit par bod newClosure))

eval :: Expr -> Runtime Value
eval (Literal l) = return (fromLiteral l)
eval (Variable tok) = lookup' tok $ "Undefined variable '" ++ _lexeme tok ++ "'."
eval (This tok) = lookup' tok "Can't use 'this' outside a class."
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
eval (Call callee tok args) = do
    fn <- eval callee
    argv <- traverse eval args
    case fn of
        (VFun f) -> do
            let n = length argv
            when (n /= arity f) $ runtimeError tok $ "Expected " ++ show (arity f) ++ " arguments but got " ++ show n
            callFun f argv
        (VClass klass) -> do
            store <- liftIO $ newIORef M.empty
            let this = VInstance klass store
            let initializer = fromMaybe (LoxFun tok True [] [] emptyEnv) (findMethod "init" klass)
            let n = length argv
            let k = arity (CallableFun initializer)
            when (n /= k) $ runtimeError tok $ "Expected " ++ show k ++ " arguments but got " ++ show n
            (VFun f) <- bindMethod this initializer
            _ <- callFun f argv
            return this
        _ -> runtimeError tok "Can only call functions and classes."
eval (Get target tok) = do
    lhs <- eval target
    case lhs of
        this@(VInstance klass store) -> do
            fields <- liftIO $ readIORef store
            case M.lookup (_lexeme tok) fields of
                (Just val) -> return val
                Nothing -> case findMethod (_lexeme tok) klass of
                    (Just fn) -> bindMethod this fn
                    Nothing -> runtimeError tok $ "Undefined property '" ++ _lexeme tok ++ "'."
        _ -> runtimeError tok "Only instances have properties."
eval (Set target tok expr) = do
    lhs <- eval target
    case lhs of
        (VInstance _ store) -> do
            rhs <- eval expr
            contents <- liftIO $ readIORef store
            liftIO $ writeIORef store (M.insert (_lexeme tok) rhs contents)
            return rhs
        _ -> runtimeError tok "Only instances have properties."
eval (Super tok method) = do
    (VClass super) <- lookup' tok "Can't use 'super' outside of a subclass."
    this <- do
        mbThis <- lookup "this" <$> getEnv
        maybe (runtimeError tok "Can't use 'super' outside of a method.") (liftIO . readIORef) mbThis
    case findMethod (_lexeme method) super of
        (Just fn) -> bindMethod this fn
        Nothing -> runtimeError method "Superclass method not found"

exec :: Stmt -> Runtime ()
exec (Print e) = do
    v <- eval e
    liftHLox.lift $ print v
exec (Expr e) = void (eval e)
exec (Var var Nothing) = modifyEnv' (define (_lexeme var) VNil)
exec (Var var (Just e)) = do
    val <- eval e
    modifyEnv' (define (_lexeme var) val)
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
exec (Fun (F name params body)) = do
    closure <- getEnv
    modifyEnv' (define (_lexeme name) (VFun (CallableFun (LoxFun name False params body closure))))
exec (Class name mSuper methods) = do
    superclass <- traverse (\tok -> do
        superclass <- eval (Variable tok)
        case superclass of
            (VClass klass) -> return klass
            _ -> runtimeError tok "Superclass must be a class.") mSuper
    modifyEnv newScope -- New scope to hold `this` and `super`
    for_ superclass (\klass -> modifyEnv' (define "super" (VClass klass)))
    closure <- getEnv
    let methodMap = M.fromList [(_lexeme fname, LoxFun fname (_lexeme fname == "init") params body closure) | (F fname params body) <- methods]
    modifyEnv dropScope
    modifyEnv' (define (_lexeme name) (VClass (LoxClass name methodMap superclass)))
exec (Return tok expr) = do
    val <- eval expr
    Base.throwCustom (ReturnErr tok val)

runProgram :: [Stmt] -> Runtime ()
runProgram = traverse_ exec