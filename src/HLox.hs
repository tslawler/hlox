module HLox (
    HLox(), runHLox, reportError, reportError', hadError, hadRuntimeError, clearError
) where

import Control.Monad.State
import System.IO

data HLoxState = HLoxState {
    _hadError :: !Bool,
    _hadRuntimeError :: !Bool
} deriving (Eq, Show)

data ErrorReport = ErrorReport {
    _line :: Int,
    _where :: String,
    _message :: String
} deriving (Eq)

instance Show ErrorReport where
    show (ErrorReport l w m) = "[line " ++ show l ++ "] Error" ++ w ++ ": " ++ m

initState :: HLoxState
initState = HLoxState False False

type HLox = StateT HLoxState IO

runHLox :: HLox a -> IO a
runHLox h = evalStateT h initState

reportError' :: Int -> String -> String -> HLox ()
reportError' l w m = do
    liftIO $ hPrint stderr $ ErrorReport l w m
    modify' (\s -> s{_hadError = True})

reportError :: Int -> String -> HLox ()
reportError l m = reportError' l "" m

hadError :: HLox Bool
hadError = gets _hadError

clearError :: HLox ()
clearError = modify' (\s -> s{_hadError = False})

hadRuntimeError :: HLox Bool
hadRuntimeError = gets _hadRuntimeError