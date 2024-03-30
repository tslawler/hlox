module HLox.Control.Base (
    Report(), scanError, parseError, runtimeError, unimplemented,
    HLox(), report, throw, runHLox
) where

import Control.Monad.Except ( MonadTrans(lift), ExceptT() )
import qualified Control.Monad.Except as E

import HLox.Data.Token
import System.IO

data Report = ErrorReport {
    _err_line :: Maybe Int,
    _err_col :: Maybe Int,
    _err_kind :: ErrorKind,
    _err_at :: String,
    _err_message :: String
} deriving (Eq)

instance Show Report where
    show (ErrorReport line col kind loc msg) = linestr ++ show kind ++ " Error" ++ loc ++ ": " ++ msg
        where 
            linestr = maybe "" (\l ->"[line " ++ show l ++ colstr ++ "] ") line
            colstr = maybe "" (\c -> " col " ++ show c) col

data ErrorKind = Scan | Parse | Runtime
    deriving (Eq, Ord, Show)

scanError :: Int -> Int -> String -> Report
scanError line col = ErrorReport (Just line) (Just col) Scan ""

parseError :: Token -> String -> Report
parseError (Token typ lexeme line col) = ErrorReport (Just line) (Just col) Parse at
    where
        at = case typ of
            EOF -> " at end"
            _ -> " at '" ++ lexeme ++ "'"

runtimeError :: Token -> String -> Report
runtimeError (Token _ _ line col) = ErrorReport (Just line) (Just col) Runtime ""

unimplemented :: String -> Report
unimplemented = ErrorReport Nothing Nothing Runtime ""

type HLox = ExceptT Report IO

report :: Report -> HLox ()
report rep = do
    lift $ hPrint stderr rep
    return ()

throw :: Report -> HLox a
throw rep = do
    report rep
    E.throwError rep

runHLox :: HLox a -> IO (Either Report a)
runHLox = E.runExceptT