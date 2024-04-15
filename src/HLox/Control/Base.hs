{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
module HLox.Control.Base (
    Report(), scanError, parseError, runtimeError, unimplemented,
    HLox'(), HLox(), report, throw, runHLox, withError, catchCustom, throwCustom
) where

import Control.Monad.Except ( MonadTrans(lift), ExceptT() )
import qualified Control.Monad.Except as E

import HLox.Data.Token
import System.IO
import Data.Void

data Report e = ErrorReport {
    _err_line :: Maybe Int,
    _err_col :: Maybe Int,
    _err_kind :: ErrorKind,
    _err_at :: String,
    _err_message :: String
} | Custom e deriving (Eq, Functor)

instance Show e => Show (Report e) where
    show (ErrorReport line col kind loc msg) = linestr ++ show kind ++ " Error" ++ loc ++ ": " ++ msg
        where 
            linestr = maybe "" (\l ->"[line " ++ show l ++ colstr ++ "] ") line
            colstr = maybe "" (\c -> ", col " ++ show c) col
    show (Custom e) = show e

data ErrorKind = Scan | Parse | Runtime
    deriving (Eq, Ord, Show)

scanError :: Int -> Int -> String -> Report e
scanError line col = ErrorReport (Just line) (Just col) Scan ""

parseError :: Token -> String -> Report e
parseError (Token typ lexeme line col) = ErrorReport (Just line) (Just col) Parse at
    where
        at = case typ of
            EOF -> " at end"
            _ -> " at '" ++ lexeme ++ "'"

runtimeError :: Token -> String -> Report e
runtimeError (Token _ _ line col) = ErrorReport (Just line) (Just col) Runtime ""

unimplemented :: String -> Report e
unimplemented = ErrorReport Nothing Nothing Runtime ""

type HLox' e = ExceptT (Report e) IO
type HLox = HLox' Void

withError :: (e -> Report e') -> HLox' e a -> HLox' e' a
withError f = E.withExceptT alter
    where 
        alter (Custom e) = f e
        alter (ErrorReport ln col typ loc msg) = ErrorReport ln col typ loc msg

report :: (Show e) => Report e -> HLox' e ()
report rep = do
    lift $ hPrint stderr rep
    return ()

throw :: (Show e) => Report e -> HLox' e a
throw rep = do
    report rep
    E.throwError rep

throwCustom :: (E.MonadError r m, r ~ Report e) => e -> m a
throwCustom e = E.throwError (Custom e)

catchCustom :: (E.MonadError r m, r ~ Report e) => m a -> (e -> m a) -> m a
catchCustom m f = E.catchError m katch
    where
        katch (Custom e) = f e
        katch r = E.throwError r

runHLox :: HLox' e a -> IO (Either (Report e) a)
runHLox = E.runExceptT