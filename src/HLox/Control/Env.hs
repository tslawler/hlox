{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module HLox.Control.Env (
    Env(), emptyEnv, define, assign, lookup, newScope, dropScope
) where

import Prelude hiding (lookup)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M
import Data.IORef

type Scope a = M.Map String a
type Env a = NonEmpty (Scope a)

data Zipper a = Zip [a] a [a] deriving (Eq, Ord, Functor)

findScope :: String -> Env a -> Maybe (Zipper (Scope a))
findScope name (m:|ms) = go [] m ms
    where
        go l m r = case M.lookup name m of
            Nothing -> case r of
                [] -> Nothing
                (r1:rs) -> go (m:l) r1 rs
            (Just _) -> Just $ Zip l m r

newScope :: Env a -> Env a
newScope (m:|ms) = M.empty :| (m:ms)

dropScope :: Env a -> Env a
dropScope (_:|(m:ms)) = m:|ms
dropScope (_:|[]) = error "BUG: Can't drop the global scope!!!"

emptyEnv :: Env a
emptyEnv = M.empty :| []

-- | Define a new name in the current local environment.
define :: String -> a -> Env (IORef a) -> IO (Env (IORef a))
define name val (m:|ms) = do
    ref <- newIORef val
    return $ M.insert name ref m :| ms

-- | Assign a value to a name in the closest environment it appears in.
-- Returns True if successful, or False if the variable is undefined.
assign :: String -> a -> Env (IORef a) -> IO Bool
assign name val env = case lookup name env of
    Nothing -> return False
    (Just ref) -> do
        writeIORef ref val
        return True

-- | Get the value of a variable in the environment.
-- Returns `Nothing` if the variable is undefined.
lookup :: String -> Env a -> Maybe a
lookup name env = do
    (Zip _ m _) <- findScope name env
    M.lookup name m
