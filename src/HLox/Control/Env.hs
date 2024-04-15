{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module HLox.Control.Env (
    Env(), globalEnv, define, assign, lookup, newScope, dropScope
) where

import Prelude hiding (lookup)

import HLox.Data.Value

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M

type Scope = M.Map String Value
type Env = NonEmpty Scope

data Zipper a = Zip [a] a [a] deriving (Eq, Ord, Functor)

fromZipper :: Zipper Scope -> Env
fromZipper (Zip l m r) = go l m r
    where
        go [] m r = m :| r
        go (a:as) m r = go as a (m:r)

findScope :: String -> Env -> Maybe (Zipper Scope)
findScope name (m:|ms) = go [] m ms
    where
        go l m r = case M.lookup name m of
            Nothing -> case r of
                [] -> Nothing
                (r1:rs) -> go (m:l) r1 rs
            (Just _) -> Just $ Zip l m r

newScope :: Env -> Env
newScope (m:|ms) = M.empty :| (m:ms)

dropScope :: Env -> Env
dropScope (_:|(m:ms)) = m:|ms
dropScope (_:|[]) = error "BUG: Can't drop the global scope!!!"

-- | The global environment.
globalEnv :: Env
globalEnv = M.fromList [("clock", VFun (FFI Clock))] :| []

-- | Define a new name in the current local environment.
define :: String -> Value -> Env -> Env
define name val (m:|ms) = M.insert name val m :| ms

-- | Assign a value to a name in the closest environment it appears in.
-- Returns the new environment, or `Nothing` if the variable is undefined.
assign :: String -> Value -> Env -> Maybe Env
assign name val env = do
    (Zip l m r) <- findScope name env
    return . fromZipper $ Zip l (M.insert name val m) r

-- | Get the value of a variable in the environment.
-- Returns `Nothing` if the variable is undefined.
lookup :: String -> Env -> Maybe Value
lookup name env = do
    (Zip _ m _) <- findScope name env
    M.lookup name m
