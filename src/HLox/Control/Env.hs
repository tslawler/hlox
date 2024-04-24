{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module HLox.Control.Env (
    Env(), emptyEnv, define, assign, lookup, newScope, dropScope
) where

import Prelude hiding (lookup)

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map as M

type Scope a = M.Map String a
type Env a = NonEmpty (Scope a)

data Zipper a = Zip [a] a [a] deriving (Eq, Ord, Functor)

fromZipper :: Zipper (Scope a) -> Env a
fromZipper (Zip l m r) = go l m r
    where
        go [] m r = m :| r
        go (a:as) m r = go as a (m:r)

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
define :: String -> a -> Env a -> Env a
define name val (m:|ms) = M.insert name val m :| ms

-- | Assign a value to a name in the closest environment it appears in.
-- Returns the new environment, or `Nothing` if the variable is undefined.
assign :: String -> a -> Env a -> Maybe (Env a)
assign name val env = do
    (Zip l m r) <- findScope name env
    return . fromZipper $ Zip l (M.insert name val m) r

-- | Get the value of a variable in the environment.
-- Returns `Nothing` if the variable is undefined.
lookup :: String -> Env a -> Maybe a
lookup name env = do
    (Zip _ m _) <- findScope name env
    M.lookup name m
