module Control.Monad.Combinators.Expr (
    makeExprParser, Operations(), infixL, infixR, prefix, postfix, infixL', infixR', prefix', postfix'
) where

import Control.Monad
import Control.Applicative

makeExprParser :: (MonadPlus m) => m a -> [Operations m a] -> m a
makeExprParser = foldl addPrecLevel

data Operations m a
    = InfixL' (m (a -> a -> m a))
    | InfixR' (m (a -> a -> m a))
    | Prefix (m (a -> m a))
    | Postfix (m (a -> m a))

infixL, infixR :: (MonadPlus m) => [m (a -> a -> a)] -> Operations m a
infixL ops = InfixL' (fmap (\g x y -> return (g x y)) (asum ops))
infixR ops = InfixR' (fmap (\g x y -> return (g x y)) (asum ops))
infixL', infixR' :: (MonadPlus m) => [m (a -> a -> m a)] -> Operations m a
infixL' ops = InfixL' (asum ops)
infixR' ops = InfixR' (asum ops)
prefix, postfix :: (MonadPlus m) => [m (a -> a)] -> Operations m a
prefix ops = Prefix (fmap (\g x -> return (g x)) (asum ops))
postfix ops = Postfix (fmap (\g x -> return (g x)) (asum ops))
prefix', postfix' :: (MonadPlus m) => [m (a -> m a)] -> Operations m a
prefix' ops = Prefix (asum ops)
postfix' ops = Postfix (asum ops)

addPrecLevel :: (MonadPlus m) => m a -> Operations m a -> m a
addPrecLevel term (Prefix ps) = goP <|> term
    where 
        goP = do
            op <- ps
            y <- goP <|> term
            op y
addPrecLevel term (Postfix ps) = term >>= \x -> asum [goP x, return x]
    where
        goP x = do
            op <- ps
            y <- op x
            goP y <|> return y
addPrecLevel term (InfixL' ls) = term >>= \x -> asum [goL' x, return x]
    where
        goL' x = do
            op <- ls
            y <- term
            r <- op x y
            goL' r <|> return r
addPrecLevel term (InfixR' rs) = term >>= \x -> asum [goR' x, return x]
    where
        goR' x = do
            op <- rs
            y <- (term >>= \r -> goR' r <|> return r)
            op x y
