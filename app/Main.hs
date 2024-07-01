module Main (main) where

import System.Environment

import HLox (prettyPrint, runRepl, runFile)

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = usage
parseArgs ("-p":xs) = prettyPrint (unwords xs)
parseArgs [] = runRepl
parseArgs [filename] = runFile filename
parseArgs _ = usage

usage :: IO ()
usage = putStrLn "usage: hlox [script] | hlox -p <expression>"