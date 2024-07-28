module Main (main) where

import System.Environment

import HLox (prettyPrint, runRepl, runFile, parseFile)

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = usage
parseArgs ("-p":xs) = prettyPrint (unwords xs)
parseArgs ["--parse", filename] = parseFile filename
parseArgs [] = runRepl
parseArgs [filename] = runFile filename
parseArgs _ = usage

usage :: IO ()
usage = putStrLn "usage: hlox [script] | hlox -p <expression> | hlox --parse script"