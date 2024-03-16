module Main (main) where

import System.Environment
import System.Exit
import System.IO
import System.IO.Error

import HLox.Scanner as Scanner

run :: String -> IO ()
run code = print (Scanner.lex code)

runRepl :: IO ()
runRepl = do
    putStrLn "Welcome to the hlox repl."
    putStrLn "Press Ctrl+D to exit."
    repl

repl :: IO a
repl = do
    putStr "hlox> "
    hFlush stdout
    input <- catchIOError getLine (\e -> if isEOFError e then exitSuccess else putStrLn "Error reading input" >> repl)
    run input
    repl

runFile :: FilePath -> IO ()
runFile file = readFile file >>= run

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = usage
parseArgs [] = runRepl
parseArgs [filename] = runFile filename
parseArgs _ = usage

usage :: IO ()
usage = putStrLn "usage: hlox [script]"