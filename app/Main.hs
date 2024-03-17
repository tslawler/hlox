module Main (main) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans
import System.Environment
import System.Exit
import System.IO
import System.IO.Error (isEOFError)

import HLox.Control.Scanner as Scanner
import HLox.Control.Parser as Parser
import HLox (HLox(), runHLox, hadError, clearError)

run :: String -> HLox ()
run code = do
    tokens <- Scanner.lex code
    b <- hadError
    unless b $ do
        expr <- Parser.parseExpr tokens
        b <- hadError
        unless b $ do
            lift $ print expr

runRepl :: IO ()
runRepl = do
    putStrLn "Welcome to the hlox repl."
    putStrLn "Press Ctrl+D to exit."
    runHLox repl

repl :: HLox a
repl = do
    lift $ putStr "hlox> "
    lift $ hFlush stdout
    input <- catchIOError (lift getLine) (\e -> if isEOFError e then lift $ exitSuccess else lift (putStrLn "Error reading input") >> repl)
    run input
    b <- hadError
    when b clearError
    repl

runFile :: FilePath -> IO ()
runFile file = do
    code <- readFile file
    runHLox $ do
        run code
        b <- hadError
        when b (lift $ exitWith (ExitFailure 65))

main :: IO ()
main = getArgs >>= parseArgs

parseArgs :: [String] -> IO ()
parseArgs ["-h"] = usage
parseArgs [] = runRepl
parseArgs [filename] = runFile filename
parseArgs _ = usage

usage :: IO ()
usage = putStrLn "usage: hlox [script]"