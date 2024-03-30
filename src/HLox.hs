module HLox (
    run, runRepl, runFile
) where

import qualified Control.Monad.Except as E
import Control.Monad.Trans (lift)
import System.IO
import System.IO.Error
import System.Exit

import HLox.Control.Base (HLox, runHLox)
import qualified HLox.Control.Scanner as Scanner
import qualified HLox.Control.Parser as Parser
import qualified HLox.Control.Interpreter as Interpreter

run :: String -> HLox ()
run code = do
    tokens <- Scanner.lex code
    expr <- Parser.parseExpr tokens
    value <- Interpreter.interpret expr
    lift $ print value

runRepl :: IO ()
runRepl = do
    putStrLn "Welcome to the hlox repl."
    putStrLn "Press Ctrl+D to exit."
    _ <- runHLox repl
    return ()

repl :: HLox a
repl = do
    lift $ putStr "hlox> " >> hFlush stdout
    input <- lift $ catchIOError getLine (\e -> if isEOFError e then exitSuccess else hPrint stderr e >> exitFailure)
    E.catchError (run input) (\_ -> return ())
    repl

runFile :: FilePath -> IO ()
runFile file = do
    code <- readFile file
    _ <- runHLox $ E.catchError (run code) (\_ -> lift $ exitWith (ExitFailure 65))
    return ()
