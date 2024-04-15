module HLox (
    run, runRepl, runFile
) where

import qualified Control.Monad.Except as E
import Control.Monad.Trans (lift)
import Control.Monad (void)
import System.IO
import System.IO.Error
import System.Exit

import HLox.Control.Base (runHLox)
import qualified HLox.Control.Scanner as Scanner
import qualified HLox.Control.Parser as Parser
import qualified HLox.Control.Interpreter as Interpreter
import HLox.Control.Interpreter (Runtime, withRuntime, liftHLox)

run :: String -> Runtime ()
run code = do
    tokens <- liftHLox $ Scanner.lex code
    prog <- liftHLox $ Parser.parseProgram tokens
    Interpreter.runProgram prog

runRepl :: IO ()
runRepl = do
    putStrLn "Welcome to the hlox repl."
    putStrLn "Press Ctrl+D to exit."
    void . runHLox $ withRuntime repl

prompt :: IO String
prompt = do
    putStr "hlox> "
    hFlush stdout
    catchIOError getLine (\e ->
        if isEOFError e then exitSuccess else hPrint stderr e >> exitFailure)

repl :: Runtime a
repl = do
    input <- liftHLox.lift $ prompt
    E.catchError (run input) (\_ -> return ())
    repl

runFile :: FilePath -> IO ()
runFile file = do
    code <- readFile file
    void . runHLox $ E.catchError (withRuntime $ run code) (\_ -> lift $ exitWith (ExitFailure 65))
