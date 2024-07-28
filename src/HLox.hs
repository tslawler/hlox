module HLox (
    run, runRepl, runFile, parseFile, prettyPrint
) where

import qualified Control.Monad.Except as E
import Control.Monad.Trans (lift)
import Control.Monad (void)
import System.IO
import System.IO.Error
import System.Exit

import HLox.Data.Stmt (Stmt)
import HLox.Control.Base (HLox(), runHLox)
import qualified HLox.Control.Scanner as Scanner
import qualified HLox.Control.Parser as Parser
import qualified HLox.Control.Pretty as Pretty
import qualified HLox.Control.Interpreter as Interpreter
import HLox.Control.Interpreter (Runtime, withRuntime, liftHLox)

run :: String -> Runtime ()
run code = do
    prog <- liftHLox $ parse code
    Interpreter.runProgram prog

parse :: String -> HLox [Stmt]
parse code = do
    tokens <- Scanner.lex code
    Parser.parseProgram tokens

prettyPrint :: String -> IO ()
prettyPrint code = flip E.catchError (\_ -> exitWith (ExitFailure 65)) . void . runHLox $ do
    tokens <- Scanner.lex code
    expr <- Parser.parseExpr tokens
    lift $ Pretty.print expr

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

parseFile :: FilePath -> IO ()
parseFile file = do
    code <- readFile file
    result <- runHLox (parse code)
    print result

runFile :: FilePath -> IO ()
runFile file = do
    code <- readFile file
    void . runHLox $ E.catchError (withRuntime $ run code) (\_ -> lift $ exitWith (ExitFailure 65))
