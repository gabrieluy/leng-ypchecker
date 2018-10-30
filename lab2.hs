import System.Environment (getArgs)
import System.Exit (exitFailure)

import AbsPascal
import LexPascal
import ParPascal
import ErrM

import TypeChecker

check :: String -> IO ()
check s = do
  case pProgram (myLexer s) of
    Bad err  -> do
      putStrLn "SYNTAX ERROR"
      putStrLn err
      exitFailure
    Ok tree -> do
      case typeCheck tree of
        Bad err -> do
          putStrLn "TYPE ERROR"
          putStrLn err
          exitFailure
        Ok tree -> do
          -- putStrLn (show tree)
          putStrLn "OK"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> readFile file >>= check
    _      -> do
      putStrLn "Usage: lab2 <SourceFile>"
      exitFailure
