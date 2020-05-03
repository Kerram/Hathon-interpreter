module Main where

import System.IO
import Control.Monad.Except
import Control.Monad.Reader

import Syntax.ParSyntax (pProg, myLexer)
import Syntax.ErrM

import Predefined
import Eval
import Utils


interpret :: String -> IO ()
interpret programString = do
  case pProg (myLexer programString) of
    Bad err -> print err
    Ok tree -> do
      result <- runExceptT $ runReaderT (evalProgram tree) predefinedEnv
      case result of
        Left err -> print err
        Right _ -> return ()

main :: IO ()
main = do
  programPath <- getProgramPath
  withFile programPath ReadMode (\handle -> do
    program <- hGetContents handle
    interpret program)
