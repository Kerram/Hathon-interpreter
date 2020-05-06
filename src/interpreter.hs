module Main where

import System.IO
import Control.Monad.Except
import Control.Monad.Reader

import Syntax.ParSyntax (pProg, myLexer)
import Syntax.ErrM

import Predefined
import Eval
import Utils
import TypeChecker.TypeChecking
import TypeChecker.Predefined
import StackTrace


interpret :: String -> IO ()
interpret program = do
  case pProg (myLexer program) of
    Bad errorMsg -> hPutStrLn stderr errorMsg
    Ok tree -> do
      case runReaderT (checkTypes tree) predefinedTypeEnv of
        Left errorMsg -> hPutStrLn stderr (errorMsg "")
        Right _ -> do
          result <- runExceptT $ runReaderT (evalProgram tree) predefinedEnv
          case result of
            Left stackTrace -> hPutStrLn stderr (stackTraceToString stackTrace)
            Right _ -> return ()

main :: IO ()
main = do
  programPath <- getProgramPath
  withFile programPath ReadMode (\handle -> do
    program <- hGetContents handle
    interpret program)
