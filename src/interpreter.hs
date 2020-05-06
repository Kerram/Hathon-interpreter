module Main where

import Control.Monad.Except
import Control.Monad.Reader
import System.IO

import Syntax.ErrM
import Syntax.ParSyntax (pProg, myLexer)

import TypeChecker.Predefined
import TypeChecker.TypeChecking

import Eval
import Predefined
import StackTrace
import Utils


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
  packedProgramPath <- runExceptT getProgramPath
  case packedProgramPath of
    Left errorMsg -> hPutStrLn stderr errorMsg
    Right programPath -> withFile programPath ReadMode (\handle -> do
      program <- hGetContents handle
      interpret program)
