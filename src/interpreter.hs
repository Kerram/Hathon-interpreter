module Main where

import System.IO
import Control.Monad.Except
import Control.Monad.Reader

import Syntax.ParSyntax (pProg, myLexer)
import Syntax.ErrM

import Predefined
import Eval
import Utils
import TypeChecking


import qualified Data.Map as M


interpret :: String -> IO ()
interpret program = do
  case pProg (myLexer program) of
    Bad errorMsg -> hPutStrLn stderr errorMsg
    Ok tree -> do
      case runReaderT (checkTypes tree) M.empty of
        Left errorMsg -> hPutStrLn stderr (errorMsg "")
        Right _ -> do
          result <- runExceptT $ runReaderT (evalProgram tree) predefinedEnv
          case result of
            Left errorMsg -> hPutStrLn stderr (errorMsg "")
            Right _ -> return ()

main :: IO ()
main = do
  programPath <- getProgramPath
  withFile programPath ReadMode (\handle -> do
    program <- hGetContents handle
    interpret program)
