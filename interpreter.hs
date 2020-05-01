import System.Environment
import System.IO
import Control.Monad.Except

import Syntax.ParSyntax (pProg, myLexer)
import Syntax.AbsSyntax


interpret :: String -> IO ()
interpret programString = do
  case pProg (myLexer programString) of
    Left err -> print err
    Right tree -> print "SUccess!"




getProgramPath :: IO String
getProgramPath = do
  args <- getArgs
  case args of
    [] -> error "ERROR: Interpreter got 0 arguments, but it expected exactly 1!"
    l@(h:h2:t) -> error $ "ERROR: Intepreter got " ++ (show $ length l) ++ " arguments, but it expected exactly 1!"
    (h:t) -> return h

main :: IO ()
main = do
  programPath <- getProgramPath
  withFile programPath ReadMode (\handle -> do
    program <- hGetContents handle
    interpret program)
