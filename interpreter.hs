import System.Environment
import System.IO
import Control.Monad.Except

import Syntax.ParSyntax (pProg, myLexer)
import Syntax.AbsSyntax


evalExp :: Exp () -> Integer
evalExp (EAdd _ exp1 exp2) = (evalExp exp1) + (evalExp exp2)
evalExp (EDiv _ exp1 exp2) = (evalExp exp1) `div` (evalExp exp2)
evalExp (EMul _ exp1 exp2) = (evalExp exp1) * (evalExp exp2)
evalExp (EInt _ x) = x


evalProgram :: Prog () -> Integer
evalProgram (PEmpty _) = 0
evalProgram (PDef _ _ _) = 0
evalProgram (PExp _ exp1 _) = evalExp exp1

interpret :: String -> IO ()
interpret programString = do
  case pProg (myLexer programString) of
    Left err -> print err
    Right tree -> print $ evalProgram tree




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
