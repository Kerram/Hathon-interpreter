import System.Environment
import System.IO
import Control.Monad.Except
import Data.Maybe

import Syntax.ParSyntax (pProg, myLexer)
import Syntax.AbsSyntax
import Syntax.ErrM


data ExpValue = IntValue Integer | BoolValue Bool


evalExp :: Exp (Maybe (Int, Int)) -> Either String ExpValue
evalExp (EAdd _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ IntValue (value1 + value2)

evalExp (EDiv position exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  if value2 == 0 then
    let (line, col) = fromJust position in
    Left $ "ERROR: You cannot divide by 0!!!" ++ "Line: " ++ (show line) ++ "Column: " ++ (show col)
  else
    return $ IntValue (value1 `div` value2)

evalExp (EMul _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ IntValue (value1 * value2)

evalExp (EInt _ x) = return $ IntValue x
evalExp (ETrue _) = return $ BoolValue True


evalProgram :: Prog (Maybe (Int, Int)) -> ExceptT String IO ()
evalProgram (PEmpty _) = return ()
evalProgram (PDef _ _ _) = return ()
evalProgram (PExp _ exp1 prog) = do
  value <- liftEither $ evalExp exp1
  case value of
    IntValue x -> liftIO $ print x
    BoolValue x -> liftIO $ print x
  evalProgram prog

interpret :: String -> IO ()
interpret programString = do
  case pProg (myLexer programString) of
    Bad err -> print err
    Ok tree -> do
      result <- runExceptT (evalProgram tree)
      case result of
        Left err -> print err
        Right _ -> return ()




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
