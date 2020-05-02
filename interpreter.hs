import System.Environment
import System.IO
import Control.Monad.Except
import Data.Maybe
import Control.Monad.Reader

import Syntax.ParSyntax (pProg, myLexer)
import Syntax.AbsSyntax
import Syntax.ErrM

import qualified Data.Map as M
import Data.Either


type Env = M.Map Ident (Either String ExpValue)



data ExpValue = IntValue Integer | BoolValue Bool | FunValue (ExpValue -> Either String ExpValue)


evalExp :: Exp (Maybe (Int, Int)) -> ReaderT Env (Either String) ExpValue
evalExp (EAdd _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ IntValue (value1 + value2)

evalExp (EDiv position exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  if value2 == 0 then
    let (line, col) = fromJust position in
    liftEither $ Left $ "ERROR: You cannot divide by 0!!!" ++ "Line: " ++ (show line) ++ "Column: " ++ (show col)
  else
    return $ IntValue (value1 `div` value2)

evalExp (EMul _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ IntValue (value1 * value2)

evalExp (EInt _ x) = return $ IntValue x
evalExp (ETrue _) = return $ BoolValue True
evalExp (EVar _ name) = do
  env <- ask
  case M.lookup name env of
    Nothing -> liftEither $ Left $ "ERROR: Variable" ++ (show name) ++ "not declared"
    Just value -> liftEither value

evalExp (EApp _ fName (ArgBase _ expr)) = do
  env <- ask
  case M.lookup fName env of
    Nothing -> liftEither $ Left $ "ERROR: Variable" ++ (show fName) ++ "not declared"
    Just (Right (FunValue fun)) -> do
      paramValue <- evalExp expr
      liftEither $ fun paramValue


evalDef :: Def (Maybe (Int, Int)) -> ReaderT Env (Either String) ExpValue
evalDef def@(DFun _ name (TFun _ paramType returnType) (param:rest) expr) = do
  env <- ask
  local (M.insert name $ runReaderT (evalDef def) env) (do
    env2 <- ask
    return $ FunValue (\paramValue -> runReaderT (do
        local (M.insert param $ Right paramValue) (evalDef (DFun (Just (1, 1)) name returnType rest expr))
          ) env2
        )
    )

evalDef def@(DFun _ name _ _ expr) = do
  env <- ask
  local (M.insert name $ runReaderT (evalDef def) env) (evalExp expr)



evalProgram :: Prog (Maybe (Int, Int)) -> ReaderT Env (ExceptT String IO) ()
evalProgram (PEmpty _) = return ()
evalProgram (PDef _ def@(DFun _ name _ _ _) prog) = do
  env <- ask
  definition <- liftEither $ runReaderT (evalDef def) env
  local (M.insert name $ Right definition) (evalProgram prog)

evalProgram (PExp _ exp1 prog) = do
  env <- ask
  value <- liftEither $ runReaderT (evalExp exp1) env
  case value of
    IntValue x -> liftIO $ print x
    BoolValue x -> liftIO $ print x
  evalProgram prog

interpret :: String -> IO ()
interpret programString = do
  case pProg (myLexer programString) of
    Bad err -> print err
    Ok tree -> do
      result <- runExceptT $ runReaderT (evalProgram tree) M.empty
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
