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




predefinedHead :: ExpValue -> Either String ExpValue
predefinedHead (ListValue (h:t)) = Right h

predefinedEmpty :: ExpValue -> Either String ExpValue
predefinedEmpty (ListValue []) = Right (BoolValue True)
predefinedEmpty (ListValue (h:t)) = Right (BoolValue False)

predefinedTail :: ExpValue -> Either String ExpValue
predefinedTail (ListValue (h:t)) = Right (ListValue t)


predefinedEnv :: Env
predefinedEnv = M.fromList [(Ident "head", Right $ FunValue predefinedHead),
                            (Ident "empty", Right $ FunValue predefinedEmpty),
                            (Ident "tail", Right $ FunValue predefinedTail)]



type Env = M.Map Ident (Either String ExpValue)



data ExpValue = IntValue Integer
                | BoolValue Bool
                | FunValue (ExpValue -> Either String ExpValue)
                | ListValue [ExpValue]


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
evalExp (EFalse _) = return $ BoolValue False
evalExp (EVar _ name) = do
  env <- ask
  case M.lookup name env of
    Nothing -> liftEither $ Left $ "ERROR: Variable" ++ (show name) ++ "not declared"
    Just value -> liftEither value

evalExp (EApp _ fName args@(ArgList _ expr _)) = do
  env <- ask
  case M.lookup fName env of
    Nothing -> liftEither $ Left $ "ERROR: Variable" ++ (show fName) ++ "not declared"
    Just (Right (FunValue fun)) -> applyArgs fun args

evalExp (EApp _ fName args@(ArgBase _ expr)) = do
  env <- ask
  case M.lookup fName env of
    Nothing -> liftEither $ Left $ "ERROR: Variable" ++ (show fName) ++ "not declared"
    Just (Right (FunValue fun)) -> applyArgs fun args

evalExp (ELet _ def@(DFun _ name _ _ _) expr) = do
  env <- ask
  local (M.insert name $ runReaderT (evalDef True def) env) (evalExp expr)

evalExp (EList _ list) = do
  elements <- mapM (\expr -> evalExp expr) list
  return $ ListValue elements

evalExp (EIf _ condition thenExp elseExp) = do
  goOrNo <- evalExp condition; let (BoolValue unpackedGoOrNo) = goOrNo
  if unpackedGoOrNo then
    evalExp thenExp
  else
    evalExp elseExp

evalExp (EGre _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ BoolValue (value1 > value2)


evalExp (ELAppend _ exp1 exp2) = do
  packedValue1 <- evalExp exp1
  packedValue2 <- evalExp exp2; let (ListValue value2) = packedValue2
  return $ ListValue $ packedValue1:value2

evalExp (EEqu _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ BoolValue (value1 == value2)

applyArgs :: (ExpValue -> Either String ExpValue) -> Args (Maybe (Int, Int)) -> ReaderT Env (Either String) ExpValue
applyArgs fun (ArgList _ expr args) = do
  paramValue <- evalExp expr
  fun2 <- liftEither $ fun paramValue; let (FunValue unpackedFun) = fun2
  applyArgs unpackedFun args

applyArgs fun (ArgBase _ expr) = do
  paramValue <- evalExp expr
  liftEither $ fun paramValue


evalDef :: Bool -> Def (Maybe (Int, Int)) -> ReaderT Env (Either String) ExpValue
evalDef outer def@(DFun _ name (TFun _ paramType returnType) (param:rest) expr) = do
  env <- ask
  local (\mapa -> if outer then (M.insert name $ runReaderT (evalDef True def) env) mapa else mapa) (do
    env2 <- ask
    return $ FunValue (\paramValue -> runReaderT (do
        local (M.insert param $ Right paramValue) (evalDef False (DFun (Just (1, 1)) name returnType rest expr))
          ) env2
        )
    )

evalDef outer def@(DFun _ name _ _ expr) = do
  env <- ask
  if outer then local (M.insert name $ runReaderT (evalDef True def) env) (evalExp expr)
           else evalExp expr


evalProgram :: Prog (Maybe (Int, Int)) -> ReaderT Env (ExceptT String IO) ()
evalProgram (PEmpty _) = return ()
evalProgram (PDef _ def@(DFun _ name _ _ _) prog) = do
  env <- ask
  definition <- liftEither $ runReaderT (evalDef True def) env
  local (M.insert name $ Right definition) (evalProgram prog)

evalProgram (PExp _ exp1 prog) = do
  env <- ask
  value <- liftEither $ runReaderT (evalExp exp1) env
  case value of
    IntValue x -> liftIO $ print x
    BoolValue x -> liftIO $ print x
    FunValue _ -> liftEither $ Left $ "ERROR: Cannot display value of functional type!"
    ListValue x -> liftIO $ print "List"
  evalProgram prog

interpret :: String -> IO ()
interpret programString = do
  case pProg (myLexer programString) of
    Bad err -> print err
    Ok tree -> do
      result <- runExceptT $ runReaderT (evalProgram tree) predefinedEnv
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
