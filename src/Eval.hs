module Eval where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M

import Syntax.AbsSyntax

import InterpreterTypes
import Utils


lambdaName :: Ident
lambdaName = Ident "__builtin_lambda_name"

evalProgram :: Prog (Maybe (Int, Int)) -> ProgMonad
evalProgram (PEmpty _) = return ()

evalProgram (PDef _ def@(DFun _ name _ _ _) prog) = do
  env <- ask
  definition <- liftEither $ runReaderT (evalDef True def) env
  local (M.insert name $ Right definition) (evalProgram prog)

evalProgram (PExp pos expr prog) = do
  env <- ask
  value <- liftEither $ runReaderT (evalExp expr) env
  case showSExpValue value of
    Left errorMsg -> liftEither $
      Left $ addPosInfoToErr (errorMsg . showString "\nThis error occured during printing value of expression") pos
    Right valueRepr -> do
      liftIO $ putStrLn (valueRepr "")
      evalProgram prog


evalDef :: Bool -> Def (Maybe (Int, Int)) -> ExpMonad
evalDef enableRecursion def@(DFun pos name (TFun _ paramType returnType) (param:paramsTail) expr) = do
  env <- ask
  if enableRecursion then local (M.insert name $ runReaderT (evalDef True def) env) partialApplication
                     else partialApplication
  where
    partialApplication = do
      env2 <- ask
      return $ FunValue (\paramValue -> runReaderT (do
          local (M.insert param $ Right paramValue) (evalDef False (DFun pos name returnType paramsTail expr))
            ) env2
          )

evalDef enableRecursion def@(DFun _ name _ _ expr) = do
  env <- ask
  if enableRecursion then local (M.insert name $ runReaderT (evalDef True def) env) (evalExp expr)
                     else evalExp expr


evalExp :: Exp (Maybe (Int, Int)) -> ExpMonad
evalExp (EIf _ condition thenExp elseExp) = do
  packedTrueOrFalse <- evalExp condition; let (BoolValue trueOrFalse) = packedTrueOrFalse
  if trueOrFalse then
    evalExp thenExp
  else
    evalExp elseExp

evalExp (ELet _ def@(DFun _ name _ _ _) expr) = do
  env <- ask
  local (M.insert name $ runReaderT (evalDef True def) env) (evalExp expr)

evalExp (ELambda pos lambdaType args expr) =
  evalDef False (DFun pos lambdaName lambdaType args expr)

evalExp (EOr _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (BoolValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (BoolValue value2) = packedValue2
  return $ BoolValue (value1 || value2)

evalExp (EAnd _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (BoolValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (BoolValue value2) = packedValue2
  return $ BoolValue (value1 && value2)

evalExp (EEqu _ exp1 exp2) = do
  value1 <- evalExp exp1
  value2 <- evalExp exp2
  equal <- liftEither $ expValueEq value1 value2
  return $ BoolValue equal

evalExp (EGre _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ BoolValue (value1 > value2)

evalExp (EGeq _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ BoolValue (value1 >= value2)

evalExp (ELes _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ BoolValue (value1 < value2)

evalExp (ELeq _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ BoolValue (value1 <= value2)

evalExp (ENeq _ exp1 exp2) = do
  value1 <- evalExp exp1
  value2 <- evalExp exp2
  equal <- liftEither $ expValueEq value1 value2
  return $ BoolValue (not equal)

evalExp (ELAppend _ exp1 exp2) = do
  packedValue1 <- evalExp exp1
  packedValue2 <- evalExp exp2; let (ListValue value2) = packedValue2
  return $ ListValue $ packedValue1:value2

evalExp (ESub _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ IntValue (value1 - value2)

evalExp (EAdd _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ IntValue (value1 + value2)

evalExp (EDiv pos exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  if value2 == 0 then
    liftEither $ Left $ addPosInfoToErr (showString "ERROR: Division by 0") pos
  else
    return $ IntValue (value1 `div` value2)

evalExp (EMul _ exp1 exp2) = do
  packedValue1 <- evalExp exp1; let (IntValue value1) = packedValue1
  packedValue2 <- evalExp exp2; let (IntValue value2) = packedValue2
  return $ IntValue (value1 * value2)

evalExp (EBNeg _ expr) = do
  packedValue <- evalExp expr; let (BoolValue value) = packedValue
  return $ BoolValue (not value)

evalExp (ENeg _ expr) = do
  packedValue <- evalExp expr; let (IntValue value) = packedValue
  return $ IntValue (-value)

evalExp (EInt _ x) = return $ IntValue x
evalExp (ETrue _) = return $ BoolValue True
evalExp (EFalse _) = return $ BoolValue False

evalExp (EList _ list) = do
  elements <- mapM (\expr -> evalExp expr) list
  return $ ListValue elements

evalExp (EVar pos (Ident name)) = do
  env <- ask
  case M.lookup (Ident name) env of
    Nothing -> liftEither $ Left $ addPosInfoToErr (showString "ERROR: Identifier " . shows name . showString " not declared") pos
    Just value -> liftEither value

-- TODO code duplication
evalExp (EApp pos (Ident fName) args@(ArgList _ expr _)) = do
  env <- ask
  case M.lookup (Ident fName) env of
    Nothing -> liftEither $ Left $ addPosInfoToErr (showString "ERROR: Identifier " . shows fName . showString " not declared") pos
    Just (Left errorMsg) -> liftEither $ Left $ addPosInfoToErr (errorMsg . showString "\nThis error occured during function application") pos
    Just (Right (FunValue fun)) -> applyArgs pos fun args

evalExp (EApp pos (Ident fName) args@(ArgBase _ expr)) = do
  env <- ask
  case M.lookup (Ident fName) env of
    Nothing -> liftEither $ Left $ addPosInfoToErr (showString "ERROR: Identifier " . shows fName . showString " not declared") pos
    Just (Left errorMsg) -> liftEither $ Left $ addPosInfoToErr (errorMsg . showString "\nThis error occured during function application") pos
    Just (Right (FunValue fun)) -> applyArgs pos fun args


applyArgs :: Maybe(Int, Int) -> HathonFunction -> Args (Maybe (Int, Int)) -> ExpMonad
applyArgs pos fun (ArgList _ expr args) = do
  paramValue <- evalExp expr
  packedPartialApp <- liftEither $ fun paramValue; let (FunValue partialApp) = packedPartialApp
  applyArgs pos partialApp args

applyArgs pos fun (ArgBase _ expr) = do
  paramValue <- evalExp expr
  case fun paramValue of
    Left errorMsg -> liftEither $ Left $ addPosInfoToErr (errorMsg . showString "\nThis error occured during function application") pos
    Right returnValue -> liftEither $ Right returnValue
