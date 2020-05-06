module TypeChecker.TypeChecking where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M

import Syntax.AbsSyntax

import TypeChecker.Types
import TypeChecker.Utils

import Utils


getUnaryOpType :: Maybe (Int, Int) ->
                  Exp (Maybe (Int, Int)) ->
                  HathonType ->
                  HathonType ->
                  TypeExpMonad
getUnaryOpType pos expr argType retType = do
  expType <- getExpType expr
  case hTypeCouldBeEq expType argType of
    True -> return retType
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Operand's type mismatch. Expected: \"" .
      shows argType . showString "\", but found: " . shows expType . showString "\"") pos


getBinaryOpType :: Maybe (Int, Int) ->
             Exp (Maybe (Int, Int)) ->
             HathonType ->
             Exp (Maybe (Int, Int)) ->
             HathonType ->
             HathonType ->
             TypeExpMonad
getBinaryOpType pos exp1 type1 exp2 type2 retType = do
 expType1 <- getExpType exp1
 expType2 <- getExpType exp2
 case (hTypeCouldBeEq expType1 type1) && (hTypeCouldBeEq expType2 type2) of
   True -> return retType
   False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Operands' types mismatch. Expected: \"" .
     shows type1 . showString "\" and \"" . shows type2 . showString "\", but found: \"" .
     shows expType1 . showString "\" and \"" . shows expType2 . showString "\"") pos


getBoolOpType :: Maybe (Int, Int) ->
                 Exp (Maybe (Int, Int)) ->
                 Exp (Maybe (Int, Int)) ->
                 TypeExpMonad
getBoolOpType pos exp1 exp2 = getBinaryOpType pos exp1 BoolType exp2 BoolType BoolType


getComparisionOpType :: Maybe (Int, Int) ->
                        Exp (Maybe (Int, Int)) ->
                        Exp (Maybe (Int, Int)) ->
                        TypeExpMonad
getComparisionOpType pos exp1 exp2 = getBinaryOpType pos exp1 IntType exp2 IntType BoolType


getArithmOpType :: Maybe (Int, Int) ->
                   Exp (Maybe (Int, Int)) ->
                   Exp (Maybe (Int, Int)) ->
                   TypeExpMonad
getArithmOpType pos exp1 exp2 = getBinaryOpType pos exp1 IntType exp2 IntType IntType


getEqualityType :: Maybe (Int, Int) ->
                   Exp (Maybe (Int, Int)) ->
                   Exp (Maybe (Int, Int)) ->
                   TypeExpMonad
getEqualityType pos exp1 exp2 = do
  expType1 <- getExpType exp1
  expType2 <- getExpType exp2
  case hTypeCouldBeEq expType1 expType2 of
    False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot check equality between values of different types: \"" .
      shows expType1 . showString "\" and \"" . shows expType2 . showString "\"") pos
    True -> case (containsFunctionalType expType1) || (containsFunctionalType expType2) of
      True -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot check equality on types containing functional types: \"" .
        shows expType1 . showString "\" and \"" . shows expType2 . showString "\"") pos
      False -> return BoolType


checkTypes :: Prog (Maybe (Int, Int)) -> TCM
checkTypes (PEmpty _) = return ()

checkTypes (PExp pos expr prog) = do
  expType <- getExpType expr
  case containsFunctionalType expType of
    True -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot print value of type \"" .
      shows expType . showString "\"") pos
    False -> checkTypes prog

checkTypes (PDef _ def@(DFun _ name _ _ _) prog) = do
  defType <- getAndCheckDefType True def
  local (M.insert name defType) (checkTypes prog)


getAndCheckDefType :: Bool -> Def (Maybe (Int, Int)) -> TypeExpMonad
getAndCheckDefType enableRecursion (DFun pos name (TFun _ argType retType) (arg:argsTail) expr) = do
    if enableRecursion then
      local (M.insert name (FunType argHType $ convertToHType retType)) partialApp
    else
      partialApp
    where
      argHType = convertToHType argType
      partialApp = local (M.insert arg argHType) (do
        retHType <- getAndCheckDefType False (DFun pos name retType argsTail expr)
        return $ (FunType argHType retHType)
        )

getAndCheckDefType enableRecursion (DFun pos name defType [] expr) = do
  let defHType = convertToHType defType
  expType <- if enableRecursion then
               local (M.insert name defHType) (getExpType expr)
             else
               getExpType expr
  case hTypeCouldBeEq defHType expType of
    True -> return $ defHType
    False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Definition type (with applied arguments) \"" .
      shows defHType . showString "\" mismatch with its body type \"" . shows expType . showString "\"") pos

getAndCheckDefType _ (DFun pos _ _ _ _) = do
  liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Too many arguments in definition") pos


getExpType :: Exp (Maybe (Int, Int)) -> TypeExpMonad
getExpType (EIf pos condition thenExp elseExp) = do
  condType <- getExpType condition
  case condType of
    BoolType -> do
      thenType <- getExpType thenExp
      elseType <- getExpType elseExp
      case hTypeCouldBeEq thenType elseType of
        False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Return expressions in if statement are not of equal types. First one is of type \"" .
          shows thenType . showString "\" and second is of type \"" . shows elseType . showString "\"") pos
        True -> return $ getMostConcreteType [thenType, elseType]
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Condition in if statement has type \"" .
        shows condType . showString "\", but expected boolean type") pos

getExpType (ELet _ def@(DFun _ name _ _ _) expr) = do
  defType <- getAndCheckDefType True def
  local (M.insert name defType) (getExpType expr)

getExpType (ELambda pos lambdaType args expr) =
  getAndCheckDefType False (DFun pos lambdaName lambdaType args expr)

getExpType (EOr pos exp1 exp2) = getBoolOpType pos exp1 exp2

getExpType (EAnd pos exp1 exp2) = getBoolOpType pos exp1 exp2

getExpType (EEqu pos exp1 exp2) = getEqualityType pos exp1 exp2

getExpType (EGre pos exp1 exp2) = getComparisionOpType pos exp1 exp2

getExpType (EGeq pos exp1 exp2) = getComparisionOpType pos exp1 exp2

getExpType (ELes pos exp1 exp2) = getComparisionOpType pos exp1 exp2

getExpType (ELeq pos exp1 exp2) = getComparisionOpType pos exp1 exp2

getExpType (ENeq pos exp1 exp2) = getEqualityType pos exp1 exp2

getExpType (ELAppend pos exp1 exp2) = do
  type1 <- getExpType exp1
  type2 <- getExpType exp2
  if not (hTypeCouldBeEq type2 (ListType type1)) then
    liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Append expected types: a and [a], but found \"" .
      shows type1 . showString "\" and \"" . shows type2 . showString "\"") pos
  else
    case type2 of
      EmptyList -> return $ ListType type1
      ListType innerType2 -> return $ ListType (getMostConcreteType [type1, innerType2])

getExpType (ESub pos exp1 exp2) = getArithmOpType pos exp1 exp2

getExpType (EAdd pos exp1 exp2) = getArithmOpType pos exp1 exp2

getExpType (EDiv pos exp1 exp2) = getArithmOpType pos exp1 exp2

getExpType (EMul pos exp1 exp2) = getArithmOpType pos exp1 exp2

getExpType (EMod pos exp1 exp2) = getArithmOpType pos exp1 exp2

getExpType (EBNeg pos expr) = getUnaryOpType pos expr BoolType BoolType

getExpType (ENeg pos expr) = getUnaryOpType pos expr IntType IntType

getExpType (EInt _ _) = return IntType
getExpType (ETrue _) = return BoolType
getExpType (EFalse _) = return BoolType
getExpType (EList _ []) = return EmptyList

getExpType (EList pos elems) = do
  listOfTypes <- mapM getExpType elems
  case allPairsEqual hTypeCouldBeEq listOfTypes of
    False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Heterogeneous lists are not allowed, but found one") pos
    True -> return $ ListType (getMostConcreteType listOfTypes)

getExpType (EVar pos (Ident name)) = do
  env <- ask
  case M.lookup (Ident name) env of
    Nothing -> liftEither $ Left $
      addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot calculate type of " . shows name . showString ", because it is undeclared") pos
    Just hType -> return hType

getExpType (EApp pos (Ident name) args) = do
  env <- ask
  case M.lookup (Ident name) env of
    Nothing -> liftEither $ Left $
      addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot calculate type of " . shows name . showString ", because it is undeclared") pos
    Just hType -> getTypeAfterApplication hType args


getTypeAfterApplication :: HathonType -> Args (Maybe (Int, Int)) -> TypeExpMonad
getTypeAfterApplication HeadFunType args = do
  let pos = getPosFromArgs args
  expType <- getExpType (getHeadFromArgs args)
  case expType of
    EmptyList -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot deduce type, because builtin head function was applied to an empty list") pos
    ListType hType -> case getTailFromArgs args of
                    Nothing -> return hType
                    Just argsTail -> getTypeAfterApplication hType argsTail
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Builtin function head was applied to a non list type") pos

getTypeAfterApplication EmptyFunType args = do
  let pos = getPosFromArgs args
  expType <- getExpType (getHeadFromArgs args)
  case expType of
    EmptyList -> partialApp
    ListType _ -> partialApp
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Builtin function empty was applied to a non list type") pos
  where
    partialApp = case getTailFromArgs args of
                  Nothing -> return BoolType
                  Just argsTail -> getTypeAfterApplication BoolType argsTail

getTypeAfterApplication TailFunType args = do
  let pos = getPosFromArgs args
  expType <- getExpType (getHeadFromArgs args)
  case expType of
    EmptyList -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot deduce type, because tail function was applied to an empty list") pos
    ListType _ -> case getTailFromArgs args of
                    Nothing -> return expType
                    Just argsTail -> getTypeAfterApplication expType argsTail
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Builtin function tail was applied to non list type") pos

getTypeAfterApplication (FunType argType retType) args = do
  let pos = getPosFromArgs args
  expType <- getExpType (getHeadFromArgs args)
  case hTypeCouldBeEq argType expType of
    True -> case getTailFromArgs args of
              Nothing -> return retType
              Just argsTail -> getTypeAfterApplication retType argsTail
    False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Type mismatch during function application, expected \"" .
      shows argType . showString "\", but found \"" . shows expType . showString "\"") pos

getTypeAfterApplication hType args = do
  liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Trying to apply value of type \"" .
    shows hType . showString "\", which is not a functional type") (getPosFromArgs args)
