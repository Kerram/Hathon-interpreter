module TypeChecking where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M

import Syntax.AbsSyntax

import Utils


-- We don't use Type a from AbsSyntax, because we don't need to
-- store information about positions in the source code.
data HathonType = IntType
                  | BoolType
                  | FunType HathonType HathonType
                  | ListType HathonType
                  | EmptyList
                  | HeadFunType -- [a] -> a
                  | EmptyFunType -- [a] -> Bool
                  | TailFunType -- [a] -> [a]

instance Show HathonType where
  showsPrec _ IntType = showString "Int"
  showsPrec _ BoolType = showString "Bool"
  showsPrec _ (FunType arg ret) = showString "(" . showsPrec 0 arg . showString ") => " . showsPrec 0 ret
  showsPrec _ (ListType listType) = showString "[" . showsPrec 0 listType . showString "]"
  showsPrec _ EmptyList = showString "[]"
  showsPrec _ HeadFunType = showString "__builtin_head_function_type"
  showsPrec _ EmptyFunType = showString "__builtin_empty_function_type"
  showsPrec _ TailFunType = showString "__builtin_tail_function_type"

-- Watch out! This relation is not transitive, because of empty lists:
-- [] == [Int] and [] == [Bool], but [Int] /= [Bool].
hTypeEq :: HathonType -> HathonType -> Bool
hTypeEq IntType IntType = True
hTypeEq BoolType BoolType = True
hTypeEq (FunType arg1 ret1) (FunType arg2 ret2) = (hTypeEq arg1 arg2) && (hTypeEq ret1 ret2)
hTypeEq EmptyList (ListType _) = True
hTypeEq (ListType _) EmptyList = True
hTypeEq EmptyList EmptyList = True
hTypeEq (ListType type1) (ListType type2) = hTypeEq type1 type2
hTypeEq HeadFunType HeadFunType = True
hTypeEq TailFunType TailFunType = True
hTypeEq EmptyFunType EmptyFunType = True
hTypeEq HeadFunType (FunType (ListType innerArg) ret) = hTypeEq innerArg ret
hTypeEq EmptyFunType (FunType (ListType _) BoolType) = True
hTypeEq TailFunType (FunType (ListType arg) (ListType ret)) = hTypeEq arg ret
hTypeEq (FunType (ListType innerArg) ret) HeadFunType = hTypeEq innerArg ret
hTypeEq (FunType (ListType _) BoolType) EmptyFunType = True
hTypeEq (FunType (ListType arg) (ListType ret)) TailFunType = hTypeEq arg ret
hTypeEq _ _ = False


type TypeEnv = M.Map Ident HathonType

-- Type checking monad
type TCM = ReaderT TypeEnv (Either ShowS) ()
type TypeExpMonad = ReaderT TypeEnv (Either ShowS) HathonType




predefinedTypeEnv = M.fromList [(Ident "head", HeadFunType),
                                (Ident "empty", EmptyFunType),
                                (Ident "tail", TailFunType)]




-- Returns Nothing, if the given type is concrete.
-- Otherwise returns number of nested lists until we reach empty list or
-- predefined polymorphic function type.
countLevel :: HathonType -> Maybe Int
countLevel EmptyList = Just 0
countLevel HeadFunType = Just 0
countLevel EmptyFunType = Just 0
countLevel TailFunType = Just 0
countLevel IntType = Nothing
countLevel BoolType = Nothing
countLevel (FunType _ _) = Nothing
countLevel (ListType listType) = do
  level <- countLevel listType
  return $ level + 1

getMostConcreteType :: [HathonType] -> HathonType
getMostConcreteType types =
  let typesWithLevels = map (\htype -> (countLevel htype, htype)) types
  in
    getMostConcreteTypeWithLevels (0, snd $ head typesWithLevels) typesWithLevels
    where
      getMostConcreteTypeWithLevels :: (Int, HathonType) -> [(Maybe Int, HathonType)] -> HathonType
      getMostConcreteTypeWithLevels (_, maxType) [] = maxType
      getMostConcreteTypeWithLevels _ ((Nothing, concreteType):_) = concreteType
      getMostConcreteTypeWithLevels (maxLevel, maxType) ((Just level, htype):t) =
        if maxLevel <= level then
          getMostConcreteTypeWithLevels (level, htype) t
        else
          getMostConcreteTypeWithLevels (maxLevel, maxType) t

containsFunctionalType :: HathonType -> Bool
containsFunctionalType IntType = False
containsFunctionalType BoolType = False
containsFunctionalType (FunType _ _) = True
containsFunctionalType (ListType listType) = containsFunctionalType listType
containsFunctionalType EmptyList = False
containsFunctionalType HeadFunType = True
containsFunctionalType EmptyFunType = True
containsFunctionalType TailFunType = True


convertToHType :: Type (Maybe (Int, Int)) -> HathonType
convertToHType (TInt _) = IntType
convertToHType (TBool _) = BoolType
convertToHType (TList _ listType) = ListType $ convertToHType listType
convertToHType (TFun _ argType retType) = FunType (convertToHType argType) (convertToHType retType)




checkTypes :: Prog (Maybe (Int, Int)) -> TCM
checkTypes (PEmpty _) = return ()

checkTypes (PExp pos expr prog) = do
  expType <- getExpType expr
  case containsFunctionalType expType of
    True -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot print value of type <" . shows expType . showString ">") pos
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
  case hTypeEq defHType expType of
    True -> return $ defHType
    False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Definition type (with applied arguments) <" .
      shows defHType . showString "> mismatch with its body type <" . shows expType . showString ">") pos

getAndCheckDefType _ (DFun pos _ _ _ _) = do
  liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Too many arguments in definition") pos




getExpType :: Exp (Maybe (Int, Int)) -> TypeExpMonad
getExpType (EInt _ _) = return IntType
getExpType (ETrue _) = return BoolType
getExpType (EFalse _) = return BoolType
getExpType (EList _ []) = return EmptyList

getExpType (EList pos elems) = do
  listOfTypes <- mapM getExpType elems
  case allPairsEqual hTypeEq listOfTypes of
    False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Heterogeneous lists are not allowed, but found one") pos
    True -> return $ ListType (getMostConcreteType listOfTypes)

getExpType (EAdd pos exp1 exp2) = do
  type1 <- getExpType exp1
  case type1 of
    IntType -> do
      type2 <- getExpType exp2
      case type2 of
        IntType -> return IntType
        _ -> liftEither $ Left $
              addPosInfoToErr (showString "TYPECHECKING ERROR: Second argument for addition must be of type Int, but it is of type <" .
              shows type2 . showString ">. Error occured") pos
    _ -> liftEither $ Left $
          addPosInfoToErr (showString "TYPECHECKING ERROR: First argument for addition must be of type Int, but it is of type <" .
          shows type1 . showString ">. Error occured") pos

getExpType (EDiv pos exp1 exp2) = do
  type1 <- getExpType exp1
  case type1 of
    IntType -> do
      type2 <- getExpType exp2
      case type2 of
        IntType -> return IntType
        _ -> liftEither $ Left $
              addPosInfoToErr (showString "TYPECHECKING ERROR: Second argument for division must be of type Int, but it is of type <" .
              shows type2 . showString ">. Error occured") pos
    _ -> liftEither $ Left $
          addPosInfoToErr (showString "TYPECHECKING ERROR: First argument for division must be of type Int, but it is of type <" .
          shows type1 . showString ">. Error occured") pos

getExpType (ELAppend pos exp1 exp2) = do
  type1 <- getExpType exp1
  type2 <- getExpType exp2
  if not (hTypeEq type2 (ListType type1)) then
    liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Append expected types: a and [a], but found <" .
      shows type1 . showString "> and <" . shows type2 . showString ">") pos
  else
    case type2 of
      EmptyList -> return $ ListType type1
      ListType innerType2 -> return $ ListType (getMostConcreteType [type1, innerType2])

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
getTypeAfterApplication HeadFunType (ArgBase pos expr) = do
  expType <- getExpType expr
  case expType of
    EmptyList -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot deduce type, because builtin head function was applied to an empty list") pos
    ListType hType -> return $ hType
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Builtin function head was applied to a non list type") pos

getTypeAfterApplication EmptyFunType (ArgBase pos expr) = do
  expType <- getExpType expr
  case expType of
    EmptyList -> return BoolType
    ListType hType -> return BoolType
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Builtin function empty was applied to a non list type") pos

getTypeAfterApplication TailFunType (ArgBase pos expr) = do
  expType <- getExpType expr
  case expType of
    EmptyList -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot deduce type, because tail function was applied to an empty list") pos
    ListType _ -> return $ expType
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Builtin function tail was applied to non list type") pos

getTypeAfterApplication (FunType argType retType) (ArgBase pos expr) = do
  expType <- getExpType expr
  case hTypeEq argType expType of
    True -> return retType
    False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Type mismatch during function application, expected <" .
      shows argType . showString ">, but found <" . shows expType . showString ">") pos





getTypeAfterApplication HeadFunType (ArgList pos expr args) = do
  expType <- getExpType expr
  case expType of
    EmptyList -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot deduce type, because builtin head function was applied to an empty list") pos
    ListType hType -> getTypeAfterApplication hType args
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Builtin function head was applied to a non list type") pos

-- To się zawsze wywala
getTypeAfterApplication EmptyFunType (ArgList pos expr args) = do
  expType <- getExpType expr
  case expType of
    EmptyList -> getTypeAfterApplication BoolType args
    ListType hType -> getTypeAfterApplication BoolType args
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Builtin function empty was applied to a non list type") pos

getTypeAfterApplication TailFunType (ArgList pos expr args) = do
  expType <- getExpType expr
  case expType of
    EmptyList -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot deduce type, because tail function was applied to an empty list") pos
    ListType _ -> getTypeAfterApplication expType args
    _ -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Builtin function tail was applied to non list type") pos
-- koniec wywalania








getTypeAfterApplication (FunType argType retType) (ArgList pos expr args) = do
  expType <- getExpType expr
  case hTypeEq argType expType of
    True -> getTypeAfterApplication retType args
    False -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Type mismatch during function application, expected <" .
      shows argType . showString ">, but found <" . shows expType . showString ">") pos

getTypeAfterApplication hType (ArgList pos _ _) = do
  liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Trying to apply value of type <" .
    shows hType . showString ">, which is not a functional type") pos

getTypeAfterApplication hType (ArgBase pos _) = do
  liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Trying to apply value of type <" .
    shows hType . showString ">, which is not a functional type") pos
