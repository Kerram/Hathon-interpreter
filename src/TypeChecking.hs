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

instance Show HathonType where
  showsPrec _ IntType = showString "Int"
  showsPrec _ BoolType = showString "Bool"
  showsPrec _ (FunType arg ret) = showString "(" . showsPrec 0 arg . showString ") => " . showsPrec 0 ret
  showsPrec _ (ListType listType) = showString "[" . showsPrec 0 listType . showString "]"
  showsPrec _ EmptyList = showString "[]"

instance Eq HathonType where
  IntType == IntType = True
  BoolType == BoolType = True
  (FunType arg1 ret1) == (FunType arg2 ret2) = (arg1 == arg2) && (ret1 == ret2)
  EmptyList == (ListType _) = True
  (ListType _) == EmptyList = True
  EmptyList == EmptyList = True
  (ListType type1) == (ListType type2) = type1 == type2
  _ == _ = False


type TypeEnv = M.Map Ident HathonType

-- Type checking monad
type TCM = ReaderT TypeEnv (Either ShowS) ()
type TypeExpMonad = ReaderT TypeEnv (Either ShowS) HathonType


-- Returns Nothing, if the given type is concrete.
-- Otherwise returns number of nested lists until we reach empty list.
countLevel :: HathonType -> Maybe Int
countLevel EmptyList = Just 0
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

checkTypes :: Prog (Maybe (Int, Int)) -> TCM
checkTypes (PEmpty _) = return ()

checkTypes (PExp pos expr prog) = do
  expType <- getExpType expr
  case containsFunctionalType expType of
    True -> liftEither $ Left $ addPosInfoToErr (showString "TYPECHECKING ERROR: Cannot print value of type <" . shows expType . showString ">") pos
    False -> checkTypes prog


getExpType :: Exp (Maybe (Int, Int)) -> TypeExpMonad
getExpType (EInt _ _) = return IntType
getExpType (ETrue _) = return BoolType
getExpType (EFalse _) = return BoolType
getExpType (EList _ []) = return EmptyList

getExpType (EList pos elems) = do
  listOfTypes <- mapM getExpType elems
  case allSame listOfTypes of
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
