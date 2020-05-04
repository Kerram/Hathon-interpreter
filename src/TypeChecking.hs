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

instance Show HathonType where
  showsPrec _ IntType = showString "Int"
  showsPrec _ BoolType = showString "Bool"
  showsPrec _ (FunType arg ret) = showString "(" . showsPrec 0 arg . showString ") => " . showsPrec 0 ret
  showsPrec _ (ListType listType) = showString "[" . showsPrec 0 listType . showString "]"

type TypeEnv = M.Map Ident HathonType

-- Type checking monad
type TCM = ReaderT TypeEnv (Either ShowS) ()
type TypeExpMonad = ReaderT TypeEnv (Either ShowS) HathonType


containsFunctionalType :: HathonType -> Bool
containsFunctionalType IntType = False
containsFunctionalType BoolType = False
containsFunctionalType (FunType _ _) = True
containsFunctionalType (ListType listType) = containsFunctionalType listType

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
