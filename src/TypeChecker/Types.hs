module TypeChecker.Types where

import Control.Monad.Reader
import qualified Data.Map as M

import Syntax.AbsSyntax


type TypeEnv = M.Map Ident HathonType

-- Type checking monad
type TCM = ReaderT TypeEnv (Either ShowS) ()
type TypeExpMonad = ReaderT TypeEnv (Either ShowS) HathonType

-- We don't use Type a from AbsSyntax, because we don't need to
-- store information about positions in the source code and we need
-- more types.
data HathonType = IntType
                  | BoolType
                  | FunType HathonType HathonType
                  | ListType HathonType
                  | EmptyList
                  | HeadFunType -- [a] -> a
                  | EmptyFunType -- [a] -> Bool
                  | TailFunType -- [a] -> [a]

-- This may be not 100% correct implementation of Show, but it is enough for our needs.
instance Show HathonType where
  showsPrec _ IntType = showString "Int"
  showsPrec _ BoolType = showString "Bool"
  showsPrec _ (ListType listType) = showString "[" . showsPrec 0 listType . showString "]"
  showsPrec _ EmptyList = showString "[]"
  showsPrec _ HeadFunType = showString "__builtin_head_function_type"
  showsPrec _ EmptyFunType = showString "__builtin_empty_function_type"
  showsPrec _ TailFunType = showString "__builtin_tail_function_type"
  showsPrec 11 (FunType arg ret) = showString "(" . showsPrec 11 arg . showString  " -> " . showsPrec 0 ret . showString ")"
  showsPrec _ (FunType arg ret) = showsPrec 11 arg . showString " -> " . showsPrec 0 ret


-- Watch out! This relation is not transitive, because of non concrete types:
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
