module TypeChecker.Types where

import Control.Monad.Reader
import qualified Data.Map as M

import Syntax.AbsSyntax


type TypeEnv = M.Map Ident HathonType
type TCM = ReaderT TypeEnv (Either ShowS) () -- Type checking monad
type TypeExpMonad = ReaderT TypeEnv (Either ShowS) HathonType

-- Hathon is a name of the language.
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
hTypeCouldBeEq :: HathonType -> HathonType -> Bool
hTypeCouldBeEq IntType IntType = True
hTypeCouldBeEq BoolType BoolType = True
hTypeCouldBeEq (FunType arg1 ret1) (FunType arg2 ret2) = (hTypeCouldBeEq arg1 arg2) && (hTypeCouldBeEq ret1 ret2)
hTypeCouldBeEq EmptyList (ListType _) = True
hTypeCouldBeEq (ListType _) EmptyList = True
hTypeCouldBeEq EmptyList EmptyList = True
hTypeCouldBeEq (ListType type1) (ListType type2) = hTypeCouldBeEq type1 type2
hTypeCouldBeEq HeadFunType HeadFunType = True
hTypeCouldBeEq TailFunType TailFunType = True
hTypeCouldBeEq EmptyFunType EmptyFunType = True
hTypeCouldBeEq HeadFunType (FunType (ListType innerArg) ret) = hTypeCouldBeEq innerArg ret
hTypeCouldBeEq EmptyFunType (FunType (ListType _) BoolType) = True
hTypeCouldBeEq TailFunType (FunType (ListType arg) (ListType ret)) = hTypeCouldBeEq arg ret
hTypeCouldBeEq (FunType (ListType innerArg) ret) HeadFunType = hTypeCouldBeEq innerArg ret
hTypeCouldBeEq (FunType (ListType _) BoolType) EmptyFunType = True
hTypeCouldBeEq (FunType (ListType arg) (ListType ret)) TailFunType = hTypeCouldBeEq arg ret
hTypeCouldBeEq _ _ = False
