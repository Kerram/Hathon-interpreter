module TypeChecker.Predefined where

import qualified Data.Map as M

import Syntax.AbsSyntax

import TypeChecker.Types


predefinedTypeEnv = M.fromList [(Ident "head", HeadFunType),
                                (Ident "empty", EmptyFunType),
                                (Ident "tail", TailFunType)]
