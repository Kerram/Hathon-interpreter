module Predefined where

import qualified Data.Map as M

import Syntax.AbsSyntax

import InterpreterTypes


predefinedHead :: HathonFunction
predefinedHead (ListValue (h:t)) = Right h

predefinedEmpty :: HathonFunction
predefinedEmpty (ListValue []) = Right (BoolValue True)
predefinedEmpty (ListValue (h:t)) = Right (BoolValue False)

predefinedTail :: HathonFunction
predefinedTail (ListValue (h:t)) = Right (ListValue t)

predefinedEnv :: Env
predefinedEnv = M.fromList [(Ident "head", Right $ FunValue predefinedHead),
                            (Ident "empty", Right $ FunValue predefinedEmpty),
                            (Ident "tail", Right $ FunValue predefinedTail)]
