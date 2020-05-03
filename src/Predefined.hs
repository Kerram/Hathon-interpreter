module Predefined where

import qualified Data.Map as M

import Syntax.AbsSyntax

import InterpreterTypes


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
