module Predefined where

import qualified Data.Map as M

import Syntax.AbsSyntax

import InterpreterTypes
import StackTrace


predefinedHead :: HathonFunction
predefinedHead (ListValue []) = Left $ newST $
  showString "RUNTIME ERROR: Function head cannot be applied to an empty list!"
predefinedHead (ListValue (h:_)) = Right h

predefinedEmpty :: HathonFunction
predefinedEmpty (ListValue []) = Right $ BoolValue True
predefinedEmpty (ListValue _) = Right $ BoolValue False

predefinedTail :: HathonFunction
predefinedTail (ListValue []) = Left $ newST $
  showString "RUNTIME ERROR: Function tail cannot be applied to an empty list!"
predefinedTail (ListValue (_:t)) = Right (ListValue t)

predefinedEnv :: Env
predefinedEnv = M.fromList [(Ident "head", Right $ FunValue predefinedHead),
                            (Ident "empty", Right $ FunValue predefinedEmpty),
                            (Ident "tail", Right $ FunValue predefinedTail)]
