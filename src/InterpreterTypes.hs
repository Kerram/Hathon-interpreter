module InterpreterTypes where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M

import Syntax.AbsSyntax

import StackTrace


type Error a = Either StackTrace a
type HathonFunction = ExpValue -> Error ExpValue -- Hathon is a name of the language.
type Env = M.Map Ident (Error ExpValue)
type ProgMonad = ReaderT Env (ExceptT StackTrace IO) ()

-- Cannot use Error instead of Either StackTrace without extensions,
-- because we cannot partially apply type synonyms.
type ExpMonad = ReaderT Env (Either StackTrace) ExpValue


data ExpValue = IntValue Integer
                | BoolValue Bool
                | FunValue HathonFunction
                | ListValue [ExpValue]

showSExpValue :: ExpValue -> Error ShowS
showSExpValue (IntValue value) = return $ shows value
showSExpValue (BoolValue value) = return $ shows value
showSExpValue (FunValue _) = Left $ newST $ showString "RUNTIME ERROR: Cannot display functional type!"
showSExpValue (ListValue value) = do
  innerRepr <- showSListOfExpValue value
  return $ (showString "[") . (innerRepr) . (showString "]")
  where
    showSListOfExpValue :: [ExpValue] -> Error ShowS
    showSListOfExpValue [] = return $ showString ""
    showSListOfExpValue (h:[]) = showSExpValue h
    showSListOfExpValue (h:t) = do
      headRepr <- showSExpValue h
      tailRepr <- showSListOfExpValue t
      return $ headRepr . showString ", " . tailRepr

expValueEq :: ExpValue -> ExpValue -> Error Bool
expValueEq (IntValue val1) (IntValue val2) = Right $ val1 == val2
expValueEq (BoolValue val1) (BoolValue val2) = Right $ val1 == val2
expValueEq (FunValue _) (FunValue _) = Left $ newST $ showString "RUNTIME ERROR: Cannot check equality between functional types!"
expValueEq (ListValue val1) (ListValue val2) = do
  if length val1 /= length val2 then
    return False
  else
    do
      intermediateResults <- zipWithM expValueEq val1 val2
      return $ foldr (&&) True intermediateResults
expValueEq _ _ = Left $ newST $ showString "RUNTIME ERROR: Cannot check equality between objects of different types!"
