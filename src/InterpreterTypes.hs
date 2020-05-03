module InterpreterTypes where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M

import Syntax.AbsSyntax

type Error a = Either String a
type HathonFunction = ExpValue -> Error ExpValue -- Hathon is a name of the language.
type Env = M.Map Ident (Error ExpValue)
type ProgMonad = ReaderT Env (ExceptT String IO) ()

-- Cannot use Error instead of Either String without extensions,
-- because we cannot partially apply type synonyms.
type ExpMonad = ReaderT Env (Either String) ExpValue


data ExpValue = IntValue Integer
                | BoolValue Bool
                | FunValue HathonFunction
                | ListValue [ExpValue]


showSExpValue :: ExpValue -> Error ShowS
showSExpValue (IntValue value) = return $ shows value
showSExpValue (BoolValue value) = return $ shows value
showSExpValue (FunValue _) = Left $ "ERROR: Cannot display functional type!"
showSExpValue (ListValue value) = do
  innerRepr <- help value
  return $ (showString "[") . (innerRepr) . (showString "]")
  where
    help :: [ExpValue] -> Error ShowS
    help [] = return $ showString ""
    help (h:t) = do
      headRepr <- showSExpValue h
      tailRepr <- help t
      return $ headRepr . showString ", " . tailRepr
