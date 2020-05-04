module InterpreterTypes where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map as M

import Syntax.AbsSyntax

type Error a = Either ShowS a
type HathonFunction = ExpValue -> Error ExpValue -- Hathon is a name of the language.
type Env = M.Map Ident (Error ExpValue)
type ProgMonad = ReaderT Env (ExceptT ShowS IO) ()

-- Cannot use Error instead of Either String without extensions,
-- because we cannot partially apply type synonyms.
type ExpMonad = ReaderT Env (Either ShowS) ExpValue


data ExpValue = IntValue Integer
                | BoolValue Bool
                | FunValue HathonFunction
                | ListValue [ExpValue]


showSExpValue :: ExpValue -> Error ShowS
showSExpValue (IntValue value) = return $ shows value
showSExpValue (BoolValue value) = return $ shows value
showSExpValue (FunValue _) = Left $ showString "ERROR: Cannot display functional type!"
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
