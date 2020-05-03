module InterpreterTypes where

import qualified Data.Map as M

import Syntax.AbsSyntax


type Env = M.Map Ident (Either String ExpValue)

data ExpValue = IntValue Integer
                | BoolValue Bool
                | FunValue (ExpValue -> Either String ExpValue)
                | ListValue [ExpValue]


showSExpValue :: ExpValue -> Either String ShowS
showSExpValue (IntValue value) = return $ shows value
showSExpValue (BoolValue value) = return $ shows value
showSExpValue (FunValue _) = Left $ "ERROR: Cannot display functional type!"
showSExpValue (ListValue value) = do
  innerRepr <- help value
  return $ (showString "[") . (innerRepr) . (showString "]")
  where
    help :: [ExpValue] -> Either String ShowS
    help [] = return $ showString ""
    help (h:t) = do
      headRepr <- showSExpValue h
      tailRepr <- help t
      return $ headRepr . showString ", " . tailRepr
