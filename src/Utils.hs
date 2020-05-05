module Utils where

import System.Environment

import Syntax.AbsSyntax


lambdaName :: Ident
lambdaName = Ident "__builtin_lambda_name"

getProgramPath :: IO String
getProgramPath = do
  args <- getArgs
  let len = length args in
    if len /= 1
      then error $ "ERROR: Intepreter got " ++ (show $ len) ++ " arguments, but it expected exactly 1!"
      else return (head args)

addPosInfoToErr :: ShowS -> Maybe (Int, Int) -> ShowS
addPosInfoToErr errorMsg Nothing = errorMsg . showString " at position <unknown>."
addPosInfoToErr errorMsg (Just (line, col)) =
  errorMsg . showString " at line:" . shows line . showString ", column:" . shows col . showString "."

allPairsEqual :: (a -> a -> Bool) -> [a] -> Bool
allPairsEqual eqFun [] = True
allPairsEqual eqFun (h:t) =
  case foldr (\elem acc -> acc && (eqFun elem h)) True t of
    False -> False
    True -> allPairsEqual eqFun t
