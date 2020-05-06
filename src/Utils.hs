module Utils where

import Control.Monad.Except
import System.Environment

import Syntax.AbsSyntax


helpLine :: String
helpLine = "Help: To run interpreter type ./interpreter {filePath}, \
  \where {filePath} is a path to file containing source code in Hathon."

lambdaName :: Ident
lambdaName = Ident "__builtin_lambda_name"

getProgramPath :: (ExceptT String IO) String
getProgramPath = do
  args <- liftIO getArgs
  let len = length args
  if len /= 1 then
    liftEither $ Left $ "ERROR: Intepreter got " ++ (show len) ++
      " arguments, but it expected exactly 1!\n" ++ helpLine
  else
    return $ head args

addPosInfoToErr :: ShowS -> Maybe (Int, Int) -> ShowS
addPosInfoToErr errorMsg Nothing = errorMsg . showString " at position <unknown>."
addPosInfoToErr errorMsg (Just (line, col)) = errorMsg . showString " at line:" .
  shows line . showString ", column:" . shows col . showString "."

allPairsEqual :: (a -> a -> Bool) -> [a] -> Bool
allPairsEqual eqFun [] = True
allPairsEqual eqFun (h:t) =
  case foldr (\elem acc -> acc && (eqFun elem h)) True t of
    False -> False
    True -> allPairsEqual eqFun t

getPosFromArgs :: Args (Maybe (Int, Int)) -> Maybe (Int, Int)
getPosFromArgs (ArgBase pos _) = pos
getPosFromArgs (ArgList pos _ _) = pos

getHeadFromArgs :: Args (Maybe (Int, Int)) -> Exp (Maybe (Int, Int))
getHeadFromArgs (ArgBase _ expr) = expr
getHeadFromArgs (ArgList _ expr _) = expr

getTailFromArgs :: Args (Maybe (Int, Int)) -> Maybe (Args (Maybe (Int, Int)))
getTailFromArgs (ArgBase _ _) = Nothing
getTailFromArgs (ArgList _ _ args) = Just args
