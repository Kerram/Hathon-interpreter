module Utils where

import System.Environment


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

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (h:[]) = True
allSame (h:h2:t) = (h == h2) && allSame (h2:t)
