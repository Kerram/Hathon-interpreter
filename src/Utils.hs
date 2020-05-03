module Utils where

import System.Environment


getProgramPath :: IO String
getProgramPath = do
  args <- getArgs
  let len = length args in
    if len /= 1
      then error $ "ERROR: Intepreter got " ++ (show $ len) ++ " arguments, but it expected exactly 1!"
      else return (head args)
