module Utils where

import System.Environment

getProgramPath :: IO String
getProgramPath = do
  args <- getArgs
  case args of
    [] -> error "ERROR: Interpreter got 0 arguments, but it expected exactly 1!"
    l@(h:h2:t) -> error $ "ERROR: Intepreter got " ++ (show $ length l) ++ " arguments, but it expected exactly 1!"
    (h:t) -> return h
