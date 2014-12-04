module Main where

import Grid
import Language
import Engine

import Control.Monad
import Control.Monad.Loops

main :: IO ()
main = do
  putStrLn $ showGame game1
  unless (isLegalProgram soluce1 [5, 4]) $ putStrLn "oops... soluce1 is not a legal program"
  let f = runRobo (iterateUntilM isSolved step game1) soluce1
  putStrLn $ showGame f

