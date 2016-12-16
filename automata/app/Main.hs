module Main where

import qualified Ex1 as E1
import qualified Ex2 as E2

main :: IO ()
main = do
  print "First example: "
  E1.main
  putStrLn ""
  print "Second example: "
  E2.main
  putStrLn ""
