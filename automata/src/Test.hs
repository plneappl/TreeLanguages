module Test where

import Data.List
import Control.Monad

calc = sort $ do
  let target = 1.45
  let stamps = [0.45, 0.45, 0.45, 0.45, 0.58, 0.7, 0.7]
  subStamps <- nub $ subsequences stamps
  let postage = sum subStamps
  guard $ postage >= target
  return (postage, subStamps)

main :: IO ()
main = print calc
