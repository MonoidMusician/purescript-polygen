module Test.Main where

import Prelude
import Test.Matrix (testMatrix)
import Test.QuickCheck (QC)

main :: QC () Unit
main = do
  testMatrix
