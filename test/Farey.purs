module Test.Farey where

import Data.Rational
import Data.Rational.Farey
import Prelude
import Math
import Data.Array as A
import Data.NonEmpty as NE

test' n x = "|" <> show x <> " - " <> show x' <> "| = " <> show (abs (x - x')) where x' = toNumber (farey n x)
genAll x lower upper = map NE.head (A.group (map (\n -> farey n x) (A.range lower upper)))
golden = (1.0 + sqrt(5.0))/2.0
