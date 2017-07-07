module Data.Rational.Farey where

import Prelude

import Data.Int (floor, round)
import Data.Int (toNumber, fromNumber) as Int
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Ratio (denominator, numerator)
import Data.Rational (Rational, fromInt, runRational, (%))
import Data.Rational (toNumber) as Rational
import Math (remainder, round) as Math

-- | Calculates the mediant or Farey addition of two rationals:
-- |     mediant (a % b) (c % d) = (a+b) % (c+d)
mediant :: Rational -> Rational -> Rational
mediant l r = top % bottom
  where
    l' = runRational l
    r' = runRational r
    top = numerator l' + numerator r'
    bottom = denominator l' + denominator r'

-- | The algorithm using Farey sequences for best `Rational` approximations to a
-- | floating-point `Number`, based on implementations such as [1].
-- |
-- | [1] https://github.com/alidasdan/best-rational-approximation
farey :: Int -> Number -> Rational
-- integers make nice rationals
farey _ 0.0 = zero
farey _ 1.0 = one
farey _ x
  | Just n <- Int.fromNumber x =
    fromInt n
-- if it is close enough to an int for the precision gained, go straight to that
farey n x
  | abs (Math.round x - x) < 1.0/Int.toNumber n =
    fromInt (round x)
-- the actual Farey algorithm
farey n x =
    -- add the base integer to the rational result and restore the sign
    signum x * (fromInt t + go zero one)
  where
    -- use the absolute remainder to get 0 < x' < 1
    x' = Math.remainder (abs x) 1.0
    -- store the integer, to add back in
    t = floor (abs x)
    -- ensure that n is not too high to cause issues with the numerator
    -- (if the denominator reaches top and t > 1, the numerator would overflow)
    n' = if t > 0 then min n (top / t / 2) else n
    -- rational denominator
    den = runRational >>> denominator
    -- error term for a rational approximation
    error r = x' - Rational.toNumber r
    minBy :: forall a b. Ord b => (a -> b) -> a -> a -> a
    minBy score l r =
      if score l < score r
      then l
      else r
    -- return the closest rational
    best = minBy (abs <<< error)

    go l r | den l > n' = r
    go l r | den r > n' = l
    go l r =
      let m = mediant l r in
      case error m `compare` 0.0 of
        EQ | den m <= n' -> m
        EQ -> best l r
        -- might not be necessary, but check both fractions before actually
        -- reaching overflow
        _ | den m > n' ->
          best l r
        GT -> go m r
        LT -> go l m

-- | A very high precision default for the max denominator for farey. Note that
-- | for numbers >= 4.0 this limit will not be reached, in order to prevent the
-- | numerator from overflowing.
defaultN :: Int
defaultN = top / 4

-- | Returns a rational approximation to the number with denominator less than
-- | `defaultN`.
fromNumber :: Number -> Rational
fromNumber = farey defaultN

-- | Generalized signum with different input and output rings.
signum :: forall a b. Ord a => Ring a => Ring b => a -> b
signum x = if x >= zero then one else negate one
