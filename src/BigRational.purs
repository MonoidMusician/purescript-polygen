module Data.Rational.Big where

import Prelude
import Data.Ord (signum)
import Data.Ratio (Ratio(..))
import Data.Ratio as Ratio
import Data.Rational as Rational
import Data.Rational (Rational)
import Data.Rational.Farey as Farey
import Data.BigInt as BigInt
import Data.BigInt (BigInt)
import Data.Int as Int

newtype BigRational = BigRational (Ratio BigInt)

mkBigRational :: Ratio BigInt -> BigRational
mkBigRational = reduce <<< BigRational

runBigRational :: BigRational -> Ratio BigInt
runBigRational (BigRational ratio) = ratio

instance showBigRational :: Show BigRational where
  show (BigRational (Ratio a b)) = show a <> " % " <> show b

instance eqBigRational :: Eq BigRational where
  eq x y = eq' (reduce x) (reduce y)
    where
    eq' (BigRational (Ratio a' b')) (BigRational (Ratio c' d')) = a' == c' && b' == d'

instance ordBigRational :: Ord BigRational where
  compare (BigRational x) (BigRational y) = case x / y of Ratio a b -> compare a b

instance semiringBigRational :: Semiring BigRational where
  one = BigRational one
  mul (BigRational a) (BigRational b) = mkBigRational $ a `mul` b
  zero = BigRational zero
  add (BigRational a) (BigRational b) = mkBigRational $ a `add` b

instance ringBigRational :: Ring BigRational where
  sub (BigRational a) (BigRational b) = mkBigRational $ a `sub` b

instance commutativeRingBigRational :: CommutativeRing BigRational

instance euclideanRingBigRational :: EuclideanRing BigRational where
  degree (BigRational a) = degree a
  div (BigRational a) (BigRational b) = BigRational $ a `div` b
  mod _ _ = BigRational zero

instance fieldBigRational :: Field BigRational

infixl 7 rational as %

rational :: BigInt -> BigInt -> BigRational
rational x y = mkBigRational $ Ratio x y

toNumber :: BigRational -> Number
toNumber (BigRational (Ratio a b)) = BigInt.toNumber a / BigInt.toNumber b

fromInt :: Int -> BigRational
fromInt i = BigRational $ Ratio (BigInt.fromInt i) one

fromBigInt :: BigInt -> BigRational
fromBigInt i = BigRational $ Ratio i one

fromRational :: Rational -> BigRational
fromRational r = case Rational.runRational r of
  Ratio n d -> mkBigRational $ Ratio (BigInt.fromInt n) (BigInt.fromInt d)

farey :: Int -> Number -> BigRational
farey n = Farey.farey n >>> fromRational

fromNumber :: Number -> BigRational
fromNumber = Farey.fromNumber >>> fromRational

reduce :: BigRational -> BigRational
reduce (BigRational (Ratio a b)) =
  let g = gcd a b
      b' = b / g
  in BigRational $ Ratio ((a / g) * signum b') (BigInt.abs b')
