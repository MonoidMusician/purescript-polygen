module Test.Matrix where

import Prelude
import Main.Matrix
import Data.Array as A
import Data.Foldable (sum)
import Data.Int (toNumber)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Math (abs, round)
import Test.QuickCheck (class Arbitrary, QC, Result(..), arbitrary, quickCheck, quickCheck', (/==), (<?>), (===))
import Test.QuickCheck.Gen (Gen, chooseInt, vectorOf)

checkInverse :: Matrix Number -> Matrix Number
checkInverse m = matProduct m (inverse m)

checkDeterminant :: Matrix Number -> Boolean
checkDeterminant m = d == det
  where
    cfm = cofactorM m
    d = determinant m
    det = sum $ A.zipWith (*) (getTop cfm) (getTop m)

matrixOf :: forall a. Arbitrary a => Int -> Int -> Gen (Matrix a)
matrixOf rows cols = do
  Matrix <$> sequence (A.replicate rows $ vectorOf cols $ arbitrary :: Gen a)

sqmatrixOf :: forall a. Arbitrary a => Int -> Gen (Matrix a)
sqmatrixOf size = matrixOf size size

mint :: Array (Array Int) -> Matrix Number
mint = map toNumber <<< mkMatrix
m2 :: Matrix Number
m2 = mint [[3,4],[2,3]]
mall :: Matrix Number
mall = mint [[1,2,3],[4,5,6],[7,8,9]]
cftest :: Matrix Number
cftest = mint [[1,2,3],[0,4,5],[1,0,6]]
m6 :: Matrix Number
m6 = mint [[0,0,0,0,0,1],[1,1,1,1,1,1],[5,4,3,2,1,0],[0,0,0,0,1,0],[0,0,0,2,0,0],[2,0,1,6,2,0,0]]

joinR :: Array Result -> Result
joinR = A.foldr acc Success
  where
    acc Success Success = Success
    acc Success f = f
    acc f _ = f

epsilon :: Number
epsilon = 1e-10

approx :: Number -> Number
approx n =
  let rn = round n in
  if abs (rn - n) < epsilon
  then rn else n

approxEq :: Number -> Number -> Result
approxEq a b =
  if approxEq' a b
  then Success
  else a === b

approxEq' :: Number -> Number -> Boolean
approxEq' a b = abs (a - b) < epsilon

deferror :: Boolean -> (Unit -> String) -> Result
deferror true _ = Success
deferror false f = Failed (f unit)

infix 2 deferror as <??>

checkSquare :: Matrix Number -> Array Result
checkSquare sq =
  let
    Tuple rows cols = dim sq
    trn = transpose sq
    det = determinant sq
    cfm = cofactorM sq
    inv = inverse sq
    idn = identity rows
    shwm = show sq
    left = matProduct inv sq
    right = matProduct sq inv
    partialdets =
      -- 1x1 matrix has no real cofactors
      if rows == 1 then []
      else zipWith (*) sq cfm # unMatrix
    rowtests = partialdets # A.mapWithIndex
      \i row -> approxEq' det (sum row)
        <??> \_-> "Row " <> show i <> " did not sum up to determinant: "
          <> "det " <> show sq <> " = " <> show det <> " != sum " <> show row <> " = " <> show (sum row)
  in
    [ rows == cols <?> "Matrix not square: " <> shwm
    , dim inv == dim sq <?> "Inverse has different dimensions: " <> shwm <> " and " <> show inv
    , approx <$> left === idn
    , approx <$> right === idn
    , det `approxEq'` determinant trn
      <??> \_-> "Determinant did not match transpose: "
        <> "det " <> show sq <> " = " <> show det <> " != det " <> show trn <> " = " <> show (determinant trn)
    ] <> rowtests

testSquare :: Gen Result
testSquare = do
  sz <- chooseInt 1 5
  sqmatrixOf sz <#> checkSquare <#> joinR

testSquareIgnore :: Gen Result
testSquareIgnore = do
  sz <- chooseInt 1 5
  ignore <- chooseInt 1 2
  let tot = sz + ignore
  let max = tot - 1
  sq <- sqmatrixOf (sz+ignore)
  sqi <- case ignore of
    1 -> do
      i <- chooseInt 0 max
      j <- chooseInt 0 max
      pure $ Ignore i j sq
    2 -> do
      i <- chooseInt 0 max
      j <- chooseInt 0 max
      k <- chooseInt 0 (max-1)
      l <- chooseInt 0 (max-1)
      pure $ Ignore k l $ Ignore i j sq
    _ -> pure sq
  pure (joinR (map (_ $ sqi) ops <> checkSquare sqi))

ops :: Array (Matrix Number -> Result)
ops =
  [ dblCheck "dim" dim
  , dblCheck "getTop" getTop
  , dblCheck "cofactorM" cofactorM
  , dblCheck "get 0 0" $ get 0 0
  , dblCheck "determinant" determinant
  , dblCheck "transpose" transpose
  ]

dblCheck :: forall a. Eq a => Show a => String -> (Matrix Number -> a) -> Matrix Number -> Result
dblCheck fn f m =
  let
    w = reMatrix m
    a = f m
    b = f w
  in a == b
    <??> \_ -> "Matrix " <> fn <> " result did not match up for matrix "
      <> show m <> ": "
        <> show a <> " /= " <> show b

testMatrix :: QC () Unit
testMatrix = do
  quickCheck testSquare
  quickCheck testSquareIgnore
  quickCheck' 1 (inverse m2 === mint [[3,-4],[-2,3]])
