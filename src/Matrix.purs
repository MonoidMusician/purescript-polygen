module Main.Matrix where

import Prelude
import Data.Array as Arr
import Data.Tuple (Tuple(..), fst, snd)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldl, minimum, sum)
import Data.Int (odd)

type Matrix = Array {-row-} (Array {-col-} Number)

dim :: Matrix -> Tuple Int Int
dim m = Tuple (Arr.length m) (fromMaybe 0 $ minimum $ map Arr.length m)

altSum :: Array Number -> Number
altSum = fst <<< foldl (\(Tuple a s) n -> Tuple (a+s*n) (negate s)) (Tuple 0.0 1.0)

remove :: Int -> Int -> Matrix -> Matrix
remove i j m = Arr.deleteAt i m # fromMaybe m # map (\r -> Arr.deleteAt j r # fromMaybe r)

mapTop :: (Matrix -> Number -> Number) -> Matrix -> Array Number
mapTop f m =
  fromMaybe [] $ Arr.mapWithIndex (\i -> f (remove 0 i m)) <$> Arr.head m

mapAll :: (Matrix -> Number -> Number) -> Matrix -> Matrix
mapAll f m =
  Arr.mapWithIndex (\i -> Arr.mapWithIndex \j d -> f (remove i j m) d) m

mapAllSigned :: (Matrix -> Number -> Number) -> Matrix -> Matrix
mapAllSigned f m =
  Arr.mapWithIndex (\i -> Arr.mapWithIndex \j d -> f (remove i j m) (sign i j)) m

sign :: Int -> Int -> Number
sign i j | odd (i+j) = -1.0
sign _ _ = 1.0

determinant :: Matrix -> Number
determinant [[]] = 0.0
determinant [[d]] = d
determinant m = altSum $ mapTop calccell m

calccell :: Matrix -> Number -> Number
calccell m d = d * determinant m

cofactorM :: Matrix -> Matrix
cofactorM = mapAllSigned calccell

inverse :: Matrix -> Matrix
inverse m = mapAll (const (_ / det)) $ transpose $ cofactorM m where det = determinant m

col :: Int -> Matrix -> Array Number
col j m =
  Arr.catMaybes $ map (_ Arr.!! j) m

transpose :: Matrix -> Matrix
transpose m = Arr.range 0 (snd (dim m) - 1) # map (flip col m)

dotProduct :: Array Number -> Array Number -> Number
dotProduct a b = sum $ Arr.zipWith (*) a b

matProduct :: Matrix -> Matrix -> Matrix
matProduct m n = map (\r -> map (dotProduct r) trn) m where trn = transpose n

checkInverse :: Matrix -> Matrix
checkInverse m = matProduct m (inverse m)
