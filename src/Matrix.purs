module Main.Matrix where

import Prelude
import Data.Array as Arr
import Data.List as List
import Data.Array ((!!), (..))
import Data.Foldable (class Foldable, foldMapDefaultL, foldl, foldr, minimum, sum)
import Data.Int (odd, toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd)

data TraverseRowST a b = TraverseRowST Int (MatrixF a) b
data TraverseColST a b = TraverseColST Int Int (MatrixF a) b
derive instance functorTraverseRowST :: Functor (TraverseRowST a)
derive instance functorTraverseColST :: Functor (TraverseColST a)

startRow :: forall a b. MatrixF a -> b -> TraverseRowST a b
startRow m b = TraverseRowST 0 m b

startCol :: forall a b. TraverseRowST a b -> TraverseColST a b
startCol (TraverseRowST i m b) = TraverseColST i 0 m b

data MatrixF a = Matrix (Array (Array a)) | Ignore Int Int (MatrixF a)
type Matrix = MatrixF Number
instance showM :: Show a => Show (MatrixF a) where show m = show $ unMatrix m

instance matrixFunctor :: Functor MatrixF where
  map f (Matrix m) = Matrix $ map (map f) m
  map f (Ignore i j m) = Ignore i j $ map f m

instance matrixFoldable :: Foldable MatrixF where
  foldl f b0 (Matrix m) = foldl (foldl f) b0 m
  foldl f b0 m@(Ignore _ _ _) = go 0 0 b0
    where
      Tuple rows cols = dim m
      go i j b =
        if j == cols then
          if i == rows then b
          else go (i+1) 0 (get i j m # maybe b (f b))
        else go i (j+1) (get i j m # maybe b (f b))
  foldr f b0 m = foldr (flip (foldr f)) b0 $ unMatrix m
  foldMap = foldMapDefaultL

fixidx :: Int -> Int -> Int
fixidx eye i = if i < eye then i else i + 1

get :: forall a. Int -> Int -> MatrixF a -> Maybe a
get i j (Matrix m) =
  m !! i >>= (_ !! j)
get i j (Ignore eye jay m) =
  get (fixidx eye i) (fixidx jay j) m

zipWith :: forall a b c. (a -> b -> c) -> MatrixF a -> MatrixF b -> MatrixF c
zipWith f (Matrix a) (Matrix b) = Matrix $ Arr.zipWith (Arr.zipWith f) a b
zipWith f ma mb = Matrix $ Arr.zipWith (Arr.zipWith f) a b
  where
    a = unMatrix ma
    b = unMatrix mb

zip :: forall a b. MatrixF a -> MatrixF b -> MatrixF (Tuple a b)
zip = zipWith Tuple

fill :: Number -> Int -> Int -> Matrix
fill n r c =
  Matrix $ Arr.replicate r $ Arr.replicate c n

instance matrixTraversable :: Traversable MatrixF where
  traverse f (Matrix m) = Matrix <$> traverse (traverse f) m
  traverse f (Ignore i j m) = Matrix <$> (traverse (traverse f) $ unMatrix m)
  sequence = sequenceDefault

mkMatrix :: forall a. Array (Array a) -> MatrixF a
mkMatrix = Matrix

unMatrix :: forall a. MatrixF a -> Array (Array a)
unMatrix (Matrix m) = m
unMatrix (Ignore i j m) =
  unMatrix m # tryDeleteAt i # map (tryDeleteAt j)

reMatrix :: forall a. MatrixF a -> MatrixF a
reMatrix = mkMatrix <<< unMatrix

tryDeleteAt :: forall a. Int -> Array a -> Array a
tryDeleteAt i as = Arr.deleteAt i as # fromMaybe as

dim :: forall a. MatrixF a -> Tuple Int Int
dim (Matrix m) = Tuple (Arr.length m) (fromMaybe 0 $ minimum $ map Arr.length m)
dim (Ignore i j m) = dim m - Tuple 1 1

altSum :: Array Number -> Number
altSum = fst <<< foldl (\(Tuple a s) n -> Tuple (a+s*n) (negate s)) (Tuple 0.0 1.0)


mapTop :: (Matrix -> Number -> Number) -> Matrix -> Array Number
mapTop f m =
  Arr.mapWithIndex (\i -> f (Ignore 0 i m)) $ getTop m

getTop :: forall a. MatrixF a -> Array a
{-getTop = go Arr.head >>> fromMaybe []
  where
    go :: (Array (Array a) -> Maybe (Array a)) -> MatrixF a -> Maybe (Array a)
    go f = case _ of
      Matrix m -> f m
      Ignore 0 j m ->
        go (\a -> Arr.tail a >>= f >>= Arr.deleteAt j) m
      Ignore _ j m ->
        go (\a -> f a >>= Arr.deleteAt j) m
-}
getTop m = Arr.catMaybes $ map (\i -> get 0 i m) (0..fst (dim m))

mapWithIndices :: forall a. (Int -> Int -> Number -> a) -> Matrix -> MatrixF a
mapWithIndices f m = list2mat $ go 0 0 (Nil : Nil)
  where
    Tuple rows cols = dim m
    go :: Int -> Int -> List (List a) -> List (List a)
    go i j b =
      if i == rows then fromMaybe b $ List.tail b else
      if j == cols - 1
        then go (i+1) 0 (get i j m # appRow b (f i j))
        else go i (j+1) (get i j m # appCol b (f i j))
    appRow :: List (List a) -> (Number -> a) -> Maybe Number -> List (List a)
    appRow b g mn = fromMaybe b do
      n <- mn
      { head, tail } <- List.uncons b
      pure (Nil : (g n:head) : tail)
    appCol :: List (List a) -> (Number -> a) -> Maybe Number -> List (List a)
    appCol b g mn = fromMaybe b do
      n <- mn
      { head, tail } <- List.uncons b
      pure ((g n:head) : tail)
    conv :: forall b. List b -> Array b
    conv = Arr.reverse <<< Arr.fromFoldable
    list2mat = Matrix <<< map conv <<< conv

{-
mapAll :: (Matrix -> Number -> Number) -> Matrix -> Matrix
mapAll f m =
  Arr.mapWithIndex (\i -> Arr.mapWithIndex \j d -> f (Ignore i j m) d) m

mapWithIndices :: (Matrix -> Number -> Number) -> Matrix -> Matrix
mapWithIndices f m =
  Arr.mapWithIndex (\i -> Arr.mapWithIndex \j d -> f (Ignore i j m) (sign i j)) m
-}

sign :: Int -> Int -> Number
sign i j | odd (i+j) = -1.0
sign _ _ = 1.0

signed :: Int -> Int -> Number -> Number
signed i j n = n * sign i j

determinant :: Matrix -> Number
determinant m =
  case dim m of
    Tuple 0 0 -> 0.0
    Tuple 1 1 -> fromMaybe 0.0 $ get 0 0 m
    Tuple 2 2 -> fromMaybe 0.0 $ do
      a <- get 0 0 m
      b <- get 0 1 m
      c <- get 1 0 m
      d <- get 1 1 m
      pure (a*d - b*c)
    Tuple r c | r /= c -> 0.0
    _ -> altSum $ mapTop calccell m

calccell :: Matrix -> Number -> Number
calccell m d = d * determinant m

cofactorM :: Matrix -> Matrix
cofactorM m = mapWithIndices (\i j _ -> sign i j * determinant (Ignore i j m)) m

inverse :: Matrix -> Matrix
inverse m = map (_ / det) $ transpose $ cfm
  where
    cfm = cofactorM m
    --det = determinant m
    det = sum $ Arr.zipWith (*) (getTop cfm) (getTop m)

col :: Int -> Matrix -> Array Number
col j m =
  Arr.catMaybes $ map (_ !! j) $ unMatrix m

transpose :: Matrix -> Matrix
transpose m = Matrix $ Arr.range 0 (snd (dim m) - 1) # map (flip col m)

dotProduct :: Array Number -> Array Number -> Number
dotProduct a b = sum $ Arr.zipWith (*) a b

matProduct :: Matrix -> Matrix -> Matrix
matProduct m n = Matrix $ map (\r -> map (dotProduct r) $ unMatrix trn) $ unMatrix m where trn = transpose n

checkInverse :: Matrix -> Matrix
checkInverse m = matProduct m (inverse m)

checkDeterminant :: Matrix -> Boolean
checkDeterminant m = d == det
  where
    cfm = cofactorM m
    d = determinant m
    det = sum $ Arr.zipWith (*) (getTop cfm) (getTop m)

mint :: Array (Array Int) -> Matrix
mint = map toNumber <<< mkMatrix
m1 :: Matrix
m1 = mint [[3,4],[2,3]]
mall :: Matrix
mall = mint [[1,2,3],[4,5,6],[7,8,9]]
cftest :: Matrix
cftest = mint [[1,2,3],[0,4,5],[1,0,6]]
m6 :: Matrix
m6 = mint [[0,0,0,0,0,1],[1,1,1,1,1,1],[5,4,3,2,1,0],[0,0,0,0,1,0],[0,0,0,2,0,0],[2,0,1,6,2,0,0]]
