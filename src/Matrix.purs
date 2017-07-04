module Main.Matrix where

import Prelude
import Data.Array as Arr
import Data.List as List
import Data.Array ((!!), (..))
import Data.Foldable (class Foldable, fold, foldMap, foldMapDefaultL, foldl, foldr, minimum, sum)
import Data.Int (odd)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (class Traversable, sequence, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (Gen, chooseInt, vectorOf)

data MatrixF a = Matrix (Array (Array a)) | Ignore Int Int (MatrixF a)
type Matrix = MatrixF Number
instance showM :: Show a => Show (MatrixF a) where show m = show $ unMatrix m

instance eqMatrixF :: Eq a => Eq (MatrixF a) where
  eq a b | dim a /= dim b = false
  eq (Matrix a) (Matrix b) = a == b
  eq (Ignore i j a) (Ignore k l b)
    | i == k && j == l && a == b = true
  eq a b = unMatrix a == unMatrix b

instance arbitraryMatrix :: Arbitrary a => Arbitrary (MatrixF a) where
  arbitrary = do
    rows <- chooseInt 1 5
    cols <- chooseInt 1 5
    matrixOf rows cols

matrixOf :: forall a. Arbitrary a => Int -> Int -> Gen (MatrixF a)
matrixOf rows cols = do
  Matrix <$> sequence (Arr.replicate rows (vectorOf cols (arbitrary :: Gen a)))

sqmatrixOf :: forall a. Arbitrary a => Int -> Gen (MatrixF a)
sqmatrixOf size = matrixOf size size

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
  foldMap f (Matrix m) = fold (foldMap (map f) m)
  foldMap f m = foldMapDefaultL f m

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

fill :: forall a. a -> Int -> Int -> MatrixF a
fill n r c =
  Matrix $ Arr.replicate r $ Arr.replicate c n

zeros :: forall n. Semiring n => Int -> Int -> MatrixF n
zeros = fill zero

identity :: forall n. Semiring n => Int -> MatrixF n
identity sz = fill one sz sz

instance matrixTraversable :: Traversable MatrixF where
  traverse f (Matrix m) = Matrix <$> traverse (traverse f) m
  traverse f (Ignore i j m) = Matrix <$> (traverse (traverse f) $ unMatrix m)
  sequence = sequenceDefault

mkMatrix :: forall a. Array (Array a) -> MatrixF a
mkMatrix = Matrix

unMatrix :: forall a. MatrixF a -> Array (Array a)
unMatrix (Matrix m) = m
unMatrix (Ignore i j m) =
  map (tryDeleteAt j) $ tryDeleteAt i $ unMatrix m

reMatrix :: forall a. MatrixF a -> MatrixF a
reMatrix = mkMatrix <<< unMatrix

tryDeleteAt :: forall a. Int -> Array a -> Array a
tryDeleteAt i as = Arr.deleteAt i as # fromMaybe as

dim :: forall a. MatrixF a -> Tuple Int Int
dim (Matrix m) = Tuple (Arr.length m) (fromMaybe 0 $ minimum $ map Arr.length m)
dim (Ignore i j m) = dim m - Tuple 1 1

altSum :: forall a. Ring a => Array a -> a
altSum = foldr (-) zero

mapTop :: forall a b. (MatrixF a -> a -> b) -> MatrixF a -> Array b
mapTop f m =
  Arr.mapWithIndex (\i -> f (Ignore 0 i m)) $ getTop m

getTop :: forall a. MatrixF a -> Array a
getTop m = Arr.mapMaybe (\i -> get 0 i m) (0..fst (dim m))

mapWithIndices :: forall a b. (Int -> Int -> a -> b) -> MatrixF a -> MatrixF b
mapWithIndices f m = list2mat $ go 0 0 (Nil : Nil)
  where
    Tuple rows cols = dim m
    go :: Int -> Int -> List (List b) -> List (List b)
    go i j b =
      if i == rows then fromMaybe b $ List.tail b else
      if j == cols - 1
        then go (i+1) 0 (get i j m # appRow b (f i j))
        else go i (j+1) (get i j m # appCol b (f i j))
    appRow :: List (List b) -> (a -> b) -> Maybe a -> List (List b)
    appRow b g mn = fromMaybe b do
      n <- mn
      { head, tail } <- List.uncons b
      pure (Nil : (g n:head) : tail)
    appCol :: List (List b) -> (a -> b) -> Maybe a -> List (List b)
    appCol b g mn = fromMaybe b do
      n <- mn
      { head, tail } <- List.uncons b
      pure ((g n:head) : tail)
    conv :: forall c. List c -> Array c
    conv = Arr.reverse <<< Arr.fromFoldable
    list2mat = Matrix <<< map conv <<< conv

sign :: forall n. Ring n => Int -> Int -> n
sign i j | odd (i+j) = negate one
sign _ _ = one

signed :: forall n. Ring n => Int -> Int -> n -> n
signed i j n = n * sign i j

determinant :: forall n. Ring n => MatrixF n -> n
determinant m =
  case dim m of
    Tuple 0 0 -> zero
    Tuple 1 1 -> fromMaybe zero $ get 0 0 m
    Tuple 2 2 -> fromMaybe zero $ do
      a <- get 0 0 m
      b <- get 0 1 m
      c <- get 1 0 m
      d <- get 1 1 m
      pure (a*d - b*c)
    Tuple r c | r /= c -> zero
    _ -> altSum $ mapTop calccell m

calccell :: forall n. Ring n => MatrixF n -> n -> n
calccell m d = d * determinant m

cofactorM :: forall n. Ring n => MatrixF n -> MatrixF n
cofactorM m = mapWithIndices (\i j _ -> sign i j * determinant (Ignore i j m)) m

inverse :: forall n. EuclideanRing n => MatrixF n -> MatrixF n
inverse m
  | dim m == Tuple 1 1
  , Just v <- get 0 0 m
    = mkMatrix [[one/v]]
inverse m
  | dim m == Tuple 2 2
  , Just a <- get 0 0 m
  , Just b <- get 0 1 m
  , Just c <- get 1 0 m
  , Just d <- get 1 1 m
    = let s = one/(a*d - b*c)
          z = negate s
      in mkMatrix [[s*d,z*b],[z*c,s*a]]
inverse m = map (_ / det) $ transpose $ cfm
  where
    cfm = cofactorM m
    det = sum $ Arr.zipWith (*) (getTop cfm) (getTop m)

row :: forall a. Int -> MatrixF a -> Array a
row i m = fromMaybe [] $ unMatrix m !! i

col :: forall a. Int -> MatrixF a -> Array a
col j m = Arr.mapMaybe (_ !! j) $ unMatrix m

transpose :: forall a. MatrixF a -> MatrixF a
transpose m = Matrix $ Arr.range 0 (snd (dim m) - 1) # map (flip col m)

dotProduct :: forall n. Semiring n => Array n -> Array n -> n
dotProduct a b = sum $ Arr.zipWith (*) a b

matProduct :: forall n. Semiring n => MatrixF n -> MatrixF n -> MatrixF n
matProduct m n =
  let trn = unMatrix $ transpose n
  in mkMatrix $ map ((flip map trn) <<< dotProduct) $ unMatrix m
