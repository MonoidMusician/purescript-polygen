module Main.Matrix where

import Prelude
import Data.Array as A
import Data.List as List
import Data.Array ((!!), (..))
import Data.Foldable (class Foldable, fold, foldMap, foldMapDefaultL, foldl, foldr, minimum, sum)
import Data.Int (odd)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd)

data MatrixF a = Matrix (Array (Array a)) | Ignore Int Int (MatrixF a)
type Matrix = MatrixF Number
instance showM :: Show a => Show (MatrixF a) where show m = show $ unMatrix m

instance eqMatrixF :: Eq a => Eq (MatrixF a) where
  eq a b | dim a /= dim b = false
  eq (Matrix a) (Matrix b) = a == b
  eq (Ignore i j a) (Ignore k l b)
    | i == k && j == l && a == b = true
  eq a b = unMatrix a == unMatrix b

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

instance matrixTraversable :: Traversable MatrixF where
  traverse f (Matrix m) = Matrix <$> traverse (traverse f) m
  traverse f (Ignore i j m) = Matrix <$> (traverse (traverse f) $ unMatrix m)
  sequence = sequenceDefault

get :: forall a. Int -> Int -> MatrixF a -> Maybe a
get i j (Matrix m) =
  m !! i >>= (_ !! j)
get i j (Ignore eye jay m) =
  get (fixidx eye i) (fixidx jay j) m
  where
    fixidx ignored idx = if idx < ignored then idx else idx + 1

gets :: forall a. Array (Tuple Int Int) -> MatrixF a -> Array a
gets ijs m =
  fromMaybe [] $ traverse (\(Tuple i j) -> get i j m) ijs

zipWith :: forall a b c. (a -> b -> c) -> MatrixF a -> MatrixF b -> MatrixF c
zipWith f (Matrix a) (Matrix b) = Matrix $ A.zipWith (A.zipWith f) a b
zipWith f ma mb = Matrix $ A.zipWith (A.zipWith f) a b
  where a = unMatrix ma
        b = unMatrix mb

zip :: forall a b. MatrixF a -> MatrixF b -> MatrixF (Tuple a b)
zip = zipWith Tuple

fill :: forall a. a -> Int -> Int -> MatrixF a
fill n r c =
  Matrix $ A.replicate r $ A.replicate c n

build :: forall a. (Int -> Int -> a) -> Int -> Int -> MatrixF a
build f r c =
  Matrix $ map (\i -> map (f i) $ A.range 0 (c-1)) $ A.range 0 (r-1)

zeros :: forall r. Semiring r => Int -> Int -> MatrixF r
zeros = fill zero

identity :: forall r. Semiring r => Int -> MatrixF r
identity sz =
  build (\i j -> if i == j then one else zero) sz sz

mkMatrix :: forall a. Array (Array a) -> MatrixF a
mkMatrix = Matrix

unMatrix :: forall a. MatrixF a -> Array (Array a)
unMatrix (Matrix m) = m
unMatrix (Ignore i j m) =
  map (tryDeleteAt j) $ tryDeleteAt i $ unMatrix m
  where
    tryDeleteAt :: forall b. Int -> Array b -> Array b
    tryDeleteAt l as = A.deleteAt l as # fromMaybe as

reMatrix :: forall a. MatrixF a -> MatrixF a
reMatrix = mkMatrix <<< unMatrix

unpack2x2 :: forall a. MatrixF a -> Array a
unpack2x2 m | dim m == Tuple 2 2 = gets
  [ Tuple 0 0, Tuple 0 1
  , Tuple 1 0, Tuple 1 1 ] m
unpack2x2 _ = []

dim :: forall a. MatrixF a -> Tuple Int Int
dim (Matrix m) =
  Tuple (A.length m) (fromMaybe 0 $ minimum $ map A.length m)
dim (Ignore i j m) =
  dim m - Tuple 1 1

altSum :: forall r. Ring r => Array r -> r
altSum = foldr (-) zero

mapTop :: forall a b. (MatrixF a -> a -> b) -> MatrixF a -> Array b
mapTop f m =
  A.mapWithIndex (\i -> f (Ignore 0 i m)) $ getTop m

getTop :: forall a. MatrixF a -> Array a
getTop m = A.mapMaybe (\i -> get 0 i m) (0..fst (dim m))

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
    appRow b g mn = Nil : appCol b g mn
    appCol b g mn = fromMaybe b do
      n <- mn
      { head, tail } <- List.uncons b
      pure ((g n:head) : tail)
    conv :: forall c. List c -> Array c
    conv = A.reverse <<< A.fromFoldable
    list2mat = Matrix <<< map conv <<< conv

sign :: forall r. Ring r => Int -> Int -> r
sign i j | odd (i+j) = negate one
sign _ _ = one

signed :: forall r. Ring r => Int -> Int -> r -> r
signed i j n = n * sign i j

determinant2x2 :: forall r. Ring r => r -> r -> r -> r -> r
determinant2x2 a b c d = a*d - b*c

determinant :: forall r. Ring r => MatrixF r -> r
determinant m =
  case dim m of
    Tuple 0 0 -> zero
    Tuple 1 1 -> fromMaybe zero $ get 0 0 m
    Tuple 2 2 -> case unpack2x2 m of
      [a,b,c,d] -> determinant2x2 a b c d
      _ -> zero
    Tuple r c | r /= c -> zero
    _ -> altSum $ mapTop calccell m

calccell :: forall r. Ring r => MatrixF r -> r -> r
calccell m d = d * determinant m

cofactorM :: forall r. Ring r => MatrixF r -> MatrixF r
cofactorM m = mapWithIndices (\i j _ -> sign i j * determinant (Ignore i j m)) m

inverse :: forall r. EuclideanRing r => MatrixF r -> MatrixF r
inverse m
  | dim m == Tuple 1 1
  , Just v <- get 0 0 m
    = mkMatrix [[one/v]]
inverse m
  | [a, b, c, d] <- unpack2x2 m
    = let s = one / determinant2x2 a b c d
          z = negate s
      in mkMatrix [[s*d,z*b],[z*c,s*a]]
inverse m = map (_ / det) $ transpose $ cfm
  where
    cfm = cofactorM m
    det = sum $ A.zipWith (*) (getTop cfm) (getTop m)

row :: forall a. Int -> MatrixF a -> Array a
row i m = fromMaybe [] $ unMatrix m !! i

col :: forall a. Int -> MatrixF a -> Array a
col j m = A.mapMaybe (_ !! j) $ unMatrix m

transpose :: forall a. MatrixF a -> MatrixF a
transpose m =
  Matrix $ A.range 0 (snd (dim m) - 1) # map (flip col $ reMatrix m)

dotProduct :: forall r. Semiring r => Array r -> Array r -> r
dotProduct a b = sum $ A.zipWith (*) a b

matProduct :: forall r. Semiring r => MatrixF r -> MatrixF r -> MatrixF r
matProduct m n =
  let trn = unMatrix $ transpose n
  in mkMatrix $ map ((flip map trn) <<< dotProduct) $ unMatrix m
