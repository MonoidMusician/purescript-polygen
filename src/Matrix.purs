module Main.Matrix where

import Prelude
import Data.Array as A
import Data.List as List
import Data.Array ((!!), (..))
import Data.Foldable (class Foldable, fold, foldMap, foldMapDefaultL, foldl, foldr, minimum, sum)
import Data.Int (odd)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (class Traversable, for, sequenceDefault, traverse)
import Data.Tuple (Tuple(..), fst, snd, uncurry)

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

instance matrixApply :: Apply MatrixF where
  apply = zipWith id

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

-- | Create a `Matrix` from a 2D `Array`.
mkMatrix :: forall a. Array (Array a) -> MatrixF a
mkMatrix = Matrix

-- | Take apart a `Matrix` as a 2D `Array`.
unMatrix :: forall a. MatrixF a -> Array (Array a)
unMatrix (Matrix m) = m
unMatrix (Ignore i j m) =
  map (tryDeleteAt j) $ tryDeleteAt i $ unMatrix m
  where
    tryDeleteAt :: forall b. Int -> Array b -> Array b
    tryDeleteAt l as = A.deleteAt l as # fromMaybe as

-- | Resolves a `Matrix` into simple `Array` structure,
reMatrix :: MatrixF ~> MatrixF
reMatrix = mkMatrix <<< unMatrix

-- | Returns the number of rows and columns in a `Matrix`.
-- Operates conservatively, giving the shortest column length.
dim :: forall a. MatrixF a -> Tuple Int Int
dim (Matrix m) =
  Tuple (A.length m) (fromMaybe 0 $ minimum $ map A.length m)
dim (Ignore i j m) =
  dim m - Tuple 1 1

-- | Fill a `Matrix` with the specified constant value.
fill :: forall a. a -> Int -> Int -> MatrixF a
fill n r c =
  mkMatrix $ A.replicate r $ A.replicate c n

-- | Generate a `Matrix` from a generator function which receives the current
-- | row and column indices.
generate :: forall a. (Int -> Int -> a) -> Int -> Int -> MatrixF a
generate f r c =
  mkMatrix $ map (\i -> map (f i) $ A.range 0 (c-1)) $ A.range 0 (r-1)

-- | Return the transpose of a `Matrix`, where rows and columns are flipped.
-- Does not behave nicely with non-rectangular shapes.
transpose :: MatrixF ~> MatrixF
transpose m =
  mkMatrix $ A.range 0 (snd (dim m) - 1) # map (flip col $ reMatrix m)

-- | Maybe get the value at row i, col j in the `Matrix`.
get :: Int -> Int -> MatrixF ~> Maybe
get i j (Matrix m) =
  m !! i >>= (_ !! j)
get i j (Ignore eye jay m) =
  get (fixidx eye i) (fixidx jay j) m
  where
    fixidx ignored idx = if idx < ignored then idx else idx + 1

-- | Get a list of the values at the specified indices, 0-indexed. Returns an
-- | empty `Array` if one or more indices is not valid.
gets :: Array (Tuple Int Int) -> MatrixF ~> Array
gets ijs m =
  fromMaybe [] $ for ijs $ map (_ $ m) (uncurry get)

-- | Unpack a 2x2 `Matrix` into an `Array` of size 4 (success) or 0 (failure).
unpack2x2 :: MatrixF ~> Array
unpack2x2 m | dim m == Tuple 2 2 = gets
  [ Tuple 0 0, Tuple 0 1
  , Tuple 1 0, Tuple 1 1 ] m
unpack2x2 _ = []

-- | Get the ith row (0-indexed) of a `Matrix` as an `Array`.
row :: Int -> MatrixF ~> Array
row i m = fromMaybe [] $ unMatrix m !! i

-- | Get the jth col (0-indexed) of a `Matrix` as an `Array`.
col :: Int -> MatrixF ~> Array
col j m = A.mapMaybe (_ !! j) $ unMatrix m

-- | Return a sized `Matrix` filled with zeros.
zeros :: forall r. Semiring r => Int -> Int -> MatrixF r
zeros = fill zero

-- | Generate the multiplicative identity for a `Matrix` in a `Semiring`.
-- | (Square sized with ones on the diagonal, zeros elsewhere.)
identity :: forall r. Semiring r => Int -> MatrixF r
identity sz =
  generate (\i j -> if i == j then one else zero) sz sz

-- | Map just the top row of a `Matrix`, passing the minor `Matrix`. Used to
-- | compute determinants.
mapTop :: forall a b. (MatrixF a -> a -> b) -> MatrixF a -> Array b
mapTop f m =
  A.mapWithIndex (\i -> f (Ignore 0 i m)) $ getTop m

-- | Get the top row of a `Matrix` as an `Array`.
getTop :: MatrixF ~> Array
getTop m = A.mapMaybe (\i -> get 0 i m) (0..fst (dim m))

-- | Map through a `Matrix`, additionally passing the row and column indices to
-- | the mapping function.
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

-- | Zip two matrices together, where the specified function determines the
-- | value in the final `Matrix` based on the corresponding values of the
-- | original matrices.
-- Operates conservatively, taking the shortest rows and columns it finds.
zipWith :: forall a b c. (a -> b -> c) -> MatrixF a -> MatrixF b -> MatrixF c
zipWith f (Matrix a) (Matrix b) = Matrix $ A.zipWith (A.zipWith f) a b
zipWith f ma mb = Matrix $ A.zipWith (A.zipWith f) a b
  where a = unMatrix ma
        b = unMatrix mb

-- | Creates a `Matrix` of `Tuple`s of the corresponding elements. Uncommon.
zip :: forall a b. MatrixF a -> MatrixF b -> MatrixF (Tuple a b)
zip = zipWith Tuple

-- | Returns the addition of two matrices with a `Semiring` instance.
matAdd :: forall r. Semiring r => MatrixF r -> MatrixF r -> MatrixF r
matAdd = zipWith (+)

-- | Returns the dot product of two vectors with a `Semiring` instance.
dotProduct :: forall r. Semiring r => Array r -> Array r -> r
dotProduct a b = sum $ A.zipWith (*) a b

-- | Returns the product of two matrices with a `Semiring` instance.
matProduct :: forall r. Semiring r => MatrixF r -> MatrixF r -> MatrixF r
matProduct m n =
  let trn = unMatrix $ transpose n
  in mkMatrix $ map ((flip map trn) <<< dotProduct) $ unMatrix m

-- | Returns the determinant of a square `Matrix` with elements from a `Ring`.
-- | Returns 0 when in doubt.
determinant :: forall r. Ring r => MatrixF r -> r
determinant m =
  case dim m of
    Tuple 0 0 -> zero
    Tuple 1 1 -> fromMaybe zero $ get 0 0 m
    Tuple 2 2 -> case unpack2x2 m of
      [a,b,c,d] -> determinant2x2 a b c d
      _ -> zero
    Tuple r c | r /= c -> zero
    _ -> foldr (-) zero $ mapTop (\minor d -> d * determinant minor) m

-- | Specialized determinant of a deconstructed 2x2 matrix.
determinant2x2 :: forall r. Ring r => r -> r -> r -> r -> r
determinant2x2 a b c d = a*d - b*c

-- | Compute the `Matrix` of cofactors. Requires square `Matrix` with elements
-- | from a `Ring`.
cofactorM :: forall r. Ring r => MatrixF r -> MatrixF r
cofactorM m =
    mapWithIndices (\i j _ -> sign i j * determinant (Ignore i j m)) m
  where
    sign i j | odd (i+j) = negate one
    sign _ _ = one

-- | Return the inverse of a `Matrix` such that the left or right product of
-- | a `Matrix` and its inverse equals `identity` (of the same size). Requires
-- | a square `Matrix` with elements from a `EuclideanRing`. Will return a
-- | `Matrix` of NaNs (or other division by `zero` values) if the inverse does
-- | not exist.
inverse :: forall r. EuclideanRing r => MatrixF r -> MatrixF r
inverse m
  | dim m == Tuple 1 1
  , Just v <- get 0 0 m
    = mkMatrix [[one/v]]
inverse m
  | [a, b, c, d] <- unpack2x2 m
      -- compute scaling factors, positive and negative
    = let s = one / determinant2x2 a b c d
          z = negate s
      in mkMatrix [[s*d,z*b],[z*c,s*a]]
inverse m =
    -- transpose cofactors and divide by determinant
    map (_ / det) $ transpose $ cfm
  where
    -- compute cofactors
    cfm = cofactorM m
    -- compute determinant` from product of entries and cofactors
    det = sum $ A.zipWith (*) (getTop cfm) (getTop m)
