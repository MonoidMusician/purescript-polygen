module Main.Polynomials where

import Prelude
import Control.Apply (lift2)
import Data.Array ((:))
import Data.Array (filter, fromFoldable, uncons, foldr) as Arr
import Data.Either (Either(..))
import Data.Int (fromNumber, toNumber)
import Data.Map (Map)
import Data.Map (filter, fromFoldableWith, mapWithKey, isEmpty) as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (class Newtype)
import Data.Number (fromString) as Number
import Data.String (joinWith, singleton)
import Data.String.Regex (Regex, match, replace, split)
import Data.String.Regex.Flags (global, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (maximum)
import Data.Tuple (Tuple(..))
import Main.PolyBuilder (PolyBuilder(..), _CharCode, dimension', geni, genx)
import Math (pow)

newtype Variable = Variable String
derive newtype instance eqVariable :: Eq Variable
derive newtype instance ordVariable :: Ord Variable
instance showVariable :: Show Variable where show (Variable s) = s

data Atom = K | V Variable
instance eqAtom :: Eq Atom where
  eq K K = true
  eq (V a) (V b) = a == b
  eq _ _ = false
instance ordAtom :: Ord Atom where
  compare K (V _) = LT
  compare (V _) K = GT
  compare K K = EQ
  compare (V a) (V b) = compare a b
instance showAtom :: Show Atom where
  show K = ""
  show (V v) = show v

mmkVariable :: String -> Maybe Variable
mmkVariable s
  | isJust $ match (unsafeRegex "^\\w+$" global) s = Just (Variable s)
  | otherwise = Nothing

mmkAtom :: String -> Maybe Atom
mmkAtom "" = Just K
mmkAtom s = V <$> mmkVariable s

genp :: Int -> String
genp 0 = ""
genp 1 = "′"
genp 2 = "″"
genp 3 = "‴"
genp n = "⁽" <> geni n <> "⁾"

newtype Polynomial = Polynomial
  (Array
    { coefficient :: Number
    , atom :: Atom
    , exponent :: Int
    })
derive instance newtypePolynomial :: Newtype Polynomial _

disp :: Number -> String
disp x = case fromNumber x of
  Just n ->
    show n
  Nothing ->
    show x

instance showPolynomial :: Show Polynomial where
  show (Polynomial poly) = joinWith " + " $ map show1 poly
    where
      show1 { coefficient: 1.0, atom, exponent } =
        show atom <> genx exponent
      show1 { coefficient, atom, exponent } =
        disp coefficient <> show atom <> genx exponent

build :: PolyBuilder -> Polynomial
build (PolyBuilder poly) = Polynomial $ go poly c0 0 []
  where
    c0 = _CharCode (\a -> a + dimension' poly - 1) 'a'
    go a c i r = case Arr.uncons a of
      Nothing -> r
      Just { head: false, tail: a' } ->
        go a' c (i+1) r
      Just { head: true, tail: a' } ->
        let
          var = singleton c
          c' = _CharCode (_ - 1) c
        in go a' c' (i+1) $
          { coefficient: 1.0
          , atom: V (Variable var)
          , exponent: i
          } : r

derivative :: Polynomial -> Polynomial
derivative (Polynomial poly) = Polynomial
  $ Arr.filter (_.coefficient >>> (_ /= 0.0))
  $ map (\d@{coefficient, exponent} -> d
    { coefficient = coefficient * toNumber exponent
    , exponent = exponent - 1
    })
  $ poly

nthderivative :: Int -> Polynomial -> Polynomial
nthderivative 0 p = p
nthderivative 1 p = derivative p
nthderivative n p = nthderivative (n-1) (derivative p)

degree :: Polynomial -> Int
degree (Polynomial poly) = fromMaybe 0 $ maximum $ map _.exponent poly

newtype Row = Row (Map Atom Number)

mkRow :: Array (Tuple Atom Number) -> Row
mkRow = Row <<< Map.filter (0.0 /= _) <<< Map.fromFoldableWith (+)

derive instance newtypeRow :: Newtype Row _
instance showRow :: Show Row where
  show (Row m) | Map.isEmpty m = "0"
  show (Row m) =
    joinWith " + " $
    Arr.fromFoldable $
    Map.mapWithKey (\atom value -> disp value <> show atom) m

evalAt :: Number -> Polynomial -> Row
evalAt x (Polynomial poly) = mkRow $ map calc poly
  where
    calc { coefficient, atom, exponent } =
      Tuple atom (coefficient * raise x exponent)

eval :: Polynomial -> Number -> Row
eval = flip evalAt

raise :: Number -> Int -> Number
raise x i = pow x (toNumber i)

linearR :: Regex
linearR = unsafeRegex
  "^(?:(-?\\d*(?:\\.\\d+)?)([_a-zA-Z]+)|(-?\\d+(?:\\.\\d+)?)\\.?)(?:/(-?\\d+(?:\\.\\d+)?)\\.?)?$"
  noFlags

parseLinear :: String -> Either String Row
parseLinear "" = Left "Empty string"
parseLinear orig =
    map mkRow
    $ Arr.foldr (lift2 (:)) (Right [])
    $ map parseTerm sections
  where
    mkv = V <<< Variable
    minus = unsafeRegex "\\s*-\\s*" global
    plus = unsafeRegex "\\s*\\++\\s*" global
    sections =
      orig
      # replace minus "+-"
      # split plus
      # Arr.filter ("" /= _)
    parseDenomm = fromMaybe "1.0" >>> Number.fromString >>> fromMaybe 1.0
    parseTerm term =
      case match linearR term of
        Nothing -> Left ("Malformed term: " <> term)
        Just [_, Just "", Just var, Nothing, denomm] ->
          Right (Tuple (mkv var) (1.0/parseDenomm denomm))
        Just [_, Just "-", Just var, Nothing, denomm] ->
          Right (Tuple (mkv var) ((-1.0)/parseDenomm denomm))
        Just [_, Just coef, Just var, Nothing, denomm] ->
          case Number.fromString coef of
            Nothing -> Left ("Invalid number " <> coef)
            Just coeff ->
              Right (Tuple (mkv var) (coeff/parseDenomm denomm))
        Just [_, Nothing, Nothing, Just const, denomm] ->
          case Number.fromString const of
            Nothing -> Left ("Invalid number " <> const)
            Just k ->
              Right (Tuple K (k/parseDenomm denomm))
        Just m -> Left ("Failed to parse " <> term <> " as " <> show m)