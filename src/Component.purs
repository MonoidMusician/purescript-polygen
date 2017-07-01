module Main.Component where

import Data.Array as Array
import Data.Array as Arr
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Lens as HL
import Halogen.HTML.Lens.Button as HL.Button
import Halogen.HTML.Lens.Checkbox as HL.Checkbox
import Halogen.HTML.Lens.Input as HL.Input
import Halogen.HTML.Lens.Int as HL.Int
import Halogen.HTML.Lens.Number as HL.Number
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import DOM (DOM)
import Data.Array (cons, drop, head, intercalate, length, replicate, singleton, tail, take, zip)
import Data.Either (Either(..), isLeft)
import Data.Lens ((^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Suggestion (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Main.Matrix (Matrix, mkMatrix, unMatrix, inverse, matProduct)
import Main.PolyBuilder (PolyBuilder)
import Main.Polynomials (Atom, Polynomial, Row(Row), Table, mkDegree, disp, evalAt, gather, genp, mkRow, mkTable, nthderivative, parameters, parseLinear, substitute, lookupIn)
import Prelude hiding (degree)

type Query = HL.Query State
type Element p = H.HTML p Query
type LensComponent = forall p. State -> Element p

type State =
  { derivative :: Int
  , position :: Number
  , value :: String
  , conditions :: Array
      { derivative :: Int
      , position :: Number
      , value :: Row
      }
  }

_derivative :: Lens' State Int
_derivative = prop (SProxy :: SProxy "derivative")

_polynomial :: Lens' State Polynomial
_polynomial = lens (\{ conditions } -> mkDegree $ Arr.length conditions) (const)

_position :: Lens' State Number
_position = prop (SProxy :: SProxy "position")

_value :: Lens' State String
_value = prop (SProxy :: SProxy "value")

expand :: forall a. a -> Array a -> Int -> Array a
expand pad array to = replicate d pad <> drop (-d) array
  where
    l = length array
    d = to - l

expand' :: forall a. a -> Array a -> Int -> Array a
expand' pad array to = take to array <> replicate (to - length array) pad

_offbyone :: Lens' Int Int
_offbyone = lens (_-1) \_ -> (_+1)

_degree :: Lens' PolyBuilder Int
_degree = _Newtype <<< lens length (expand' true) <<< _offbyone

_origin :: Lens' PolyBuilder Boolean
_origin = _Newtype <<< lens
  (\poly -> case head poly of
    Just true -> false
    _ -> true
  )
  (\poly origin -> fromMaybe poly (cons (not origin) <$> tail poly))

addCondition :: State -> State
addCondition state@{ derivative, position, value: val, conditions } =
  case parseLinear val of
    Right value -> state
      { conditions = conditions <>
          [{derivative
          , position
          , value
          }]
      }
    _ -> state

derivativeComponent :: LensComponent
derivativeComponent state@{ derivative, conditions } =
  HH.div_
    [ HL.Int.renderBounded (Just 0) (Just (length conditions)) _derivative state
    , HH.text (suff <> " derivative: ")
    , HH.text $ show $ nthderivative derivative (state ^. _polynomial)
    ]
  where
    suff = case derivative of
      1 -> "st"
      2 -> "nd"
      3 -> "rd"
      _ -> "th"

evalRow :: forall r. Polynomial -> { position :: Number, derivative :: Int | r } -> Row
evalRow polynomial { position, derivative } =
  evalAt position $ nthderivative derivative polynomial

positionComponent :: LensComponent
positionComponent state =
  HH.div_
    [ HH.text "evaluated at x = "
    , HL.Number.renderBounded (Just (-10.0)) (Just 10.0) _position state
    , HH.text (": " <> show (evalRow (state ^. _polynomial) state))
    ]

valueComponent :: LensComponent
valueComponent state@{ derivative, position, value } =
  HH.div_
    [ HH.text "should equal "
    , HL.Input.render _value state
    , HH.text (": " <> evalue)
    , HH.div_ [ HL.Button.renderAsField "Add condition" addCondition (isLeft parsed) ]
    ]
  where
    parsed = parseLinear value
    evalue = case parsed of
      Left s -> s
      Right v -> show v

rowTable :: forall p. Array (Tuple Row Row) -> Element p
rowTable rows =
  HH.div_ $ map (HH.tr_ <<< map (HH.td_ <<< Arr.singleton <<< HH.text))
    (_header Arr.: _rows)
  where
    get = flip Map.lookup
    gets (Row r) = map (get r >>> fromMaybe 0.0 >>> disp)
    params = gather $ map fst rows
    values = gather $ map snd rows
    _header :: Array String
    _header = [""] <> map show params <> ["="] <> map show values
    _rows :: Array (Array String)
    _rows = rows # Arr.mapWithIndex \i (Tuple ps vs) ->
      [show (i+1) <> "."] <> gets ps params <> ["="] <> gets vs values

toMatrix :: Array Atom -> Array Row -> Matrix
toMatrix values rows = mkMatrix $ map (\r -> map (lookupIn r) values) rows

fromMatrix :: Array Atom -> Matrix -> Array Row
fromMatrix values matrix = map (mkRow <<< zip values) $ unMatrix matrix

compute :: Array (Tuple Row Row) -> Table
compute rows =
    mkTable $ zip params $ fromMatrix values matR
  where
    paramRs = map fst rows
    valueRs = map snd rows
    params = gather paramRs
    values = gather valueRs
    matA = toMatrix params paramRs
    matC = toMatrix values valueRs
    matA_ = inverse matA
    matR = matA_ `matProduct` matC

component :: forall eff. H.Component HH.HTML Query Unit Void (Aff (dom :: DOM | eff))
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { derivative: 0
    , position: 0.0
    , value: "0"
    , conditions: []
    }

  render :: State -> H.ComponentHTML Query
  render state@{ derivative, position, value, conditions } =
    HH.div_
      [ HH.h1_
          [ HH.text "Create a Polynomial" ]
      , HH.h2_
          [ HH.text "which satisfies certain properties" ]
      , HH.div_ [ HH.text ("f(x) = " <> show polynomial) ]
      , derivativeComponent state
      , positionComponent state
      , valueComponent state
      , HH.div_ [ HH.text ("f" <> genp derivative <> "(" <> show position <> ") = 0.0")]
      , HH.div_ $ map (HH.div_ <<< singleton) $ map (\r -> HH.text $ show (evalRow polynomial r) <> " = " <> show r.value) conditions
      , rowTable $ rows
      , if not solvable then HH.text "-" else
        let solution = compute rows in
          HH.div_
            [ HH.text $ show solution
            , HH.br_
            , HH.text $ show (substitute polynomial solution)
            ]
      ]
      where
        polynomial = state ^. _polynomial
        solvable =
            length conditions == length (parameters polynomial)
        rows = conditions # map \st -> Tuple (evalRow polynomial st) st.value

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval = HL.eval

separate :: forall p i. HH.HTML p i -> Array (HH.HTML p i) -> HH.HTML p i
separate sep = HH.span_ <<< intercalate [sep] <<< map Array.singleton
