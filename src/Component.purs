module Main.Component where

import Data.Array as Arr
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
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Suggestion (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Main.Matrix (Matrix, mkMatrix, unMatrix, inverse, matProduct)
import Main.PolyBuilder (PolyBuilder)
import Main.Polynomials (Atom, Polynomial, Row, Table, disp, evalAt, gather, genp, lookupIn, mkRow, mkSpecialized, mkTable, nthderivative, parseLinear, substitute, zeroRow)
import Prelude hiding (degree)

type Query = HL.Query State
type Element p = H.HTML p Query
type LensComponent = forall p. Computed -> State -> Element p

type StateBase r =
  { derivative :: Int
  , position :: Number
  , value :: String
  | r
  }
type State = StateBase (conditions :: Conditions)
type Conditions = Array Condition
type ConditionBase r =
  { derivative :: Int
  , position :: Number
  , value :: Row
  | r
  }
type ConditionPlus =
  ConditionBase
    ( parameters :: Row
    , polynomial :: Polynomial
    )
type ConditionsPlus = Array ConditionPlus
type Condition = ConditionBase ()
type Computed = StateBase
  ( polynomial :: Polynomial
  , conditions :: ConditionsPlus
  , params :: Array Atom
  , values :: Array Atom
  , coefficientM :: Matrix
  , valueM :: Matrix
  , coefficientMI :: Matrix
  , productM :: Matrix
  , result :: Table
  )

_derivative :: Lens' State Int
_derivative = prop (SProxy :: SProxy "derivative")

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
derivativeComponent { derivative, conditions, polynomial } state =
  HH.div_
    [ HL.Int.renderBounded (Just 0) (Just (length conditions)) _derivative state
    , HH.text (suff <> " derivative: ")
    , HH.text $ show $ nthderivative derivative polynomial
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
positionComponent { polynomial } state =
  HH.div_
    [ HH.text "evaluated at x = "
    , HL.Number.renderBounded (Just (-10.0)) (Just 10.0) _position state
    , HH.text (": " <> show (evalRow polynomial state))
    ]

valueComponent :: LensComponent
valueComponent { derivative, position, value } state =
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

specialization :: Conditions -> Array Int
specialization conditions =
  Arr.catMaybes $ conditions # map case _ of
    r@{ derivative: d } | trivial r ->
      Just d
    _ -> Nothing

trivial :: Condition -> Boolean
trivial r =
  r.position == 0.0 && r.value == zeroRow

nonTrivials :: Conditions -> Conditions
nonTrivials = Arr.filter (not trivial)

rowTable :: forall p. ConditionsPlus -> Element p
rowTable rows =
  HH.div_ $ map (HH.tr_ <<< map (HH.td_ <<< Arr.singleton <<< HH.text))
    (_header Arr.: _rows)
  where
    listWith r = map (lookupIn r >>> disp)
    params = gather $ map _.parameters rows
    values = gather $ map _.value rows
    _header :: Array String
    _header = [""] <> map show params <> ["="] <> map show values
    _rows :: Array (Array String)
    _rows = rows # Arr.mapWithIndex \i { parameters: ps, value: vs } ->
      [show (i+1) <> "."] <> listWith ps params <> ["="] <> listWith vs values

toMatrix :: Array Atom -> Array Row -> Matrix
toMatrix values rows = mkMatrix $ map (\r -> map (lookupIn r) values) rows

fromMatrix :: Array Atom -> Matrix -> Array Row
fromMatrix values matrix = map (mkRow <<< zip values) $ unMatrix matrix

compute :: State -> Computed
compute { derivative, position, value, conditions: cs } =
    { derivative, position, value
    , polynomial, conditions
    , params, values
    , coefficientM, valueM
    , coefficientMI, productM
    , result
    }
  where
    polynomial = mkSpecialized (Arr.length cs) $ specialization cs
    conditions = nonTrivials cs # map
      \{ derivative: d, position: p, value: v } ->
        { derivative: d, position: p, value: v
        , polynomial
        , parameters:
            evalAt p $
              nthderivative d polynomial
        }
    paramRs = map _.parameters conditions
    valueRs = map _.value conditions
    params = gather paramRs
    values = gather valueRs
    coefficientM = toMatrix params paramRs
    valueM = toMatrix values valueRs
    coefficientMI = inverse coefficientM
    productM = coefficientMI `matProduct` valueM
    result = mkTable $ zip params $ fromMatrix values productM

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
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Create a Polynomial" ]
      , HH.h2_
          [ HH.text "which satisfies certain properties" ]
      , HH.div_ [ HH.text ("f(x) = " <> show polynomial) ]
      , derivativeComponent computed state
      , positionComponent computed state
      , valueComponent computed state
      , HH.div_ [ HH.text ("f" <> genp derivative <> "(" <> show position <> ") = 0.0")]
      , HH.div_ $ map (HH.div_ <<< singleton) $ map (\r -> HH.text $ show r.parameters <> " = " <> show r.value) conditions
      , rowTable $ conditions
      , HH.div_
          [ HH.text $ show result
          , HH.br_
          , HH.text $ show (substitute polynomial result)
          ]
      ]
      where
        computed@
          { derivative, position, value
          , polynomial, conditions
          , params, values
          , coefficientM, valueM
          , coefficientMI, productM
          , result
          } = compute state

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval = HL.eval

separate :: forall p i. HH.HTML p i -> Array (HH.HTML p i) -> HH.HTML p i
separate sep = HH.span_ <<< intercalate [sep] <<< map Arr.singleton
