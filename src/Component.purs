module Main.Component where

import Data.Array as Array
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
import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array (cons, drop, head, intercalate, length, replicate, singleton, tail, take)
import Data.Either (Either(..), isLeft, isRight)
import Data.Lens (preview, prism', review)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Suggestion (Lens', lens)
import Data.Lens.Types (Prism')
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Symbol (SProxy(..))
import Main.PolyBuilder (PolyBuilder(..), dimension, renderPolyBuilder)
import Main.Polynomials (Polynomial, Row(..), build, degree, derivative, evalAt, genp, nthderivative, parseLinear)
import Partial.Unsafe (unsafePartial)
import Prelude hiding (degree)

type Query = HL.Query State

type Element p = H.HTML p Query

data State
  = Building PolyBuilder
  | Deriving Deriving

type Deriving =
  { polynomial :: Polynomial
  , derivative :: Int
  , position :: Number
  , value :: String
  , conditions :: Array
      { polynomial :: Polynomial
      , derivative :: Int
      , position :: Number
      , value :: Row
      }
  }

_unsafePrismicLens :: forall a s. Prism' a s -> Lens' a s
_unsafePrismicLens p = lens (unsafePartial fromJust <<< preview p) (const (review p))

_Building :: Prism' State PolyBuilder
_Building = prism' Building case _ of
    Building poly -> Just poly
    _ -> Nothing

_unsafeBuilding :: Lens' State PolyBuilder
_unsafeBuilding = _unsafePrismicLens _Building

rebuild :: forall a s. Prism' s a -> (a -> s) -> (s -> s)
rebuild p f s = case preview p s of
  Nothing -> s
  Just a -> f a

_proceed :: State -> State
_proceed = rebuild _Building \poly ->
  Deriving { polynomial: build poly, derivative: 0, position: 1.0, value: "", conditions: [] }

_Deriving :: Prism' State Deriving
_Deriving = prism' Deriving case _ of
    Deriving dstate -> Just dstate
    _ -> Nothing

_unsafeDeriving :: Lens' State Deriving
_unsafeDeriving = _unsafePrismicLens _Deriving

_derivative :: Lens' Deriving Int
_derivative = prop (SProxy :: SProxy "derivative")

_polynomial :: Lens' Deriving Polynomial
_polynomial = prop (SProxy :: SProxy "polynomial")

_position :: Lens' Deriving Number
_position = prop (SProxy :: SProxy "position")

_value :: Lens' Deriving String
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
addCondition state@(Deriving st@{ polynomial, derivative, position, value: val, conditions }) =
  case parseLinear val of
    Right value -> Deriving st
      { conditions = conditions <>
          [{polynomial
          , derivative
          , position
          , value
          }]
      }
    _ -> state
addCondition state = state

degreeComponent :: forall p. State -> Element p
degreeComponent state =
  HH.div_
    [ HH.text "Degree: "
    , HL.Int.renderBounded (Just 0) (Just 10) (_unsafeBuilding <<< _degree) state
    ]

originComponent :: forall p. State -> Element p
originComponent = HL.Checkbox.renderAsField "Passes through (0,0)" (_unsafeBuilding <<< _origin)

proceedComponent :: forall p. Partial => State -> Element p
proceedComponent state@(Building polynomial) =
    HL.Button.renderAsField t _proceed (d < 2)
    --HH.button [ HE.onClick $ HE.input_ $ msg ] [ HH.text t ]
  where
    setter :: forall eff. Eff (dom :: DOM | eff) (State -> State)
    setter = pure _proceed
    msg :: forall a. a -> HL.Query State a
    msg = HL.UpdateState setter
    d = dimension polynomial
    s = if d == 1 then "" else "s"
    t = "Proceed to satisfy " <> show d <> " variable" <> s

derivativeComponent :: forall p. Partial => State -> Element p
derivativeComponent state@(Deriving { polynomial, derivative }) =
  HH.div_
    [ HL.Int.renderBounded (Just 0) (Just $ degree polynomial) (_unsafeDeriving <<< _derivative) state
    , HH.text (suff <> " derivative: ")
    , HH.text $ show $ nthderivative derivative polynomial
    ]
  where
    suff = case derivative of
      1 -> "st"
      2 -> "nd"
      3 -> "rd"
      _ -> "th"

evalRow { position, derivative, polynomial } =
  evalAt position $ nthderivative derivative polynomial

positionComponent :: forall p. Partial => State -> Element p
positionComponent state@(Deriving st) =
  HH.div_
    [ HH.text "evaluated at x = "
    , HL.Number.renderBounded (Just (-10.0)) (Just 10.0) (_unsafeDeriving <<< _position) state
    , HH.text (": " <> show (evalRow st))
    ]

valueComponent :: forall p. Partial => State -> Element p
valueComponent state@(Deriving { polynomial, derivative, position, value }) =
  HH.div_
    [ HH.text "should equal "
    , HL.Input.render (_unsafeDeriving <<< _value) state
    , HH.text (": " <> evalue)
    , HH.div_ [ HL.Button.renderAsField "Add condition" addCondition (isLeft parsed) ]
    ]
  where
    parsed = parseLinear value
    evalue = case parsed of
      Left s -> s
      Right v -> show v

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
  initialState = Building $ PolyBuilder [false, true, true]

  render :: State -> H.ComponentHTML Query
  render state@(Building polynomial) =
    HH.div_
      [ HH.h1_
          [ HH.text "Create a Polynomial" ]
      , HH.h2_
          [ HH.text "which satisfies certain properties" ]
      , degreeComponent state
      , originComponent state
      , renderPolyBuilder _unsafeBuilding state
      , HH.div_ [ HH.text $ show polynomial ]
      , HH.div_ [ HH.text $ show (build polynomial) ]
      , HH.div_ [ HH.text $ show (derivative $ build polynomial) ]
      , unsafePartial proceedComponent state
      ]
  render state@(Deriving { polynomial, derivative, position, value, conditions }) =
    HH.div_
      [ HH.h1_
          [ HH.text "Create a Polynomial" ]
      , HH.h2_
          [ HH.text "which satisfies certain properties" ]
      , HH.div_ [ HH.text ("f(x) = " <> show polynomial) ]
      , unsafePartial derivativeComponent state
      , unsafePartial positionComponent state
      , unsafePartial valueComponent state
      , HH.div_ [ HH.text ("f" <> genp derivative <> "(" <> show position <> ") = 0.0")]
      , HH.div_ $ map (HH.div_ <<< singleton) $ map (\r -> HH.text $ show (evalRow r) <> " = " <> show r.value) conditions
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval = HL.eval

separate :: forall p i. HH.HTML p i -> Array (HH.HTML p i) -> HH.HTML p i
separate sep = HH.span_ <<< intercalate [sep] <<< map Array.singleton
