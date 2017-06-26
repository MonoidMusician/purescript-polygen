module Main.Component where

import Prelude
import Data.Array as Array
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Lens as HL
import Halogen.HTML.Lens.Checkbox as HL.Checkbox
import Halogen.HTML.Lens.Input as HL.Input
import Halogen.HTML.Lens.Int as HL.Int
import Halogen.HTML.Lens.TextArea as HL.TextArea
import Halogen.HTML.Properties as HP
import Control.Monad.Aff (Aff)
import DOM (DOM)
import Data.Array (cons, drop, head, intercalate, length, replicate, tail, take)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Suggestion (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Main.PolyBuilder

type Query = HL.Query State

type Element p = H.HTML p Query

type State =
  { polynomial :: PolyBuilder
  , origin :: Boolean
  }

_polynomial :: Lens' State PolyBuilder
_polynomial = prop (SProxy :: SProxy "polynomial")

expand :: forall a. a -> Array a -> Int -> Array a
expand pad array to = replicate d pad <> drop (-d) array
  where
    l = length array
    d = to - l

expand' :: forall a. a -> Array a -> Int -> Array a
expand' pad array to = take to array <> replicate (to - length array) pad

_offbyone :: Lens' Int Int
_offbyone = lens (_-1) \_ -> (_+1)

_degree :: Lens' State Int
_degree = _polynomial <<< _Newtype <<< lens length (expand' true) <<< _offbyone

_origin :: Lens' State Boolean
_origin = _polynomial <<< _Newtype <<< lens
  (\poly -> case head poly of
    Just true -> false
    _ -> true
  )
  (\poly origin -> fromMaybe poly (cons (not origin) <$> tail poly))

degreeComponent :: forall p. State -> Element p
degreeComponent state =
  HH.div_
    [ HH.text "Degree: "
    , HL.Int.renderBounded (Just 0) (Just 10) _degree state
    ]

originComponent :: forall p. State -> Element p
originComponent = HL.Checkbox.renderAsField "Passes through (0,0)" _origin

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
    { polynomial: PolyBuilder [false, true, true, false]
    , origin: true
    }

  render :: State -> H.ComponentHTML Query
  render state@{ polynomial } =
    HH.div_
      [ HH.h1_
          [ HH.text "Create a Polynomial satisfying certain properties" ]
      , degreeComponent state
      , originComponent state
      , renderPolyBuilder _polynomial state
      ]

  eval :: Query ~> H.ComponentDSL State Query Void (Aff (dom :: DOM | eff))
  eval = HL.eval

separate :: forall p i. HH.HTML p i -> Array (HH.HTML p i) -> HH.HTML p i
separate sep = HH.span_ <<< intercalate [sep] <<< map Array.singleton
