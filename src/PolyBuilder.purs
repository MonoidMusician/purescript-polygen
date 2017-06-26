module Main.PolyBuilder where

import Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Lens.Checkbox as HL.Checkbox
import Data.Array (index, null, uncons, updateAt)
import Data.Char (fromCharCode, toCharCode)
import Data.Lens (iso, lens, (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Types (Iso', Lens')
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (singleton)
import Halogen.HTML.Lens (Query)

type Element s p = H.HTML p (Query s)

newtype PolyBuilder = PolyBuilder (Array Boolean)
derive instance newtypePolyBuilder :: Newtype PolyBuilder _

genx :: Int -> String
genx 0 = ""
genx 1 = "x"
genx i = "x" <> geni i

geni :: Int -> String
geni 0 = "⁰"
geni 1 = "¹"
geni 2 = "²"
geni 3 = "³"
geni i | i >= 4 && i < 10 =
  singleton $ _CharCode (\base -> base + i - 4) '⁴'
geni i = geni (i `div` 10) <> geni (i `mod` 10)

_CharCode :: Iso' Char Int
_CharCode = iso toCharCode fromCharCode

instance showPolyBuilder :: Show PolyBuilder where
  show (PolyBuilder poly) = go poly 'a' 0 "" where
    go a c i s = case uncons a of
      Nothing -> s
      Just { head: false, tail: a' } ->
        go a' c (i+1) $ s <> op <> "0" <> genx i
      Just { head: true, tail: a' } ->
        let
          var = singleton c
          c' = _CharCode (_ + 1) c
        in go a' c' (i+1) $ s <> op <> var <> genx i
      where op = if s == "" then "" else "+"

indexed :: Int -> Lens' PolyBuilder Boolean
indexed i = _Newtype <<< lens
  (flip index i >>> fromMaybe false)
  (\poly b -> updateAt i b poly # fromMaybe poly)

renderPolyBuilder :: forall s p. Lens' s PolyBuilder -> s -> Element s p
renderPolyBuilder l s = HH.div_ $ go (unwrap (s ^. l)) 'a' 0 []
  where
    go a c i r = case uncons a of
      Nothing -> r
      Just { head: b, tail: a' } ->
        let
          var = if b then singleton c else "0"
          c' = if b then _CharCode (_ + 1) c else c
          op = HH.text if null r then "" else "+"
          cb = HL.Checkbox.render (l <<< indexed i) s
        in go a' c' (i+1) $
          r <> [op, cb, HH.text (var <> genx i)]
