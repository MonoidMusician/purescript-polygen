module Main.Component where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Array (intercalate, zip)
import Data.Array as Arr
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Either.Nested (Either2)
import Data.Function.Uncurried (Fn2)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Lens.Record (prop)
import Data.Lens.Suggestion (Lens')
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Rational.Big (BigRational, toNumber, fromNumber)
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Lens as HL
import Halogen.HTML.Lens.Button as HL.Button
import Halogen.HTML.Lens.Checkbox as HL.Checkbox
import Halogen.HTML.Lens.Input as HL.Input
import Halogen.HTML.Lens.Int as HL.Int
import Halogen.HTML.Lens.Number as HL.Number
import Halogen.HTML.Properties as HP
import Main.Matrix (Matrix, mkMatrix, unMatrix, inverse, matProduct)
import Main.Polynomials (Atom, Polynomial, Row, Table, showcode, constant, disp, evalAt, gather, genp, lookupIn, mkRow, mkSpecialized, mkTable, nthderivative, parseLinear, substitute, zeroRow)
import Prelude hiding (degree)

type AffDOM eff = Aff ( dom :: DOM | eff )

data Query a
  = UpdateState (HL.Query State a)
  | InsertCondition Condition a

data Subquery a
  = UpdateSubstate (HL.Query Substate a)
  | AddCondition a

type Submessage = Condition

type ChildrenQuery = Subquery <\/> GraphQuery <\/> Const Void
type Slot = Either2 Unit Unit

type Element p = H.HTML p Query
type LensComponent = forall p. Substate -> H.HTML p Subquery

type Substate =
  { derivative :: Int
  , position :: Number
  , value :: String
  }
type State = Map ConditionKey Row
type ConditionKeyBase r =
  { derivative :: Int
  , position :: Number
  | r
  }
newtype ConditionKey = CKey (ConditionKeyBase ())
derive instance newtypeConditionKey :: Newtype ConditionKey _
instance eqConditionKey :: Eq ConditionKey where
  eq = map (eq EQ) <$> compare
instance ordConditionKey :: Ord ConditionKey where
  compare (CKey l) (CKey r) =
    comparing _.derivative l r
      <> comparing _.position l r
type ConditionBase r = ConditionKeyBase
  ( value :: Row
  | r
  )
type Condition = ConditionBase ()
type Conditions = Array Condition
type ConditionPlus =
  ConditionBase
    ( parameters :: Maybe Row
    , polynomial :: Polynomial
    )
type ConditionsPlus = Array ConditionPlus
type Computed = Record
  ( polynomial :: Polynomial
  , conditions :: ConditionsPlus
  , params :: Array Atom
  , values :: Array Atom
  , coefficientM :: Matrix BigRational
  , valueM :: Matrix BigRational
  , coefficientMI :: Matrix BigRational
  , productM :: Matrix BigRational
  , result :: Table
  , substituted :: Polynomial
  )

_derivative :: Lens' Substate Int
_derivative = prop (SProxy :: SProxy "derivative")

_position :: Lens' Substate Number
_position = prop (SProxy :: SProxy "position")

_value :: Lens' Substate String
_value = prop (SProxy :: SProxy "value")

ckey :: forall r. ConditionKeyBase r -> ConditionKey
ckey { derivative, position } = CKey { derivative, position }

cvalue :: Tuple ConditionKey Row -> Condition
cvalue (Tuple (CKey { derivative, position }) value) =
  { derivative, position, value }

clist :: State -> Conditions
clist m = Map.toAscUnfoldable m <#> cvalue

removeCondition :: ConditionKey -> State -> State
removeCondition k = Map.delete k

derivativeComponent :: LensComponent
derivativeComponent state@{ derivative } =
  HH.div_
    [ map UpdateSubstate $ HL.Int.renderBounded (Just 0) (Just 20) _derivative state
    , HH.text (suff <> " derivative: ")
    --, HH.text $ show $ nthderivative derivative polynomial
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
positionComponent state{- polynomial -} =
  HH.div_
    [ HH.text "evaluated at x = "
    , map UpdateSubstate $ HL.Number.renderBounded (Just (-10.0)) (Just 10.0) _position state
    --, HH.text (": " <> show (evalRow polynomial state))
    ]

valueComponent :: LensComponent
valueComponent state@{ derivative, position, value } =
  HH.div_
    [ HH.text "should equal "
    , map UpdateSubstate $ HL.Input.render _value state
    , HH.text (": " <> evalue)
    ]
  where
    parsed = parseLinear value
    evalue = case parsed of
      Left s -> s
      Right v -> show v

specialization :: Conditions -> Array Int
specialization =
  Arr.mapMaybe case _ of
    r@{ derivative: d } | trivial r ->
      Just d
    _ -> Nothing

trivial :: forall r. ConditionBase r -> Boolean
trivial r =
  r.position == 0.0 && r.value == zeroRow

nonTrivials :: forall r. Array (ConditionBase r) -> Array (ConditionBase r)
nonTrivials = Arr.filter (not trivial)

rowTable :: forall p. ConditionsPlus -> Element p
rowTable rows =
  HH.table [ HP.class_ $ wrap "row-table"]
    $ map (HH.tr_ <<< map (HH.td_ <<< Arr.singleton <<< HH.text))
        (_header Arr.: _rows)
  where
    listWith r = map (lookupIn r >>> disp)
    params = gather $ Arr.catMaybes $ map _.parameters rows
    values = gather $ map _.value $ nonTrivials rows
    _header :: Array String
    _header = [""] <> map show params <> ["="] <> map show values
    _rows :: Array (Array String)
    _rows = Arr.catMaybes $ rows # Arr.mapWithIndex \i { parameters: psm, value: vs } ->
      psm # map \ps ->
        [show (i+1) <> "."] <> listWith ps params <> ["="] <> listWith vs values

toMatrix :: Array Atom -> Array Row -> Matrix BigRational
toMatrix values rows = mkMatrix $ map (\r -> map (lookupIn r >>> fromNumber) values) rows

fromMatrix :: Array Atom -> Matrix BigRational -> Array Row
fromMatrix values matrix = map (mkRow <<< zip values <<< map toNumber) $ unMatrix matrix

compute :: State -> Computed
compute cs =
    { polynomial, conditions
    , params, values
    , coefficientM, valueM
    , coefficientMI, productM
    , result, substituted
    }
  where
    polynomial = mkSpecialized (Map.size cs) $ specialization $ clist cs
    conditions = clist cs # map
      \c@{ derivative: d, position: p, value: v } ->
        { derivative: d, position: p, value: v
        , polynomial
        , parameters:
            if trivial c
            then Nothing
            else Just $ evalAt p $
              nthderivative d polynomial
        }
    paramRs = Arr.catMaybes $ map _.parameters conditions
    valueRs = map _.value $ nonTrivials conditions
    params = gather paramRs
    values = gather valueRs
    coefficientM = toMatrix params paramRs
    valueM = toMatrix values valueRs
    coefficientMI = inverse coefficientM
    productM = coefficientMI `matProduct` valueM
    result = mkTable $ zip params $ fromMatrix values productM
    substituted = substitute polynomial result

addingComponent :: forall eff. H.Component HH.HTML Subquery Unit Submessage (AffDOM eff)
addingComponent =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: Substate
  initialState =
    { derivative: 0
    , position: 0.0
    , value: "0"
    }

  render :: Substate -> H.ComponentHTML Subquery
  render substate@{ derivative, position, value } =
    HH.div
      [ HP.class_ $ wrap "adding-component" ]
      [ HH.h3_ [ HH.text "Add property" ]
      , derivativeComponent substate
      , positionComponent substate
      , valueComponent substate
      , HH.div_ [ HH.text (showf substate <> " = " <> value)]
      , HH.button [ HE.onClick (HE.input_ AddCondition) ]
          [ HH.text "Add property" ]
      ]

  eval :: Subquery ~> H.ComponentDSL Substate Subquery Submessage (AffDOM eff)
  eval (UpdateSubstate (HL.UpdateState run next)) = do
    reset <- H.liftEff run
    H.modify reset
    pure next
  eval (AddCondition a) = do
    state@{ value: val } <- H.get
    case parseLinear val of
      Right value -> H.raise state { value = value }
      Left _ -> pure unit
    pure a

showf :: forall r. { derivative :: Int, position :: Number | r } -> String
showf { derivative: d, position: p } =
  "f" <> genp d <> "(" <> show p <> ")"

component :: forall eff. H.Component HH.HTML Query Unit Void (AffDOM eff)
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = Map.fromFoldable $ (\c -> Tuple (ckey c) c.value) <$>
    [ { derivative: 0, position: 0.0, value: zeroRow }
    , { derivative: 0, position: 1.0, value: constant 1.0 }
    , { derivative: 1, position: 0.0, value: zeroRow }
    , { derivative: 1, position: 1.0, value: zeroRow }
    , { derivative: 2, position: 0.0, value: zeroRow }
    , { derivative: 2, position: 1.0, value: zeroRow }
    ]

  render :: State -> H.ParentHTML Query ChildrenQuery Slot (AffDOM eff)
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Create a Polynomial" ]
      , HH.h2_
          [ HH.text "which satisfies certain properties" ]
      , HH.slot' cp1 unit addingComponent unit (HE.input InsertCondition)
      , HH.slot' cp2 unit graphComponent unit absurd
      , HH.div_ [ HH.text ("f(x) = " <> show polynomial) ]
      , HH.br_
      , HH.div_ $ pure $ HH.text datapoints
      , HH.br_
      , HH.div_ $ conditionsDisplayed
      , HH.br_
      , rowTable $ conditions
      , HH.div_
          [ HH.text $ show result
          , HH.br_
          , HH.text $ show substituted
          ]
      , HH.br_
      , HH.pre_ $ pure $ HH.text $ Arr.intercalate "\n" $ showcode <$>
          [ polynomial
          , substituted
          , nthderivative 1 substituted
          ]
      ]
      where
        computed@
          { polynomial, conditions
          , params, values
          , coefficientM, valueM
          , coefficientMI, productM
          , result, substituted
          } = compute state
        conditionsDisplayed = conditions <#> removeIth
        showc = case _ of
          c@{ parameters: Just ps } ->
            showf c <> " = " <> show ps <> " = " <> show c.value
          c ->
            showf c <> " = " <> show c.value
        removeIth c =
          HH.div_
            [ map UpdateState $
                HL.Button.renderAsField "\x2212"
                  (removeCondition $ ckey c) false
            , HH.text $ " " <> showc c ]
        c2d :: Map Number (Map Int Row)
        c2d =
          Map.fromFoldableWith (<>) $ conditions <#>
            \{ derivative, position, value } ->
              Tuple position (Map.singleton derivative value)
        byIndex :: forall a. Map Int a -> Array (Maybe a)
        byIndex m | Map.isEmpty m = []
        byIndex m = 0 Arr... maybe 0 _.key (Map.findMax m) <#> flip Map.lookup m
        datapoints = intercalate ", " $ (Map.toAscUnfoldable c2d :: Array (Tuple Number (Map Int Row)))
          <#> \(Tuple position derivatives) ->
            byIndex derivatives
              <#> maybe "_" show
              # append [show position]
              # \vs -> "(" <> joinWith ", " vs <> ")"

  eval :: Query ~> H.ParentDSL State Query ChildrenQuery Slot Void (AffDOM eff)
  eval (UpdateState (HL.UpdateState run next)) = do
    reset <- H.liftEff run
    H.modify reset
    p <- H.gets $ compute >>> _.substituted
    _ <- H.query' cp2 unit $ H.action (SetPolynomial p)
    pure next
  eval (InsertCondition c a) = do
    H.modify $ ckey c # Map.alter (const (Just c.value))
    p <- H.gets $ compute >>> _.substituted
    _ <- H.query' cp2 unit $ H.action (SetPolynomial p)
    pure a

separate :: forall p i. HH.HTML p i -> Array (HH.HTML p i) -> HH.HTML p i
separate sep = HH.span_ <<< intercalate [sep] <<< map Arr.singleton

type GraphState = Maybe Plot
data GraphQuery a = Initialize a | SetPolynomial Polynomial a

graphComponent :: forall eff. H.Component HH.HTML GraphQuery Unit Void (AffDOM eff)
graphComponent =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: GraphState
  initialState = Nothing

  render :: GraphState -> H.ComponentHTML GraphQuery
  render _ =
    HH.div
      [ HP.id_ "graph" ]
      []

  eval :: GraphQuery ~> H.ComponentDSL GraphState GraphQuery Void (AffDOM eff)
  eval (SetPolynomial p a) = do
    H.get >>= case _ of
      Nothing -> pure unit
      Just graph -> H.liftEff do
        let
          graphData =
            [{ graphType: "polyline"
            , fn: showcode p
            , derivative:
                { fn: showcode (nthderivative 1 p)
                , updateOnMouseMove: true
                }
            }]
        setData graph graphData
        draw graph
    pure a
  eval (Initialize a) = do
    let
      options =
        {
          target: wrap "#graph",
          xAxis: {
            "type": "linear",
            domain: [0.0, 1.0],
            invert: false,
            label: ""
          },
          yAxis: {
            "type": "linear",
            domain: [0.0, 1.0],
            invert: false,
            label: ""
          },
          "data": [{
            graphType: "polyline",
            fn: "6.0*x^5 - 15.0*x^4 + 10.0*x^3",
            derivative: {
              fn: "30.0*x^4 - 60.0*x^3 + 30.0*x^2",
              updateOnMouseMove: true
            }
          }]
        }
    plot <- H.liftEff $ functionPlot options
    H.modify (const (Just plot))
    pure a

type AxisOptions =
  { type :: String
  , domain :: Array Number
  , invert :: Boolean
  , label :: String
  }
type Options =
  ( target :: QuerySelector
  , title :: String
  , xAxis :: AxisOptions
  , yAxis :: AxisOptions
  , disableZoom :: Boolean
  , grid :: Boolean
  , tip ::
      { xLine :: Boolean
      , yLine :: Boolean
      , renderer :: Fn2 Number Number String
      }
  , annotations :: Array
      { x :: Number
      , y :: Number
      , text :: String
      }
  , "data" :: Data
  )
type Data = Array Datum
type Datum =
  { graphType :: String
  , fn :: String
  , derivative ::
      { fn :: String
      , updateOnMouseMove :: Boolean
      }
  }
  {- title :: String
  , skipTip :: Boolean
  , range :: Array Number
  , nSamples :: Int
  , graphType :: String
  , fnType :: String
  , sampler :: String
  -}

foreign import data Plot :: Type
foreign import functionPlotImpl ::
  forall r eff. Record r -> Eff (dom :: DOM | eff) Plot
functionPlot ::
  forall r fill eff.
    Union r fill Options =>
  Record r -> Eff (dom :: DOM | eff) Plot
functionPlot = functionPlotImpl

foreign import setOptionsImpl ::
  forall r eff. Plot -> Record r -> Eff (dom :: DOM | eff) Unit
setOptions ::
  forall r fill eff.
    Union r fill Options =>
  Plot -> Record r -> Eff (dom :: DOM | eff) Unit
setOptions = setOptionsImpl

foreign import setDataImpl ::
  forall r eff. Plot -> Array (Record r) -> Eff (dom :: DOM | eff) Unit
setData ::
  forall r fill eff.
  Plot -> Array (Record r) -> Eff (dom :: DOM | eff) Unit
setData = setDataImpl

foreign import draw :: forall eff. Plot -> Eff (dom :: DOM | eff) Unit
