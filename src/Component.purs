module Main.Component where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.Node.ParentNode (QuerySelector)
import Data.Array (intercalate, zip)
import Data.Array as Arr
import Data.Either (Either(Left))
import Data.Either.Nested (Either2)
import Data.Foldable (foldMap, for_)
import Data.Function.Uncurried (Fn2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Lens.Lens (lens)
import Data.Lens.Record (prop)
import Data.Lens.Suggestion (Lens')
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord.Down (Down(..))
import Data.Rational.Big (BigRational, toNumber, fromNumber)
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Data.Traversable (maximum)
import Data.Tuple (Tuple(..), snd)
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Lens as HL
import Halogen.HTML.Lens.Input as HL.Input
import Halogen.HTML.Lens.Int as HL.Int
import Halogen.HTML.Lens.Number as HL.Number
import Halogen.HTML.Properties as HP
import Main.Matrix (Matrix, mkMatrix, unMatrix, inverse, matProduct)
import Main.Polynomials (Atom(..), Polynomial, Row(..), Table, Variable, simplify, calcRow, constant, degree, discardVariables, disp, evalAt, freeVariables, gather, genp, lookup, lookupIn, mkRow, mkSpecialized, mkTable, mmkAtom, nthderivative, parseLinear, parseLinear_, setCoefficient, showcode, showGLSL, substitute, variable, zeroRow)
import Partial.Unsafe (unsafePartial)
import Prelude hiding (degree)
import Unsafe.Coerce (unsafeCoerce)

type AffDOM eff = Aff ( dom :: DOM | eff )

data Query a
  = DeleteCondition ConditionKey a
  | InsertCondition Condition a
  | UpdateState (HL.Query State a)
  | SendSubquery (forall b. b -> Subquery b) a

data Subquery a
  = UpdateSubstate (HL.Query Substate a)
  | SetSubstate Substate a
  | AddCondition a

type Submessage = Condition

type ChildrenQuery = Coproduct2 Subquery GraphQuery
type Slot = Either2 Unit Unit

type Element p = H.HTML p Query
type LensComponent = forall p. Substate -> H.HTML p Subquery

type Substate =
  { derivative :: Int
  , position :: Number
  , value :: String
  }
type State =
  { conditions :: ConditionMap
  , computed :: Maybe Computed
  , variables :: Map Variable Number
  , evaluate ::
      { derivative :: Int
      , position :: Number
      }
  }
type ConditionMap = Map ConditionKey Row
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
    comparing (_.derivative >>> Down) l r
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
  , graphSafe :: Polynomial
  )

_derivative :: Lens' Substate Int
_derivative = prop (SProxy :: SProxy "derivative")

_position :: Lens' Substate Number
_position = prop (SProxy :: SProxy "position")

_value :: Lens' Substate String
_value = prop (SProxy :: SProxy "value")

_conditions :: Lens' State ConditionMap
_conditions = prop (SProxy :: SProxy "conditions")

_evaluate :: Lens' State { derivative :: Int, position :: Number }
_evaluate = prop (SProxy :: SProxy "evaluate")

_evalderivative :: Lens' State Int
_evalderivative = _evaluate <<< prop (SProxy :: SProxy "derivative")

_evalposition :: Lens' State Number
_evalposition = _evaluate <<< prop (SProxy :: SProxy "position")

ckey :: forall r. ConditionKeyBase r -> ConditionKey
ckey { derivative, position } = CKey { derivative, position }

cvalue :: Tuple ConditionKey Row -> Condition
cvalue (Tuple (CKey { derivative, position }) value) =
  { derivative, position, value }

clist :: ConditionMap -> Conditions
clist m = Map.toAscUnfoldable m <#> cvalue

removeCondition :: ConditionKey -> ConditionMap -> ConditionMap
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
    , HH.text ": "
    , evalue
    ]
  where
    parsed = parseLinear value
    parsed' = parseLinear_ value
    parsedLens :: Atom -> Lens' Substate Number
    parsedLens k = prop (SProxy :: SProxy "value") <<< lens
      (\val -> fromMaybe 0.0 $
        (if val == value then parsed' else parseLinear_ val)
        <#> lookup k
      )
      (\val v -> fromMaybe val $
        (if val == value then parsed' else parseLinear_ val)
        <#> setCoefficient k v >>> show
      )
    evalue = case parsed, parsed' of
      Left s, _ ->
        HH.div
          [ HP.class_ $ wrap "error" ]
          [ HH.text s ]
      _, Just v@(Row r) ->
        let
          r' = Map.alter (Just <<< fromMaybe 0.0) K r
          fields = Map.toAscUnfoldable (r') <#> \(Tuple k _) ->
            HH.span_
              [ map UpdateSubstate $ HL.Number.render (parsedLens k) state
              , HH.text $ show k
              ]
        in HH.div_ $ HH.div_ <<< pure <$> ([HH.text $ show v] <> fields)
      _, _ -> HH.text "parsing failed"

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

compute :: State -> Maybe Computed
compute { conditions: cs }
  | Just largest <- maximum $ Map.keys cs <#> \(CKey { derivative }) -> derivative
  , largest >= Map.size cs
    = Nothing
compute { conditions: cs, variables } = Just
    { polynomial, conditions
    , params, values
    , coefficientM, valueM
    , coefficientMI, productM
    , result, substituted
    , graphSafe
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
    substituted = simplify $ substitute polynomial result
    varTable = mkVarTable variables
    graphSafe = discardVariables $ simplify $ substitute substituted varTable

mkVarTable :: Map Variable Number -> Table
mkVarTable variables = mkTable $ variables # Map.toUnfoldable <#> \(Tuple k v) ->
  Tuple (V k) (constant v)

addingComponent :: forall eff. H.Component HH.HTML Subquery Unit Submessage (AffDOM eff)
addingComponent =
  H.component
    { initialState: const
        { derivative: 0
        , position: 0.0
        , value: "0"
        }
    , render
    , eval
    , receiver: const Nothing
    }
  where

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
  eval (SetSubstate substate a) = pure a <* H.put substate
  eval (UpdateSubstate (HL.UpdateState run a)) = pure a <* do
    reset <- H.liftEff run
    H.modify reset
  eval (AddCondition a) = pure a <* do
    state@{ value: val } <- H.get
    for_ (parseLinear val) \value ->
      H.raise state { value = value }

showf :: forall r. { derivative :: Int, position :: Number | r } -> String
showf { derivative: d, position: p } =
  "f" <> genp d <> "(" <> show p <> ")"

reviseConditions :: (ConditionMap -> ConditionMap) -> (State -> State)
reviseConditions f state =
  let conditions = f state.conditions in
  if conditions == state.conditions
    then state
    else let
      state' = state { conditions = conditions }
      computed = compute state'
    in state'
      { computed = computed
      , variables = fromMaybe state.variables $ computed <#> \{ substituted } ->
          Map.fromFoldable $ freeVariables substituted <#> \k ->
            Tuple k $ fromMaybe 0.0 $ Map.lookup k state.variables
      }

computePoints :: Map Variable Number -> ConditionMap -> Array (Tuple Number Number)
computePoints variables = Map.toUnfoldable >>> Arr.mapMaybe
  \(Tuple (CKey { derivative, position }) value) ->
    if derivative /= 0 then Nothing else Just $
      Tuple position $ calcRow variables value

initialState :: State
initialState =
  reviseConditions (const $
    Map.fromFoldable $ (\c -> Tuple (ckey c) c.value) <$>
      [ { derivative: 0, position: 0.0, value: zeroRow }
      , { derivative: 0, position: 0.5, value: constant one }
      , { derivative: 1, position: 0.5, value: zeroRow }
      , { derivative: 0, position: 1.0, value: variable (unsafePartial fromJust (mmkAtom "h")) }
      ]) { conditions: Map.empty, computed: Nothing, variables: Map.empty, evaluate: { derivative: 0, position: 0.0 } }

component :: forall eff. H.Component HH.HTML Query Unit Void (AffDOM eff)
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ParentHTML Query ChildrenQuery Slot (AffDOM eff)
  render state@{ conditions: cs, computed, variables } =
    HH.div_ $
      [ HH.h1_
          [ HH.text "Create a Polynomial" ]
      , HH.h2_
          [ HH.text "which satisfies certain properties" ]
      , HH.slot' cp1 unit addingComponent unit (HE.input InsertCondition)
      , HH.slot' cp2 unit graphComponent unit absurd
      ] <>
        (computed # foldMap (case _ of {polynomial} -> [ HH.div_ [ HH.text ("f(x) = " <> show polynomial) ] ]))
        <>
      [ HH.br_
      , HH.div_ $ pure $ HH.text datapoints
      , HH.br_
      , HH.div_ $ snd <$> Map.toAscUnfoldable conditionsDisplayed
      , HH.br_
      ] <> (computed # flip foldMap) case _ of
        { polynomial, conditions
        , params, values
        , coefficientM, valueM
        , coefficientMI, productM
        , result, substituted
        , graphSafe
        } ->
          [ rowTable $ conditions
          , HH.div_
              [ HH.text $ show result
              , HH.br_
              , HH.text $ show substituted
              ]
          , HH.br_
          , HH.div_ variablesDisplayed
          , HH.br_
          , HH.text "Evaluate derivative "
          , map UpdateState $ HL.Int.renderBounded (Just 0) (Just $ degree polynomial) _evalderivative state
          , HH.text " at x = "
          , map UpdateState $ HL.Number.renderBounded Nothing Nothing _evalposition state
          , HH.br_
          , HH.text let
              calculate =
                nthderivative state.evaluate.derivative
                  >>> evalAt state.evaluate.position
            in showf state.evaluate <> " = " <>
              joinWith " = " (map show $ Arr.nub $ calculate <$> [polynomial, substituted, graphSafe])
          , HH.br_
          , HH.pre_ $ pure $ HH.text $ Arr.intercalate "\n"
              [ "f(x) = " <> showcode polynomial
              , "     = " <> showcode substituted
              , "     = " <> showGLSL substituted
              , "f'(x) =" <> showcode (nthderivative 1 substituted)
              , "f(x) = " <> showcode graphSafe
              , "     = " <> showGLSL graphSafe
              ]
          ]
      where
        getParameters :: ConditionKey -> Maybe Row
        getParameters key = computed >>=
          \{ conditions } ->
            Arr.find (ckey >>> eq key) conditions >>= _.parameters
        conditionsDisplayed = cs # Map.mapWithKey
          \key -> removeIth (getParameters key) key
        showc key value mparameters =
          showf key <> " = "
            <> (mparameters # foldMap \ps -> show ps <> " = ")
            <> show value
        removeIth mparameters key value =
          HH.div_
            [ HH.button
                [ HE.onClick (HE.input_ $ DeleteCondition key) ]
                [ HH.text "\x2212" ]
            , HH.span
                [ HE.onClick (HE.input_ $ SendSubquery (SetSubstate substate)) ]
                [ HH.text $ " " <> showc (unwrap key) value mparameters ]
            ]
          where
            substate = case key of
              CKey { derivative: d, position: x } ->
                { derivative: d, position: x, value: show value }
        c2d :: Map Number (Map Int Row)
        c2d =
          Map.fromFoldableWith (<>) $ cs # Map.mapWithKey
            \(CKey { derivative, position}) value ->
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
        --variableLens :: Lens' State Number
        variableLens k = lens
          (\st -> fromMaybe 0.0 $ Map.lookup k st.variables)
          (\st v -> st { variables = Map.insert k v st.variables })
        variablesDisplayed = map snd $ Map.toAscUnfoldable $ variables # Map.mapWithKey \k v ->
          HH.div_
            [ HH.text $ show k <> " = "
            , map UpdateState $ HL.Number.renderBounded Nothing Nothing (variableLens k) state
            ]

  updateConditions :: (ConditionMap -> ConditionMap) -> H.ParentDSL State Query ChildrenQuery Slot Void (AffDOM eff) Unit
  updateConditions f = do
    H.modify $ reviseConditions f
    updateGraph

  updateGraph :: H.ParentDSL State Query ChildrenQuery Slot Void (AffDOM eff) Unit
  updateGraph = do
    { conditions, variables } <- H.get
    let points = computePoints variables conditions
    p <- H.gets $ _.computed >>> map _.graphSafe
    for_ p $ H.query' cp2 unit <<< H.action <<< SetPolynomial points

  eval :: Query ~> H.ParentDSL State Query ChildrenQuery Slot Void (AffDOM eff)
  eval (UpdateState q) = HL.eval q <* do
    H.modify case _ of
      state@{ computed: Just computed, variables } ->
        let
          varTable = mkVarTable variables
          graphSafe = discardVariables $ simplify $ substitute computed.substituted varTable
        in state { computed = Just (computed { graphSafe = graphSafe })}
      state -> state
    updateGraph
  eval (DeleteCondition key a) =
    pure a <* updateConditions (Map.delete key)
  eval (InsertCondition c a) =
    pure a <* updateConditions (Map.insert (ckey c) c.value)
  eval (SendSubquery query a) =
    pure a <* H.query' cp1 unit (H.action query)


separate :: forall p i. HH.HTML p i -> Array (HH.HTML p i) -> HH.HTML p i
separate sep = HH.span_ <<< intercalate [sep] <<< map Arr.singleton

type GraphState = Maybe Plot
type Points = Array (Tuple Number Number)
data GraphQuery a = Initialize a | SetPolynomial Points Polynomial a

graphComponent :: forall eff. H.Component HH.HTML GraphQuery Unit Void (AffDOM eff)
graphComponent =
  H.lifecycleComponent
    { initialState: const Nothing
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  render :: GraphState -> H.ComponentHTML GraphQuery
  render _ =
    HH.div
      [ HP.id_ "graph" ]
      []

  eval :: GraphQuery ~> H.ComponentDSL GraphState GraphQuery Void (AffDOM eff)
  eval (SetPolynomial pts p a) = do
    H.get >>= case _ of
      Nothing -> pure unit
      Just graph -> H.liftEff do
        let
          graphData =
            [ { graphType: "polyline"
              , fn: showcode p
              , derivative:
                  { fn: showcode (nthderivative 1 p)
                  , updateOnMouseMove: true
                  }
              }
            , { graphType: "scatter"
              , fnType: "points"
              , points: pts <#> \(Tuple x y) -> [x, y]
              } # unsafeCoerce
            ]
        setData graph graphData
        draw graph
    pure a
  eval (Initialize a) = do
    let
      computed = unsafePartial fromJust initialState.computed
      p = computed.graphSafe
      pts = computePoints initialState.variables initialState.conditions
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
          }
        }
    plot <- H.liftEff $ functionPlot options
    H.modify (const (Just plot))
    eval (SetPolynomial pts p a)

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
type Datum = forall r. Record r
  {- graphType :: String
  , fn :: String
  , derivative ::
      { fn :: String
      , updateOnMouseMove :: Boolean
      }
  -}
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
  forall r eff.
  Plot -> Array (Record r) -> Eff (dom :: DOM | eff) Unit
setData = setDataImpl

foreign import draw :: forall eff. Plot -> Eff (dom :: DOM | eff) Unit
