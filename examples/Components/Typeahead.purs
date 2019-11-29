module Components.Typeahead where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AR
import Components.Dropdown as D
import Data.Argonaut.Decode ((.:), decodeJson)
import Data.Array (mapWithIndex, filter, (:), (!!), length, null, difference)
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Internal.CSS (class_, classes_, whenElem)
import Internal.RemoteData as RD
import Record.Builder (Builder)
import Record.Builder as Builder
import HalogenModular (caseV, injV, onV)
import HalogenModular as HM
import Select (HS_ACTION, HS_INPUT, HS_STATE, _halogenSelect)
import Select as S
import Select.Setters as SS
import Type.Row (type (+))

_component :: SProxy "mainComponent"
_component = SProxy

type Input = Unit

type State = { | StateRows }
type StateRows_ r =
  ( selections :: Array Location
  , available :: RD.RemoteData String (Array Location)
  | r
  )
type PipelineRows =
  ( HS_INPUT
  + StateRows_
  + ()
  )

type StateRows =
  ( HS_STATE
  + StateRows_
  + ()
  )

type Action = Variant ActionRows
data Action_
  = Initialize
  | Remove Location
  | HandleDropdown D.Message

type ActionRows =
  ( mainComponent :: Action_
  | HS_ACTION
  + ()
  )

data Query a
  = GetSelections (Array Location -> a)

data Message
  = ItemRemoved Location
  | SelectionsChanged (Array Location)

type ChildSlots =
  ( dropdown :: D.SelfSlot Unit )

type Monad = Aff
type SelfSlot index = H.Slot Query Message index

component :: H.Component HH.HTML Query Input Message Monad
component = HM.component (Builder.build pipeline <<< inputToPipeline) $ HM.defaultSpec
  { render = render
  , handleAction =
      caseV
        -- 3rd-party renderless components' actions
        # onV _halogenSelect (S.handleHalogenSelectAction handleHalogenSelectEvent)

        -- main component's actions
        # onV _component handleMainAction
  , handleQuery = handleMainQuery
  , initialize = Just $ injV _component Initialize
  }
  where
    inputToPipeline :: Input -> { | PipelineRows }
    inputToPipeline _ =
      -- labels for main component's non-3rd-party-library state
      { selections: []
      , available: RD.NotAsked

      -- == Labels for 3rd-party renderless components ==
      -- Halogen Select input
      , inputType: S.Text
      , search: Nothing
      , debounceTime: Just (Milliseconds 300.0)
      , getItemCount: maybe 0 length <<< RD.toMaybe <<< _.available

      -- Other library input
      }

    -- 3rd-party renderless components' `input -> state` functions
    pipeline :: Builder { | PipelineRows } { | StateRows }
    pipeline = S.mkHalogenSelectInput
      -- >>> OtherComponent.mkInput

    render :: State -> H.ComponentHTML Action ChildSlots Monad
    render st =
      HH.div
        [ class_ "Typeahead" ]
        [ renderSelections, renderInput, renderDropdown, renderContainer ]
      where
      hasSelections = length st.selections > 0

      renderSelections = whenElem hasSelections \_ ->
        HH.div
          [ class_ "Typeahead__selections" ]
          (renderSelectedItem <$> st.selections)
        where
        renderSelectedItem item =
          HH.div
            [ class_ "Typeahead__item--selected Location" ]
            [ HH.span
                [ class_ "Location__name" ]
                [ HH.text item.name ]
            , closeButton item
            ]

        closeButton item =
          HH.span
            [ class_ "Location__closeButton"
            , HE.onClick \_ -> Just $ injV _component $ Remove item
            ]
            [ HH.text "×" ]

      renderInput = HH.input $ SS.setInputProps
        [ classes_
            [ "Typeahead__input"
            , "Typeahead__input--selections" # guard hasSelections
            , "Typeahead__input--active" # guard (st.visibility == S.On)
            ]
        , HP.placeholder "Type to search..."
        ]

      renderDropdown = whenElem (st.visibility == S.On) \_ ->
        HH.slot _dropdown unit D.component dropdownInput handler
        where
        _dropdown = SProxy :: SProxy "dropdown"
        handler msg = Just $ injV _component $ HandleDropdown msg
        dropdownInput = { items: [ "Earth", "Mars" ], buttonLabel: "Human Planets" }

      renderContainer = whenElem (st.visibility == S.On) \_ ->
        HH.div
          (SS.setContainerProps
            [ classes_
                [ "Typeahead__container"
                , "Typeahead__container--hasItems" # guard hasItems
                ]
            ]
          )
          renderItems
        where
        hasItems = maybe false (not <<< null) (RD.toMaybe st.available)
        renderItems = do
          let renderMsg msg = [ HH.span_ [ HH.text msg ] ]
          case st.available of
            RD.NotAsked -> renderMsg "No search performed..."
            RD.Loading -> renderMsg "Loading..."
            RD.Failure e -> renderMsg e
            RD.Success available
              | hasItems -> renderItem `mapWithIndex` available
              | otherwise -> renderMsg "No results found"

        renderItem index { name, population } =
          HH.div
            (SS.setItemProps index [ classes_ [ base, highlight, "Location" ] ])
            [ HH.span
                [ class_ "Location__name" ]
                [ HH.text name ]
            , HH.span
                [ class_ "Location__population" ]
                [ HH.text population ]
            ]
          where
          base = "Typeahead__item"
          highlight = "Typeahead__item--highlighted"
                          # guard (st.highlightedIndex == Just index)

    handleHalogenSelectEvent :: S.Event -> H.HalogenM State Action ChildSlots Message Monad Unit
    handleHalogenSelectEvent = case _ of
      S.Selected ix -> do
        st <- H.get
        for_ st.available \arr ->
          for_ (arr !! ix) \item -> do
            let newSelections = item : st.selections
            H.modify_ _
              { selections = item : st.selections
              , available = RD.Success (filter (_ /= item) arr)
              , search = ""
              }
            H.raise $ SelectionsChanged newSelections
      S.Searched str -> do
        st <- H.get
        -- we'll use an external api to search locations
        H.modify_ _ { available = RD.Loading }
        items <- H.liftAff $ searchLocations str
        H.modify_ _ { available = items <#> \xs -> difference xs st.selections }
      _ -> pure unit

    handleMainAction :: Action_ -> H.HalogenM State Action ChildSlots Message Monad Unit
    handleMainAction = case _ of
      Initialize ->
        -- initialize 3rd-party renderless components (if needed)
        S.initializeHalogenSelect

        -- initialize main component

      Remove item -> do
        st <- H.get
        let newSelections = filter (_ /= item) st.selections
        H.modify_ _ { selections = newSelections }
        H.raise $ ItemRemoved item

      HandleDropdown msg -> case msg of
        D.SelectionChanged oldSelection newSelection -> do
          st <- H.get
          let
            mkLocation str = { name: "User Added: " <> str, population: "1" }
            newSelections = case oldSelection, newSelection of
              Nothing, Nothing ->
                Nothing
              Nothing, Just str ->
                Just (mkLocation str : st.selections)
              Just str, Nothing ->
                Just (filter (_ /= mkLocation str) st.selections)
              Just old, Just new ->
                Just (mkLocation new : (filter (_ /= mkLocation old) st.selections))
          for_ newSelections \selections ->
            H.modify_ _ { selections = selections }

    handleMainQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Message Monad (Maybe a)
    handleMainQuery = case _ of
      GetSelections reply -> do
         st <- H.get
         pure $ Just $ reply st.selections

-- Let's make this typeahead async.

type Location =
  { name :: String
  , population :: String
  }

searchLocations :: String -> Aff (RD.RemoteData String (Array Location))
searchLocations search = do
  res <- AX.get AR.json ("https://swapi.co/api/planets/?search=" <> search)
  let body = bimap AX.printError _.body res
  pure $ RD.fromEither $ traverse decodeJson =<< (_ .: "results") =<< decodeJson =<< body
