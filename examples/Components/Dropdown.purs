module Components.Dropdown where

import Prelude

import Data.Array (index, length, mapWithIndex)
import Data.Const (Const)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import HalogenModular (caseV, injV, onV)
import HalogenModular as HM
import Internal.CSS (class_, classes_, whenElem)
import Record.Builder (Builder)
import Record.Builder as Builder
import Select (HS_ACTION, HS_INPUT, HS_STATE, _halogenSelect)
import Select as S
import Select.Setters as SS
import Type.Row (type (+))

type Input =
  { items :: Array String
  , buttonLabel :: String
  }

type State = { | StateRows }
type StateRows_ r =
  ( items :: Array String
  , selection :: Maybe String
  , buttonLabel :: String
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

_component :: SProxy "mainComponent"
_component = SProxy

type Action = Variant ActionRows
data Action_
  = Initialize

type ActionRows =
  ( mainComponent :: Action_
  | HS_ACTION
  + ()
  )

type Query = Const Void

data Message
  = SelectionChanged (Maybe String) (Maybe String)

type ChildSlots = ()
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
  , initialize = Just $ injV _component Initialize
  }
  where
    inputToPipeline :: Input -> { | PipelineRows }
    inputToPipeline { items, buttonLabel } =
      -- labels for main component's non-3rd-party-library state
      { items
      , buttonLabel
      , selection: Nothing

      -- == Labels for 3rd-party renderless components ==
      -- Halogen Select input
      , inputType: S.Toggle
      , search: Nothing
      , debounceTime: Nothing
      , getItemCount: \state -> length state.items

      -- Other library input
      }

    -- 3rd-party renderless components' `input -> state` functions
    pipeline :: Builder { | PipelineRows } { | StateRows }
    pipeline = S.mkHalogenSelectInput
      -- >>> OtherComponent.mkInput

    render :: State -> H.ComponentHTML Action ChildSlots Monad
    render state =
      HH.div
        [ class_ "Dropdown" ]
        [ renderToggle, renderContainer ]
      where
      renderToggle =
        HH.button
          ( SS.setToggleProps [ class_ "Dropdown__toggle" ] )
          [ HH.text (fromMaybe state.buttonLabel state.selection) ]

      renderContainer = whenElem (state.visibility == S.On) \_ ->
        HH.div
          ( SS.setContainerProps [ class_ "Dropdown__container" ] )
          ( renderItem `mapWithIndex` state.items )
        where
        renderItem index item =
          HH.div
            ( SS.setItemProps index
                [ classes_
                    [ "Dropdown__item"
                    , "Dropdown__item--highlighted"
                        # guard (state.highlightedIndex == Just index)
                    ]
                ]
            )
            [ HH.text item ]

    handleHalogenSelectEvent :: S.Event -> H.HalogenM State Action ChildSlots Message Monad Unit
    handleHalogenSelectEvent = case _ of
      S.Selected idx -> do
        st <- H.get
        let selection = index st.items idx
        H.modify_ _ { selection = selection, visibility = S.Off }
        H.raise $ SelectionChanged st.selection selection
      _ -> do
        pure unit

    handleMainAction :: Action_ -> H.HalogenM State Action ChildSlots Message Monad Unit
    handleMainAction = case _ of
      Initialize -> do
        -- initialize 3rd-party renderless components (if needed)
        S.initializeHalogenSelect
