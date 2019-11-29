module Components.Dropdown where

import Prelude

import Data.Array (index, length, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Internal.CSS (class_, classes_, whenElem)
import Record.Builder (Builder)
import Record.Builder as Builder
import Renderless.Halogen (caseV, injV, onV)
import Renderless.Halogen as RH
import Select (HS_ACTION, HS_INPUT, HS_STATE, _halogenSelect)
import Select as S
import Select.Setters as SS
import Type.Row (type (+))

type Input =
  { items :: Array String
  , buttonLabel :: String
  }

type MAIN_STATE_ROWS r =
  ( items :: Array String
  , selection :: Maybe String
  , buttonLabel :: String
  | r
  )
type InputRows =
  ( HS_INPUT
  + MAIN_STATE_ROWS
  + ()
  )

type StateRows =
  ( HS_STATE
  + MAIN_STATE_ROWS
  + ()
  )

_mainComponent :: SProxy "mainComponent"
_mainComponent = SProxy

data Action
  = Initialize

type ActionRows =
  ( mainComponent :: Action
  | HS_ACTION
  + ()
  )

type QueryRows = ()

data Message
  = SelectionChanged (Maybe String) (Maybe String)
type MsgRows =
  ( mainComponent :: Message
  -- |
  -- +
  )
type ChildSlots = ()
type Monad = Aff
type SelfSlot index = RH.SelfSlot QueryRows MsgRows index

component :: RH.Component HH.HTML QueryRows Input MsgRows Monad
component = RH.component (Builder.build pipeline <<< inputToPipeline) $ RH.defaultSpec
  { render = render
  , handleAction =
      caseV
        -- 3rd-party renderless components' actions
        # onV _halogenSelect (S.handleHalogenSelectAction handleHalogenSelectEvent)

        -- main component's actions
        # onV _mainComponent handleMainAction
  , initialize = Just $ injV _mainComponent Initialize
  }
  where
    inputToPipeline :: Input -> { | InputRows }
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
    pipeline :: Builder { | InputRows } { | StateRows }
    pipeline = S.mkHalogenSelectInput
      -- >>> OtherComponent.mkInput

    render :: { | StateRows } -> RH.ComponentHTML ActionRows ChildSlots Monad
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

    handleHalogenSelectEvent :: S.Event -> RH.HalogenM StateRows ActionRows ChildSlots MsgRows Monad Unit
    handleHalogenSelectEvent = case _ of
      S.Selected idx -> do
        st <- H.get
        let selection = index st.items idx
        H.modify_ _ { selection = selection, visibility = S.Off }
        H.raise $ injV _mainComponent $ SelectionChanged st.selection selection
      _ -> do
        pure unit

    handleMainAction :: Action -> RH.HalogenM StateRows ActionRows ChildSlots MsgRows Monad Unit
    handleMainAction = case _ of
      Initialize -> do
        -- initialize 3rd-party renderless components (if needed)
        S.initializeHalogenSelect
