module Template.Select.Uncommented where

import Prelude

import Data.Array (index, length, mapWithIndex)
import Data.Functor.Variant (FProxy)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Record.Builder (Builder)
import Record.Builder as Builder
import HalogenModular (caseV, caseVF, injV, onV, onVF)
import HalogenModular as HM
import Select (HS_ACTION, HS_INPUT, HS_STATE, _halogenSelect)
import Select as S
import Select.Setters as SS
import Type.Row (type (+))

type Input_ = Unit
type State_ = Unit

type StateRows_ r =
  ( buttonLabel :: String
  , selection :: Maybe String
  , items :: Array String
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

-- Uncomment one of these...
-- type State = State_
type State = { | StateRows }

_mainComponent :: SProxy "mainComponent"
_mainComponent = SProxy

data Action_
  = Initialize
  | Finalize
  | Receive Input

type ActionRows =
  ( mainComponent :: Action
  | HS_ACTION
  + ()
  )

-- Uncomment one of these...
-- type Action = Action_
type Action = Variant ActionRows

data Query_ a
  = Reply (Unit -> a)
  | Command a

type QueryRows =
  ( mainComponent :: FProxy Query
  -- |
  -- + ()
  )

-- Uncomment one of these...
type Query = Query_
-- type Query = VariantF QueryRows

type Msg_ = Int
type MsgRows =
  ( mainComponent :: Message
  -- |
  -- +
  )

-- Uncomment one of these...
type Message = Msg_
-- type Message = Variant MsgRows


type ChildSlots = ()
type Monad = Aff

type SelfSlot index = H.SelfSlot Query Message index

component :: H.Component HH.HTML Query Input Message Monad
component = HM.component (Builder.build pipeline <<< inputToPipeline) $ HM.defaultSpec
  { render = render
  , handleAction =
      caseV
        -- 3rd-party renderless components' actions
        # onV _halogenSelect (S.handleHalogenSelectAction handleHalogenSelectEvent)

        -- main component's actions
        # onV _mainComponent handleMainAction
  , handleQuery =
      caseVF
        -- 3rd-party renderless components' queries
        -- # onVF _libraryName (Library.handleLibraryQuery anyAdditionalArgs)

        -- main component's queries
        # onVF _mainComponent handleMainQuery
  , receive = Just <<< injV _mainComponent <<< Receive
  , initialize = Just $ injV _mainComponent Initialize
  , finalize = Just $ injV _mainComponent Finalize
  }
  where
    inputToPipeline :: Input -> { | InputRows }
    inputToPipeline _ =
      -- labels for main component's non-3rd-party-library state
      { buttonLabel: "-- Select --"
      , selection: Nothing
      , items: [ "1", "2", "3" ]

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

    render :: { | StateRows } -> H.ComponentHTML ActionRows ChildSlots Monad
    render state =
      HH.div
        [ HP.class_ $ ClassName "Dropdown" ]
        [ renderToggle, renderContainer ]
      where
      renderToggle =
        HH.button
          ( SS.setToggleProps [ HP.class_ $ ClassName "Dropdown__toggle" ] )
          [ HH.text (fromMaybe state.buttonLabel state.selection) ]

      renderContainer =
        if (state.visibility == S.Off)
          then
            HH.text ""
          else
            HH.div
              ( SS.setContainerProps [ HP.class_ $ ClassName "Dropdown__container" ] )
              ( mapWithIndex renderItem state.items )

      renderItem index item =
        HH.div
          ( SS.setItemProps index
              [ HP.classes $ ClassName <$>
                  [ "Dropdown__item"
                  , if (state.highlightedIndex /= Just index)
                      then ""
                      else "Dropdown__item--highlighted"
                  ]
              ]
          )
          [ HH.text item ]

    handleHalogenSelectEvent :: S.Event -> H.HalogenM StateRows ActionRows ChildSlots Message Monad Unit
    handleHalogenSelectEvent = case _ of
      S.Searched str -> do
        pure unit
      S.Selected idx -> do
        H.modify_ \s -> s { selection = index s.items idx }
      S.VisibilityChanged visibility -> do
        pure unit

    handleMainAction :: Action -> H.HalogenM StateRows ActionRows ChildSlots Message Monad Unit
    handleMainAction = case _ of
      Initialize ->
        -- initialize 3rd-party renderless components (if needed)
        S.initializeHalogenSelect

        -- initialize main component
      Finalize ->
        -- finalize main component

        -- finalized 3rd-party renderless components (if needed)
        pure unit
      Receive input ->
        pure unit

    handleMainQuery :: forall a. Query a -> H.HalogenM StateRows ActionRows ChildSlots Message Monad (Maybe a)
    handleMainQuery = case _ of
      Reply reply -> do
        pure $ Just $ reply unit
      Command next -> do
        pure $ Just next
