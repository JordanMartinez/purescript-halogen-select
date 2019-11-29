-- | This module exposes a component that can be used to build accessible selection
-- | user interfaces. You are responsible for providing all rendering, with the help
-- | of the `Select.Setters` module, but this component provides the relevant
-- | behaviors for dropdowns, autocompletes, typeaheads, keyboard-navigable calendars,
-- | and other selection UIs.
module Select where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds)
import Data.Traversable (for_, traverse, traverse_)
import Effect.Aff (Fiber, delay, error, forkAff, killFiber)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Prim.Row as Row
import Record.Builder as Builder
import Renderless.Halogen (HalogenM)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLElement as HTMLElement
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-----
-- HELPER TYPES

-- | The component slot type for easy use in a parent component
-- type Slot query slots msg = H.Slot (Query query slots) msg

-- | The component slot type when there is no extension
-- type Slot' = Slot (Const Void) () Void

-- | Represents a way to navigate on `Highlight` events: to the previous
-- | item, next item, or the item at a particular index.
data Target = Prev | Next | Index Int
derive instance eqTarget :: Eq Target

-- | Represents whether the component should display the item container. You
-- | should use this in your render function to control visibility:
-- |
-- | ```purescript
-- | render state = if state.visibility == On then renderAll else renderInputOnly
-- | ```
data Visibility = Off | On
derive instance eqVisibility :: Eq Visibility
derive instance ordVisibility :: Ord Visibility

_halogenSelect :: SProxy "halogenSelect"
_halogenSelect = SProxy

type HS_ACTION r = ( halogenSelect :: Action | r )

data Action
  = Search String
  | Highlight Target
  | Select Target (Maybe ME.MouseEvent)
  | ToggleClick ME.MouseEvent
  | Focus Boolean
  | Key KE.KeyboardEvent
  | PreventClick ME.MouseEvent
  | SetVisibility Visibility
  -- | Initialize (Maybe action) -- <-- that will be a problem...

-----
-- Event
data Event
  = Searched String
  | Selected Int
  | VisibilityChanged Visibility

-- | Text-driven inputs will operate like a normal search-driven selection component.
-- | Toggle-driven inputs will capture key streams and debounce in reverse (only notify
-- | about searches when time has expired).
data InputType = Text | Toggle

type HS_STATE r =
    ( inputType :: InputType
    , search :: String
    , debounceTime :: Milliseconds
    , debounceRef :: Maybe (Ref (Maybe Debouncer))
    , visibility :: Visibility
    , highlightedIndex :: Maybe Int
    , getItemCount :: {| r } -> Int
    | r
    )

type Debouncer =
  { var :: AVar Unit
  , fiber :: Fiber Unit
  }

type HS_INPUT r =
  ( inputType :: InputType
  , search :: Maybe String
  , debounceTime :: Maybe Milliseconds
  , getItemCount :: {| r } -> Int
  | r
  )

mkHalogenSelectInput
  :: forall inputRows
   . Row.Lacks "debounceRef" inputRows
  => Row.Lacks "visibility" inputRows
  => Row.Lacks "highlightedIndex" inputRows
  => Builder.Builder { | HS_INPUT + inputRows } { | HS_STATE + inputRows }
mkHalogenSelectInput =
    Builder.modify (SProxy :: _ "search") (fromMaybe "")
      >>> Builder.modify (SProxy :: _ "debounceTime") (fromMaybe mempty)
      >>> Builder.insert (SProxy :: _ "debounceRef") Nothing
      >>> Builder.insert (SProxy :: _ "visibility") Off
      >>> Builder.insert (SProxy :: _ "highlightedIndex") Nothing

handleHalogenSelectAction
  :: forall stateRows actionRows slots msgRows m
   . MonadAff m
  => Row.Lacks "debounceRef" stateRows
  => Row.Lacks "visibility" stateRows
  => Row.Lacks "highlightedIndex" stateRows
  => (Event -> HalogenM (HS_STATE + stateRows) (HS_ACTION + actionRows) slots msgRows m Unit)
  -> Action -> HalogenM (HS_STATE + stateRows) (HS_ACTION + actionRows) slots msgRows m Unit
handleHalogenSelectAction handleEvent = case _ of
  Search str -> do
    st <- H.get
    ref <- H.liftEffect $ map join $ traverse Ref.read st.debounceRef
    H.modify_ _ { search = str }
    void $ H.fork $ handle $ SetVisibility On

    case st.inputType, ref of
      Text, Nothing -> unit <$ do
        var   <- H.liftAff AVar.empty
        fiber <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put unit var

        -- This compututation will fork and run in the background. When the
        -- var is finally filled, the action will run
        void $ H.fork do
          void $ H.liftAff $ AVar.take var
          void $ H.liftEffect $ traverse_ (Ref.write Nothing) st.debounceRef
          H.modify_ _ { highlightedIndex = Just 0 }
          newState <- H.get
          handleEvent $ Searched newState.search

        void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

      Text, Just debouncer -> do
        let var = debouncer.var
        void $ H.liftAff $ killFiber (error "Time's up!") debouncer.fiber
        fiber <- H.liftAff $ forkAff do
          delay st.debounceTime
          AVar.put unit var
        void $ H.liftEffect $ traverse_ (Ref.write $ Just { var, fiber }) st.debounceRef

      -- Key stream is not yet implemented. However, this should capture user
      -- key events and expire their search after a set number of milliseconds.
      _, _ -> pure unit

  Highlight target -> do
    st <- H.get
    when (st.visibility == On) do
      H.modify_ _ { highlightedIndex = Just $ getTargetIndex st target }

  Select target mbEv -> do
    for_ mbEv (H.liftEffect <<< preventDefault <<< ME.toEvent)
    st <- H.get
    when (st.visibility == On) case target of
      Index ix -> handleEvent $ Selected ix
      Next -> handleEvent $ Selected $ getTargetIndex st target
      Prev -> handleEvent $ Selected $ getTargetIndex st target

  ToggleClick ev -> do
    H.liftEffect $ preventDefault $ ME.toEvent ev
    st <- H.get
    case st.visibility of
      On -> do
        handle $ Focus false
        handle $ SetVisibility Off
      Off -> do
        handle $ Focus true
        handle $ SetVisibility On

  Focus shouldFocus -> do
    inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
    for_ inputElement \el -> H.liftEffect case shouldFocus of
      true -> HTMLElement.focus el
      _ -> HTMLElement.blur el

  Key ev -> do
    void $ H.fork $ handle $ SetVisibility On
    let preventIt = H.liftEffect $ preventDefault $ KE.toEvent ev
    case KE.code ev of
      "ArrowUp" ->
        preventIt *> handle (Highlight Prev)
      "ArrowDown" ->
        preventIt *> handle (Highlight Next)
      "Escape" -> do
        inputElement <- H.getHTMLElementRef $ H.RefLabel "select-input"
        preventIt
        for_ inputElement (H.liftEffect <<< HTMLElement.blur)
      "Enter" -> do
        st <- H.get
        preventIt
        for_ st.highlightedIndex \ix ->
          handle $ Select (Index ix) Nothing
      otherKey -> pure unit

  PreventClick ev ->
    H.liftEffect $ preventDefault $ ME.toEvent ev

  SetVisibility v -> do
    st <- H.get
    when (st.visibility /= v) do
      H.modify_ _ { visibility = v, highlightedIndex = Just 0 }
      handleEvent $ VisibilityChanged v

  where
  -- eta-expansion is necessary to avoid infinite recursion
  handle act = handleHalogenSelectAction handleEvent act

  getTargetIndex st = case _ of
    Index i -> i
    Prev -> case st.highlightedIndex of
      Just i | i /= 0 -> i - 1
      _ -> lastIndex st
    Next -> case st.highlightedIndex of
      Just i | i /= lastIndex st -> i + 1
      _ -> 0
    where
    -- we know that the getItemCount function will only touch user fields,
    -- and that the state record contains *at least* the user fields, so
    -- this saves us from a set of unnecessary record deletions / modifications
    userState :: { | HS_STATE + stateRows } -> {| stateRows }
    userState = unsafeCoerce

    lastIndex :: { | HS_STATE + stateRows } -> Int
    lastIndex = (_ - 1) <<< st.getItemCount <<< userState
