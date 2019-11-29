-- | This module exposes helper functions necessary for the library to attach behaviors
-- | to your render functions. These allow you to write a render function for your
-- | `Select` UI and then augment it at relevant points with the properties defined
-- | below.
module Select.Setters where

import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Halogen as H
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prelude (append, ($), (<<<))
import Renderless.Halogen (injAction)
import Select (Action(..), HS_ACTION, Target(..), _halogenSelect, Visibility(..))
import Type.Row (type (+))
import Web.Event.Event as E
import Web.UIEvent.FocusEvent as FE
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.MouseEvent as ME

-- | The properties that must be supported by the HTML element that serves
-- | as a menu toggle. This should be used with toggle-driven `Select` components.
type ToggleProps props =
  ( onFocus :: FE.FocusEvent
  , onKeyDown :: KE.KeyboardEvent
  , onMouseDown :: ME.MouseEvent
  , onClick :: ME.MouseEvent
  , onBlur :: FE.FocusEvent
  , tabIndex :: Int
  | props
  )

-- | A helper function that augments an array of `IProps` with `ToggleProps`. It
-- | allows the toggle element to register key events for navigation or highlighting,
-- | record open and close events based on focus and blur, and to be focused with
-- | the tab key.
-- |
-- | ```purescript
-- | renderToggle = div (setToggleProps [ class "btn-class" ]) [ ...html ]
-- | ```
setToggleProps
  :: forall props actionRows
   . Array (HP.IProp (ToggleProps props) (Variant (HS_ACTION + actionRows)))
  -> Array (HP.IProp (ToggleProps props) (Variant (HS_ACTION + actionRows)))
setToggleProps = append
  [ HE.onFocus \_ -> Just $ injAction _halogenSelect $ SetVisibility On
  , HE.onMouseDown $ Just <<< injAction _halogenSelect <<< ToggleClick
  , HE.onKeyDown $ Just <<< injAction _halogenSelect<<< Key
  , HE.onBlur \_ -> Just $ injAction _halogenSelect $ SetVisibility Off
  , HP.tabIndex 0
  , HP.ref (H.RefLabel "select-input")
  ]

-- | The properties that must be supported by the HTML element that serves
-- | as a text input. This should be used with input-driven `Select` components.
type InputProps props =
  ( onFocus :: FE.FocusEvent
  , onKeyDown :: KE.KeyboardEvent
  , onInput :: E.Event
  , value :: String
  , onMouseDown :: ME.MouseEvent
  , onBlur :: FE.FocusEvent
  , tabIndex :: Int
  | props
  )

-- | A helper function that augments an array of `IProps` with `InputProps`. It
-- | allows the input element to capture string values, register key events for
-- | navigation, record open and close events based on focus and blur, and to be
-- | focused with the tab key.
-- |
-- | ```purescript
-- | renderInput = input_ (setInputProps [ class "my-class" ])
-- | ```
setInputProps
  :: forall props actionRows
   . Array (HP.IProp (InputProps props) (Variant (HS_ACTION + actionRows)))
  -> Array (HP.IProp (InputProps props) (Variant (HS_ACTION + actionRows)))
setInputProps = append
  [ HE.onFocus \_ -> Just $ injAction _halogenSelect $ SetVisibility On
  , HE.onKeyDown $ Just <<< injAction _halogenSelect <<< Key
  , HE.onValueInput $ Just <<< injAction _halogenSelect <<< Search
  , HE.onMouseDown \_ -> Just $ injAction _halogenSelect $ SetVisibility On
  , HE.onBlur \_ -> Just $ injAction _halogenSelect $ SetVisibility Off
  , HP.tabIndex 0
  , HP.ref (H.RefLabel "select-input")
  ]

-- | The properties that must be supported by the HTML element that acts as a
-- | selectable "item" in your UI. This should be attached to every item that
-- | can be selected.
type ItemProps props =
  ( onMouseDown :: ME.MouseEvent
  , onMouseOver :: ME.MouseEvent
  | props
  )

-- | A helper function that augments an array of `IProps` with `ItemProps`. It
-- | allows items to be highlighted and selected.
-- |
-- | This expects an index for use in highlighting. It's useful in combination
-- | with `mapWithIndex`:
-- |
-- | ```purescript
-- | renderItem index itemHTML =
-- |   HH.li (setItemProps index [ props ]) [ itemHTML ]
-- |
-- | render = renderItem `mapWithIndex` itemsArray
-- | ```
setItemProps
  :: forall props actionRows
   . Int
  -> Array (HP.IProp (ItemProps props) (Variant (HS_ACTION + actionRows)))
  -> Array (HP.IProp (ItemProps props) (Variant (HS_ACTION + actionRows)))
setItemProps index = append
  [ HE.onMouseDown \ev -> Just $ injAction _halogenSelect (Select (Index index) (Just ev))
  , HE.onMouseOver \_ -> Just $ injAction _halogenSelect $ Highlight (Index index)
  ]

-- | A helper function that augments an array of `IProps` with a `MouseDown`
-- | handler. It prevents clicking on an item within an enclosing HTML element
-- | from bubbling up a blur event to the DOM. This should be used on the parent
-- | element that contains your items.
setContainerProps
  :: forall props actionRows
   . Array (HP.IProp (onMouseDown :: ME.MouseEvent | props) (Variant (HS_ACTION + actionRows)))
  -> Array (HP.IProp (onMouseDown :: ME.MouseEvent | props) (Variant (HS_ACTION + actionRows)))
setContainerProps = append
  [ HE.onMouseDown $ Just <<< injAction _halogenSelect <<< PreventClick ]
