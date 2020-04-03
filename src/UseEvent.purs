module Example.Hooks.UseEvent
  ( useEvent
  , UseEvent
  )
  where

import Prelude

import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import FRP.Event (Event, create)
import Halogen.Hooks (Hook, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Partial.Unsafe (unsafePartial)

newtype UseEvent a hooks =
  UseEvent (UseEffect (UseState (Maybe { event :: Event a, push :: a -> Effect Unit }) hooks))

derive instance newtypeUseEvent :: Newtype (UseEvent a hooks) _

useEvent
  :: forall slots output m a
   . MonadEffect m
  => Hook slots output m (UseEvent a) { event :: Event a, push :: a -> Effect Unit }
useEvent = Hooks.wrap Hooks.do
  state /\ tState <- Hooks.useState Nothing
  Hooks.useLifecycleEffect do
    event <- liftEffect create
    Hooks.put tState (Just event)
    pure Nothing

  let event = unsafePartial $ fromJust state

  Hooks.pure event
