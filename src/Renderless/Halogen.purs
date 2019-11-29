module Renderless.Halogen where

import Control.Applicative (pure)
import Data.Unit (Unit, unit)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Function ((<<<), const)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (traverse_)
import Data.Functor (($>), map)
import Data.Functor.Variant (VariantF)
import Data.Variant (Variant)
import Halogen (HalogenQ(..))
import Halogen as H
import Halogen.HTML as HH
import Record.Builder as Builder

type Component surface queryRows inputRows msgRows m =
  H.Component surface (VariantF queryRows) { | inputRows } (Variant msgRows) m

type ComponentHTML actionRows slots m =
  H.ComponentHTML (Variant actionRows) slots m

type SelfSlot queryRows msgRows index =
  H.Slot (VariantF queryRows) (Variant msgRows) index

type HalogenM stateRows actionRows slots msgRows m a =
  H.HalogenM { | stateRows } (Variant actionRows) slots (Variant msgRows) m a

type Spec stateRows actionRows queryRows slots inputRows msgRows m =
  { -- usual Halogen component spec
    render
      :: { | stateRows }
      -> ComponentHTML actionRows slots m

    -- handle additional actions provided to the component
  , handleAction
      :: Variant actionRows
      -> HalogenM stateRows actionRows slots msgRows m Unit

    -- handle additional queries provided to the component
  , handleQuery
      :: forall a
       . VariantF queryRows a
      -> HalogenM stateRows actionRows slots msgRows m (Maybe a)

    -- optionally handle input on parent re-renders
  , receive
      :: { | inputRows }
      -> Maybe (Variant actionRows)

    -- perform some action when the component initializes.
  , initialize
      :: Maybe (Variant actionRows)

    -- optionally perform some action on initialization. disabled by default.
  , finalize
      :: Maybe (Variant actionRows)
  }

defaultSpec
  :: forall stateRows actionRows queryRows slots inputRows msgRows m
   . Spec stateRows actionRows queryRows slots inputRows msgRows m
defaultSpec =
  { render: const (HH.text mempty)
  , handleAction: const (pure unit)
  , handleQuery: const (pure Nothing)
  , receive: const Nothing
  , initialize: Nothing
  , finalize: Nothing
  }

component
  :: forall stateRows actionRows queryRows slots inputRows msgRows m
   . Builder.Builder { | inputRows } { | stateRows }
  -> Spec stateRows actionRows queryRows slots inputRows msgRows m
  -> Component HH.HTML queryRows inputRows msgRows m
component inputPipeline spec = H.mkComponent
  { initialState: Builder.build inputPipeline
  , render: spec.render
  , eval: case _ of
    Initialize a ->
      traverse_ spec.handleAction spec.initialize $> a
    Finalize a ->
      traverse_ spec.handleAction spec.finalize $> a
    Receive i a ->
      traverse_ spec.handleAction (spec.receive i) $> a
    Action action a ->
      spec.handleAction action $> a
    Query req f ->
      unCoyoneda (\g → map (maybe (f unit) g) <<< spec.handleQuery) req
  }
