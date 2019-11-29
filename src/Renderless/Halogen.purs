module Renderless.Halogen where

import Control.Applicative (pure)
import Data.Coyoneda (unCoyoneda)
import Data.Foldable (traverse_)
import Data.Function ((<<<), const)
import Data.Functor (($>), map, class Functor)
import Data.Functor.Variant (FProxy, VariantF)
import Data.Functor.Variant as VF
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Unit (Unit, unit)
import Data.Variant (Variant)
import Data.Variant as V
import Halogen (HalogenQ(..))
import Halogen as H
import Halogen.HTML as HH
import Type.Row as R

type Component surface queryRows input msgRows m =
  H.Component surface (VariantF queryRows) input (Variant msgRows) m

type ComponentHTML actionRows slots m =
  H.ComponentHTML (Variant actionRows) slots m

type SelfSlot queryRows msgRows index =
  H.Slot (VariantF queryRows) (Variant msgRows) index

type HalogenM stateRows actionRows slots msgRows m a =
  H.HalogenM { | stateRows } (Variant actionRows) slots (Variant msgRows) m a

type Spec stateRows actionRows queryRows slots input msgRows m =
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
      :: input
      -> Maybe (Variant actionRows)

    -- perform some action when the component initializes.
  , initialize
      :: Maybe (Variant actionRows)

    -- optionally perform some action on initialization. disabled by default.
  , finalize
      :: Maybe (Variant actionRows)
  }

defaultSpec
  :: forall stateRows actionRows queryRows slots input msgRows m
   . Spec stateRows actionRows queryRows slots input msgRows m
defaultSpec =
  { render: const (HH.text mempty)
  , handleAction: const (pure unit)
  , handleQuery: const (pure Nothing)
  , receive: const Nothing
  , initialize: Nothing
  , finalize: Nothing
  }

component
  :: forall stateRows actionRows queryRows slots input msgRows m
   . (input -> { | stateRows })
  -> Spec stateRows actionRows queryRows slots input msgRows m
  -> Component HH.HTML queryRows input msgRows m
component mkInput spec = H.mkComponent
  { initialState: mkInput
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

injV
  :: forall sym a r1 r2
  . R.Cons sym a r1 r2
  ⇒ IsSymbol sym
  ⇒ SProxy sym
  → a
  → Variant r2
injV = V.inj

caseV :: forall a. Variant () → a
caseV = V.case_

onV
  :: forall sym a b r1 r2
  . R.Cons sym a r1 r2
  ⇒ IsSymbol sym
  ⇒ SProxy sym
  → (a → b)
  → (Variant r1 → b)
  → Variant r2
  → b
onV = V.on

injVF
  :: forall sym f a r1 r2
  . R.Cons sym (FProxy f) r1 r2
  ⇒ IsSymbol sym
  ⇒ Functor f
  ⇒ SProxy sym
  → f a
  → VariantF r2 a
injVF = VF.inj

caseVF :: forall a b. VariantF () a → b
caseVF = VF.case_

onVF
  :: forall sym f a b r1 r2
   . R.Cons sym (FProxy f) r1 r2
  => IsSymbol sym
  => SProxy sym
  -> (f a -> b)
  -> (VariantF r1 a -> b)
  -> VariantF r2 a
  -> b
onVF = VF.on
