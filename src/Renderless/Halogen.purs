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

type Spec state action query slots input message m =
  { -- usual Halogen component spec
    render
      :: state
      -> H.ComponentHTML action slots m

    -- handle additional actions provided to the component
  , handleAction
      :: action
      -> H.HalogenM state action slots message m Unit

    -- handle additional queries provided to the component
  , handleQuery
      :: forall a
       . query a
      -> H.HalogenM state action slots message m (Maybe a)

    -- optionally handle input on parent re-renders
  , receive
      :: input
      -> Maybe action

    -- perform some action when the component initializes.
  , initialize
      :: Maybe action

    -- optionally perform some action on initialization. disabled by default.
  , finalize
      :: Maybe action
  }

defaultSpec
  :: forall state action query slots input message m
   . Spec state action query slots input message m
defaultSpec =
  { render: const (HH.text mempty)
  , handleAction: const (pure unit)
  , handleQuery: const (pure Nothing)
  , receive: const Nothing
  , initialize: Nothing
  , finalize: Nothing
  }

component
  :: forall state action query slots input message m
   . (input -> state)
  -> Spec state action query slots input message m
  -> H.Component HH.HTML query input message m
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

-- | Convenience function that reduces boilerplate.
-- | `raiseV symbol value` == `H.raise (injV symbol value)`
raiseV ::
  forall symbol value otherRows state action slots msgRows m
  . R.Cons symbol value otherRows msgRows
  ⇒ IsSymbol symbol
  ⇒ SProxy symbol
  → value
  → H.HalogenM state action slots (Variant msgRows) m Unit
raiseV symbol value = H.raise (injV symbol value)

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
