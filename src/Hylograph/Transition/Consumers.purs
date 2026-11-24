-- | Tick consumers for transitions: adapters that make a transition, or a
-- | group of them, something the `Coordinator` can tick.
-- |
-- | These need nothing beyond this package (the `Engine` and the
-- | `Coordinator`), which is why they live here. Until 0.1.1 they were in
-- | `hylograph-simulation`, so that a page with no simulation in it had to
-- | take the simulation package to animate anything. The consumer for a
-- | force simulation is `Hylograph.Simulation.Consumers` in that package.
module Hylograph.Transition.Consumers
  ( transitionConsumer
  , transitionGroupConsumer
  , module Engine
  , module Coordinator
  ) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Hylograph.Transition.Coordinator (TickResult(..), Milliseconds)
import Hylograph.Transition.Coordinator (TickResult(..), Milliseconds) as Coordinator
import Hylograph.Transition.Engine (TransitionState, TransitionGroup)
import Hylograph.Transition.Engine (start, tick, currentValue, isComplete, group, groupTick, groupValues, groupComplete) as Engine

-- | A tick function for one transition.
-- |
-- | Takes a `Ref` to the transition's state and an effect to run on each
-- | tick with the current value. `Completed` when the transition finishes.
-- |
-- | ```purescript
-- | stateRef <- Ref.new (Engine.start mySpec)
-- | let consumer = transitionConsumer stateRef \value -> setOpacity element value
-- | C.register coord { tick: consumer, onComplete: log "done" }
-- | ```
transitionConsumer
  :: forall a
   . Ref (TransitionState a)
  -> (a -> Effect Unit)
  -> Milliseconds
  -> Effect TickResult
transitionConsumer stateRef onValue deltaMs = do
  state <- Ref.read stateRef
  let newState = Engine.tick deltaMs state
  Ref.write newState stateRef
  onValue (Engine.currentValue newState)
  pure $ if Engine.isComplete newState then Completed else StillRunning

-- | A tick function for a group of transitions run together.
-- |
-- | Takes a `Ref` to the group and an effect to run on each tick with all
-- | the current values. `Completed` when every transition has finished.
-- |
-- | ```purescript
-- | groupRef <- Ref.new (Engine.group [spec1, spec2, spec3])
-- | let consumer = transitionGroupConsumer groupRef \values -> draw values
-- | C.register coord { tick: consumer, onComplete: log "all done" }
-- | ```
transitionGroupConsumer
  :: Ref TransitionGroup
  -> (Array Number -> Effect Unit)
  -> Milliseconds
  -> Effect TickResult
transitionGroupConsumer groupRef onValues deltaMs = do
  grp <- Ref.read groupRef
  let newGroup = Engine.groupTick deltaMs grp
  Ref.write newGroup groupRef
  onValues (Engine.groupValues newGroup)
  pure $ if Engine.groupComplete newGroup then Completed else StillRunning
