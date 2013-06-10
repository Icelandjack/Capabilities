{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

-- | Internal plumbing for capabilities.
module Capabilities.Internals (
  
  Restr(..), Run(..), run, inj, (:<:), (:+:)(Inl, Inr), liftRaw,
  Free(..)
  ) where

import Control.Monad.Free (Free(..))
import Data.Comp.Algebra  (Alg, AlgM)
import Data.Comp.Derive   (derive, liftSum)
import Data.Comp.Ops      (inj, (:<:), (:+:)(Inl, Inr))

-- | Core datatype of the Capabilities package. Represents a
-- restricted IO action where @f@ indicates the restriction type
-- returning a value of type @a@.
newtype Restr f a = Restr (Free f a)
  deriving (Monad, Functor)

-- | Type class to define the semantics of a capability
class Functor f => Run f where
  runAlgebra :: Alg f (IO a)

-- | Lifts @Run@ so that it works with @(Run f, Run g) => Run (f :+: g)@.
$(derive [liftSum] [''Run])

foldRestr :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
foldRestr pure imp (Pure x) = pure x
foldRestr pure imp (Free t) = imp (fmap (foldRestr pure imp) t)

-- | Runs a restricted computation.
run :: Run f => Restr f a -> IO a
run (Restr restr) = foldRestr return runAlgebra restr

-- | Lift an action in a specific capability to Restr.
liftRaw :: (sub :<: f) => sub (Free f a) -> Restr f a
liftRaw = Restr . Free . inj
