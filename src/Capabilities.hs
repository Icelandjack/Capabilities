module Capabilities (
  module Capabilities.IO,
  module Capabilities.Internals 
  )
  where

import Capabilities.IO
import Capabilities.Internals 
  -- Config(..), Run(..), Restr(..), liftRaw, Free(Pure),
  -- IsCapConfig(..), CapConfig(..),

  -- ask, asks, r,
  
  -- run, run', run'',
-- ) where


-- import Control.Monad.Reader  (ReaderT(..), MonadReader, runReaderT, ask, asks)
-- import Data.Comp.Algebra     (Alg, AlgM)
-- import Data.Comp.Ops         (inj, (:<:), (:+:)(Inl, Inr))
-- import qualified Data.Map as M
-- import Data.Typeable
-- import Control.Monad.Free    (Free(..))

-- -- | Data type for the configuration storage.
-- data Config = Config (M.Map TypeRep CapConfig)

-- -- | Operations required from configurations.
-- class Typeable a => IsCapConfig a where
--     def :: a

-- -- | An existential wrapper to allow storing different configuration types.
-- data CapConfig = forall a. IsCapConfig a => CapConfig a

-- -- | The main restricted monad type.
-- newtype Restr f a = Restr (Free f a)
--   deriving (Monad, Functor)

-- -- | Type class to define the semantics of a capability
-- class Functor f => Run f where
--   runAlgebra :: Alg f (ReaderT Config IO a)

-- instance (Run f, Run g) => Run (f :+: g) where
--   runAlgebra (Inl x) = runAlgebra x
--   runAlgebra (Inr y) = runAlgebra y

-- foldRestr :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
-- foldRestr pure imp (Pure x) = pure x
-- foldRestr pure imp (Free t) = imp (fmap (foldRestr pure imp) t)

-- runRawRestr :: Run f => Free f a -> ReaderT Config IO a
-- runRawRestr = foldRestr return runAlgebra

-- -- | Run an action in the Restr monad.
-- run :: Run f => Restr f a -> ReaderT Config IO a
-- run (Restr restr) = runRawRestr restr

-- -- | Run an action in the Restr monad using a given configuration.
-- run' :: Run f => Config -> Restr f a -> IO a
-- run' c action = (`runReaderT` c) $ run action

-- -- | Run an action with the default configuration.
-- run'' :: Run f => Restr f a -> IO a
-- run'' = run' (Config M.empty)

-- -- | Lift an action in a specific capability to Restr.
-- liftRaw :: (sub :<: f) => sub (Free f a) -> Restr f a
-- liftRaw = Restr . Free . inj

-- -- | Lift a monadic action to a Reader action.
-- r :: m a -> ReaderT r m a
-- r = ReaderT . const

