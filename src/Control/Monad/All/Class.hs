-- |
-- Module    :  Control.Monad.All.Class
-- License   :  Public Domain
-- Stability :  stable
--
-- The @MonadAll@ type class, which represents monads which are powerful enough to interpret any monadic action.

module Control.Monad.All.Class where

import Data.Monoid

import Control.Monad.All.Trans

import Control.Monad.Trans.Class

import Control.Monad.Trans.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as SS

import Control.Monad.Trans.Writer.Lazy as WL
import Control.Monad.Trans.Writer.Strict as WS

import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS

-- | The @MonadAll@ class represents those monads which support every monadic action.
--  
-- Instances are provided for @AllT@ and the standard monad transformers, so that you can use
-- arbitrary actions in other monad transformer stacks.
class (Monad m) => MonadAll m where
  action :: a -> m b
  
-- | Perform an action, discarding the result.
action_ :: (MonadAll m) => a -> m ()
action_ a = do
  _ <- action a
  return ()

instance MonadAll (AllT m) where
  action _ = anything

instance (MonadAll m) => MonadAll (IdentityT m) where
  action = lift . action

instance (MonadAll m) => MonadAll (ExceptT e m) where
  action = lift . action

instance (MonadAll m) => MonadAll (ListT m) where
  action = lift . action

instance (MonadAll m) => MonadAll (MaybeT m) where
  action = lift . action

instance (MonadAll m) => MonadAll (ReaderT r m) where
  action = lift . action

instance (MonadAll m, Monoid w) => MonadAll (WL.WriterT w m) where
  action = lift . action

instance (MonadAll m, Monoid w) => MonadAll (WS.WriterT w m) where
  action = lift . action

instance (MonadAll m) => MonadAll (SL.StateT s m) where
  action = lift . action

instance (MonadAll m) => MonadAll (SS.StateT s m) where
  action = lift . action

instance (MonadAll m, Monoid w) => MonadAll (RWSL.RWST r w s m) where
  action = lift . action

instance (MonadAll m, Monoid w) => MonadAll (RWSS.RWST r w s m) where
  action = lift . action