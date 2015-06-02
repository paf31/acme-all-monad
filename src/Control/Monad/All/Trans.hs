-- |
-- Module    :  Control.Monad.All.Trans
-- License   :  Public Domain
-- Stability :  stable
--
-- The @AllT@ monad monad transformer.

{-# LANGUAGE KindSignatures #-}

module Control.Monad.All.Trans (AllT(), anything) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Applicative

-- | The @AllT@ monad transformer.
-- 
-- @AllT@ is powerful enough to interpret any monadic action, which makes it very convenient for
-- defining domain specific languages.
--
-- Of course, any actions supported by the base monad are also supported by @AllT@.
data AllT (m :: * -> *) a = AllT

-- | Create any value in the @AllT@ monad.
anything :: AllT m a
anything = AllT

instance Functor (AllT m) where
  fmap f _ = AllT

instance Applicative (AllT m) where
  pure _ = AllT
  _ <*> _ = AllT

instance Monad (AllT m) where
  return _ = AllT
  _ >>= _ = AllT

instance Alternative (AllT m) where
  empty = AllT
  _ <|> _ = AllT

instance MonadPlus (AllT m) where
  mzero = AllT
  mplus _ _ = AllT

instance MonadTrans AllT where
  lift _ = AllT

instance MonadIO (AllT m) where
  liftIO _ = AllT