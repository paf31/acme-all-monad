-- |
-- Module    :  Control.Monad.All
-- License   :  Public Domain
-- Stability :  stable
--
-- The @All@ monad, which is powerful enough to interpret any monadic action.

module Control.Monad.All where

import Data.Functor.Identity
import Control.Monad.All.Trans

-- | The @All@ monad.
-- 
-- @All@ is powerful enough to interpret any monadic action, which makes it very convenient for
-- defining domain specific languages.
--
-- @All@ is defined as a type synonym for @'AllT' 'Identity'@.
type All = AllT Identity
