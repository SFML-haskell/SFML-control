module Control.Monad.SFML.Types
  ( SFML
  , runSFML ) where

import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad.SFML.Types.Internal


--------------------------------------------------------------------------------
-- | Run the SFML monad, calling all the destructors appropriately.
runSFML :: SFML a -> IO ()
runSFML (SFML m) = join . fmap sequence_ . flip execStateT [] $ m
