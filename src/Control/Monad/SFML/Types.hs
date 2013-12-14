module Control.Monad.SFML.Types
  ( SFML
  , runSFML ) where

import Control.Monad.State.Strict
import Control.Monad.SFML.Types.Internal


--------------------------------------------------------------------------------
-- | Run the SFML monad, calling all the destructors appropriately.
-- The library is designed to force you to call this function to get back into
-- the 'IO' monad, so that every object is appropriately destroyed.
runSFML :: SFML a -> IO ()
runSFML (SFML m) = join . fmap sequence_ . flip execStateT [] $ m
