{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.SFML
  ( module Control.Monad.SFML.Types
  , module Control.Monad.SFML.Conversions
  , liftIO
  ) where


import SFML.System.Vector2
import qualified SFML.Graphics as G

import Control.Monad.State.Strict hiding (lift)


import Control.Monad.SFML.Types.Internal
import Control.Monad.SFML.Types
import Control.Monad.SFML.Conversions


--------------------------------------------------------------------------------
drawRectangleOfSize :: Vec2f -> SFML G.RectangleShape
drawRectangleOfSize size = SFML $ do
  shp <- liftIO . G.err $ G.createRectangleShape
  liftIO $ G.setSize shp size
  modify $ \s -> G.destroy shp : s
  return shp
