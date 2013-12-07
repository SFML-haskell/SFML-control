{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.SFML
  ( module Control.Monad.SFML.Types
  , createRenderWindow
  , createSprite
  , createRectangleShape
  , drawRectangleOfSize
  , clearRenderWindow
  , waitEvent
  , drawRectangle
  , display
  ) where


import SFML.System.Vector2
import SFML.Graphics.Color
import SFML.SFDisplayable (SFDisplayable)
import SFML.Window (SFEvent, SFWindow, VideoMode, WindowStyle, ContextSettings)
import SFML.Graphics (Sprite, RenderWindow)
import qualified SFML.Graphics as G
import qualified SFML.Graphics.RenderWindow as RW
import Control.Monad.State.Strict hiding (lift)

import Control.Monad.SFML.Types.Internal
import Control.Monad.SFML.Types
import Control.Monad.SFML.Types.TH hiding (f)
import qualified Control.Monad.SFML.Types.TH as TH


--------------------------------------------------------------------------------
drawRectangleOfSize :: Vec2f -> SFML G.RectangleShape
drawRectangleOfSize size = SFML $ do
  shp <- io . G.err $ G.createRectangleShape
  io $ G.setSize shp size
  modify $ \s -> G.destroy shp : s
  return shp


-- Exported functions
$(lift 'G.drawRectangle)
$(lift 'G.clearRenderWindow)
$(lift 'G.display)
$(lift 'G.waitEvent)
$(liftWithDestroy 'G.err 'G.createSprite)
$(liftWithDestroy 'G.err 'G.createRectangleShape)
$(liftWithDestroy 'id 'G.createRenderWindow)
