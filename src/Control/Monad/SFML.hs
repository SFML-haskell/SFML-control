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
import Control.Monad.SFML.Types.TH


--------------------------------------------------------------------------------
drawRectangleOfSize :: Vec2f -> SFML G.RectangleShape
drawRectangleOfSize size = SFML $ do
  shp <- io . G.err $ G.createRectangleShape
  io $ G.setSize shp size
  modify $ \s -> G.destroy shp : s
  return shp


-- Exported functions
$(lift 'G.drawRectangle 3)
$(lift 'G.clearRenderWindow 2)
$(lift 'G.display 1)
$(lift 'G.waitEvent 1)
$(liftWithDestroy 'G.err 'G.createSprite 0)
$(liftWithDestroy 'G.err 'G.createRectangleShape 0)
$(liftWithDestroy 'id 'G.createRenderWindow 4)
