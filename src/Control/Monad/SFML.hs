{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Control.Monad.SFML
  ( module Control.Monad.SFML.Types
  , createRenderWindow
  , createSprite
  , createRectangleShape
  , drawRectangle
  , drawRectangleOfSize
  , clearRenderWindow
  , waitEvent
  , display
  , sfmlDrawRectangle
  , sfmlDisplay
  ) where

import SFML.System.Vector2
import SFML.Graphics.Color
import SFML.SFDisplayable (SFDisplayable)
import SFML.Window (SFEvent, SFWindow, VideoMode, WindowStyle, ContextSettings)
import SFML.Graphics (Sprite, RenderWindow)
import qualified SFML.Graphics as G
import qualified SFML.Graphics.RenderWindow as RW
import Control.Monad.State.Strict

import Control.Monad.SFML.Types.Internal
import Control.Monad.SFML.Types
import Control.Monad.SFML.Types.TH




--------------------------------------------------------------------------------
createRenderWindow :: VideoMode
                   -> String
                   -> [WindowStyle]
                   -> Maybe ContextSettings
                   -> SFML RenderWindow
createRenderWindow vm t stl cs = SFML $ do
    wnd <- liftIO $ G.createRenderWindow vm t stl cs
    modify $ \s -> G.destroy wnd : s
    return wnd


--------------------------------------------------------------------------------
clearRenderWindow :: RenderWindow -> Color -> SFML ()
clearRenderWindow wnd = SFML . io . G.clearRenderWindow wnd


--------------------------------------------------------------------------------
createRectangleShape :: SFML G.RectangleShape
createRectangleShape = SFML $ do
  shp <- io . G.err $ G.createRectangleShape
  modify $ \s -> G.destroy shp : s
  return shp


--------------------------------------------------------------------------------
drawRectangle :: RenderWindow
              -> G.RectangleShape
              -> Maybe G.RenderStates
              -> SFML ()
drawRectangle wnd shp states = SFML $ io $ G.drawRectangle wnd shp states


--------------------------------------------------------------------------------
drawRectangleOfSize :: Vec2f -> SFML G.RectangleShape
drawRectangleOfSize size = SFML $ do
  shp <- io . G.err $ G.createRectangleShape
  io $ G.setSize shp size
  modify $ \s -> G.destroy shp : s
  return shp

--------------------------------------------------------------------------------
display :: SFDisplayable a => a -> SFML ()
display = SFML . io . G.display


--------------------------------------------------------------------------------
waitEvent :: SFWindow a => a -> SFML (Maybe SFEvent)
waitEvent = SFML . io . G.waitEvent


--------------------------------------------------------------------------------
createSprite :: SFML Sprite
createSprite = SFML $ do
 spr <- io . G.err $ G.createSprite
 modify $ \s -> G.destroy spr : s
 return spr

-- Exported functions
$(mkSimple 'G.drawRectangle 3)
$(mkSimple 'G.display 1)
