{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}


{-|

This package builds on top of @SFML@ and offers an higher level of
abstraction over the original library, mainly exposing the 'SFML' monad,
which is responsible for allocating and running the destructors for your
managed objects. This allow us to program at an higher level of abstraction:


> import Control.Monad.SFML
> import qualified SFML.Graphics as G
> import qualified SFML.Window as W
> import SFML.Graphics.Color
> 
> import Paths_SFMLExamples
> 
> main :: IO ()
> main = runSFML $ do
>     let ctxSettings = Just $ W.ContextSettings 24 8 0 1 2
>     wnd <- createRenderWindow (W.VideoMode 640 480 32)
>            "SFML-Control Demo" [W.SFDefaultStyle] ctxSettings
>     logoPath  <- liftIO $ getDataFileName "Haskell-Logo.png"
>     fontPath  <- liftIO $ getDataFileName "Vera.ttf"
>     musicPath <- liftIO $ getDataFileName "DST-BreakOut.ogg"
>     tex <- textureFromFile logoPath Nothing
>     spr <- createSprite
>     fnt <- fontFromFile fontPath
>     txt <- createText
>     setTextString txt "Haskell-Control\nhandles memory\nfor you"
>     setTextFont txt fnt
>     setTextCharacterSize txt 20
>     setTextColor txt blue
>     msc <- musicFromFile musicPath
>     play msc
>     setTexture spr tex True
>     loop wnd spr txt
> 
> 
> loop :: G.RenderWindow -> G.Sprite -> G.Text -> SFML ()
> loop wnd spr txt = do
>     drawSprite wnd spr Nothing
>     drawText   wnd txt $ Just (G.renderStates { G.transform = G.translation 460 40 })
>     display wnd
>     evt <- waitEvent wnd
>     case evt of
>         Nothing -> return ()
>         Just W.SFEvtClosed -> return ()
>         _ -> loop wnd spr txt

-}


module Control.Monad.SFML
  ( module Control.Monad.SFML.Types
  -- * Re-exports for your convenience
  , liftIO
  ) where


import SFML.System.Vector2
import qualified SFML.Graphics as G

import Control.Monad.State.Strict hiding (lift)
import Control.Monad.SFML.Types.Internal
import Control.Monad.SFML.Types
import Control.Monad.SFML.Conversions
