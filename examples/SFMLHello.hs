module Main where

import Control.Monad.SFML
import Control.Monad.SFML.Graphics
import qualified SFML.Graphics as G
import qualified SFML.Window as W


main :: IO ()
main = runSFML $ do
    let ctxSettings = Just $ W.ContextSettings 24 8 0 1 2
    let videoMode = W.VideoMode 640 480 32
    wnd <- createRenderWindow videoMode "Hello" [W.SFDefaultStyle] ctxSettings
    loop wnd


loop :: G.RenderWindow -> SFML ()
loop wnd = do
    clearRenderWindow wnd G.blue
    display wnd
    evt <- waitEvent wnd
    case evt of
        Nothing -> return ()
        Just W.SFEvtClosed -> return ()
        _ -> loop wnd
