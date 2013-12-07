module Main where

import Control.Monad.SFML
import SFML.Graphics
import SFML.Window

example :: SFML ()
example = undefined


main :: IO ()
main = runSFML $ do
    let ctxSettings = Just $ ContextSettings 24 8 0 1 2
    let videoMode = VideoMode 640 480 32
    wnd <- sfmlCreateRenderWindow videoMode "Hello" [SFDefaultStyle] ctxSettings
    spr <- sfmlCreateSprite
    loop wnd spr


loop :: RenderWindow -> Sprite -> SFML ()
loop wnd spr = do
    --sfmlDrawSprite wnd spr Nothing
    sfmlClearRenderWindow wnd blue
    sfmlDisplay wnd
    evt <- sfmlWaitEvent wnd
    case evt of
        Nothing -> return ()
        Just SFEvtClosed -> return ()
        _ -> loop wnd spr
