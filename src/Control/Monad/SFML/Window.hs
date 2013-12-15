{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.SFML.Window where


import Control.Monad.SFML.Types.TH
import qualified SFML.Window as W


-- * Window / Context.hsc
$(liftWithDestroy 'id 'W.createContext)
$(lift 'W.setActiveContext)

-- * Window / Joystick.hs
$(lift 'W.isJoystickConnected)
$(lift 'W.getButtonCount)
$(lift 'W.hasAxis)
$(lift 'W.isJoystickButtonPressed)
$(lift 'W.getAxisPosition)
$(lift 'W.updateJoystick)

-- * Window / Keyboard.hs
$(lift 'W.isKeyPressed)

-- * Window / Mouse.hs
$(lift 'W.isMouseButtonPressed)

-- * Window / VideoMode.hs
$(lift 'W.getDesktopMode)
$(lift 'W.getFullscreenModes)
$(lift 'W.isValid)

-- * Window / Window.hsc
$(liftWithDestroy 'id 'W.createWindow)
$(liftWithDestroy 'id 'W.windowFromHandle)
