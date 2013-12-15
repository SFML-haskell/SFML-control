{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.SFML.System where


import Control.Monad.SFML.Types.TH
import qualified SFML.System as S

-- * System / Clock.hs
$(liftWithDestroy 'id 'S.createClock)
$(lift 'S.getElapsedTime)
$(lift 'S.restartClock)

-- * System / Sleep.hsc
$(lift 'S.sfSleep)
