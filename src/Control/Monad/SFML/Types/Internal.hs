{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.SFML.Types.Internal
  ( SFML(..)
  , SFMLState) where

import Control.Monad.State.Strict
import Control.Applicative


--------------------------------------------------------------------------------
type DestroyAction = IO ()


--------------------------------------------------------------------------------
type SFMLState = [DestroyAction]


--------------------------------------------------------------------------------
newtype SFML a = SFML { unSFML :: StateT SFMLState IO a }
  deriving (Functor, Applicative, Monad, MonadIO)
