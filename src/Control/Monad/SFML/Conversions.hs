{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Monad.SFML.Conversions (mb, G.err) where


import Control.Monad.SFML.Types.TH
import qualified SFML.Graphics as G
import qualified SFML.System as S
import qualified SFML.Window as W
import Data.Maybe

-- | Run the given IO action and throw an error if it fails.
-- This function is the dual of SFML's 'err'. The idea here
-- is that we accept a certain level of pragmatism and assume
-- the underlying C library is unlikely to fail for out-of-memory
-- errors or data corruption. 'SFML' follows a more disciplined
-- approach.
mb :: IO (Maybe a) -> IO a
mb = (maybe (error "Nothing found.") return =<<)
