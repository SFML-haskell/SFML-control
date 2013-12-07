{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Control.Monad.SFML.Types.TH
       ( lift
       , liftWithDestroy ) where

import Language.Haskell.TH
import Data.Char
import Control.Monad.State.Strict hiding (lift)
import Control.Monad.SFML.Types.Internal
import qualified SFML.Graphics as G


{-- This module offer abstractions to easily convert
    original low-level Haskell functions into the SFML monad
--}


--------------------------------------------------------------------------------
-- | Generates a new function, lifted inside the SFML monad.
lift :: Name -> Int -> Q [Dec]
lift adapteeName argsNum = do
  let args = mkArgs argsNum
  adapteeFn <- varE adapteeName
  let wrapper = mkApply adapteeFn (map VarE args)
  fnBody <- [| SFML $ liftIO $ $(return wrapper) |]
  generateWrapper adapteeName argsNum fnBody


--------------------------------------------------------------------------------
generateWrapper :: Name -> Int -> Exp -> Q [Dec]
generateWrapper adapteeName argsNum fnBody = do
  adapterName <- newName ("sfml" ++ (capitalize . nameBase $ adapteeName))
  adapteeFn <- varE adapteeName
  let args = mkArgs argsNum
  let wrapper = mkApply adapteeFn (map VarE args)
  return [FunD adapterName [Clause (map VarP args) (NormalB fnBody) []]]



liftWithDestroy :: Name -> Name -> Int -> Q [Dec]
liftWithDestroy modifier adapteeName argsNum = do
  let args = mkArgs argsNum
  adapteeFn <- varE adapteeName
  let wrapper = mkApply adapteeFn (map VarE args)
  fnBody <- [| SFML $ do
    res <- liftIO $ $(varE modifier) $ $(return wrapper)
    modify $ \s -> G.destroy res : s
    return res |]
  generateWrapper adapteeName argsNum fnBody


--------------------------------------------------------------------------------
mkArgs :: Int -> [Name]
mkArgs n = map (mkName . (:[])) . take n $ ['a' .. 'z']



--------------------------------------------------------------------------------
-- Given f and its args (e.g. x y z) builds ((f x) y) z)
mkApply :: Exp -> [Exp] -> Exp
mkApply fn [] = fn
mkApply fn (x:xs) = foldl AppE (AppE fn x) xs


--------------------------------------------------------------------------------
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
