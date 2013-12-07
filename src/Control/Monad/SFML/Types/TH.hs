{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Control.Monad.SFML.Types.TH
       ( mkSimple ) where

import Language.Haskell.TH
import Data.Char
import Control.Monad.State.Strict
import Control.Monad.SFML.Types.Internal


{-- This module offer abstractions to easily convert
    original low-level Haskell functions into the SFML monad
--}


mkSimple :: Name -> Int -> Q [Dec]
mkSimple adapteeName argsNum = do
  adapterName <- newName ("sfml" ++ (capitalize . nameBase $ adapteeName))
  adapteeFn <- varE adapteeName
  let args = mkArgs argsNum
  let wrapper = mkApply adapteeFn (map VarE args)
  fnBody <- [| SFML $ liftIO $ $(return wrapper) |]
  return [FunD adapterName [Clause (map VarP args) (NormalB fnBody) []]]



--------------------------------------------------------------------------------
mkArgs :: Int -> [Name]
mkArgs n = map (mkName . (:[])) . take n $ ['a' .. 'z']


-- Given f and its args (e.g. x y z) builds ((f x) y) z)
mkApply :: Exp -> [Exp] -> Exp
mkApply fn [] = fn
mkApply fn (x:xs) = foldr (\ e acc -> AppE acc e) (AppE fn x) xs



capitalize [] = []
capitalize (x:xs) = toUpper x : xs

mkWithDestroy = undefined
