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
lift :: Name -> Q [Dec]
lift adapteeName = do
  argsNum <- extractArgNum adapteeName
  let args = mkArgs argsNum
  adapteeFn <- varE adapteeName
  let wrapper = mkApply adapteeFn (map VarE args)
  fnBody <- [| SFML $ liftIO $ $(return wrapper) |]
  generateWrapper adapteeName args fnBody


--------------------------------------------------------------------------------
generateWrapper :: Name -> [Name] -> Exp -> Q [Dec]
generateWrapper adapteeName args fnBody = do
  adapterName <- newName $ nameBase adapteeName
  adapteeFn <- varE adapteeName
  let wrapper = mkApply adapteeFn (map VarE args)
  return [FunD adapterName [Clause (map VarP args) (NormalB fnBody) []]]


--------------------------------------------------------------------------------
liftWithDestroy :: Name -> Name -> Q [Dec]
liftWithDestroy modifier adapteeName = do
  argsNum <- extractArgNum adapteeName
  let args = mkArgs argsNum
  adapteeFn <- varE adapteeName
  let wrapper = mkApply adapteeFn (map VarE args)
  fnBody <- [| SFML $ do
    res <- liftIO $ $(varE modifier) $ $(return wrapper)
    modify $ \s -> G.destroy res : s
    return res |]
  generateWrapper adapteeName args fnBody


--------------------------------------------------------------------------------
mkArgs :: Int -> [Name]
mkArgs n = map (mkName . (:[])) . take n $ ['a' .. 'z']


--------------------------------------------------------------------------------
extractArgNum :: Name -> Q Int
extractArgNum fname = do
  info <- reify fname
  case info of
   (VarI _ (ForallT _ _ t) _ _) -> return . countArgs $ t
   (VarI _ t _ _) -> return . countArgs $ t
   (ClassOpI _ (ForallT _ _ t) _ _) -> return . countArgs $ t
   e -> error $ show e ++ " is not a function."

   where
     countArgs (AppT (AppT ArrowT _) ts) = 1 + countArgs ts
     countArgs _ = 0


--------------------------------------------------------------------------------
-- Given f and its args (e.g. x y z) builds ((f x) y) z)
mkApply :: Exp -> [Exp] -> Exp
mkApply fn [] = fn
mkApply fn (x:xs) = foldl AppE (AppE fn x) xs


--------------------------------------------------------------------------------
capitalize [] = []
capitalize (x:xs) = toUpper x : xs
