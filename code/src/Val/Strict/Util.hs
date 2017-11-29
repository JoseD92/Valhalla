-------------------------------------------------------------------
-- |
-- Module      :  Val.Strict.Util
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Utilities.
--
-------------------------------------------------------------------

{-# LANGUAGE Arrows #-}

module Val.Strict.Util (
  makeSF,
  makeCamSF,
  deltaTime,
  uiState
)
where

import           FRP.Yampa
import           Val.Strict.Data
import           Val.Strict.UI
import EasyGL

-- | Returns time since last frame from ObjInput.
deltaTime :: ObjInput b c -> Time
deltaTime = timeGI . oiGameInput

-- | Returns screen state from ObjInput.
uiState :: ObjInput b c -> UIRead
uiState = uiGI . oiGameInput

-- | Creates a game object from an initial state and an update function.
makeSF :: a -> (a -> ObjInput b c -> (a,ObjOutput b c)) -> Object b c
makeSF a0 f = proc oi -> do
  rec
    --(a1,oo) <- iPre (f a0 emptyObjInput) <<< arr (uncurry f) -< (a1,oi)
    --(a1,oo) <- iPre (a0,undefined) <<< arr (uncurry f) -< (a1,oi)
    (a2,oo) <- arr (uncurry f) -< (a1,oi)
    a1 <- iPre a0 -< a2
  returnA -< oo

-- | Creates a camera from an initial state and an update function.
makeCamSF :: IsCamera c => a
  -> (a -> Time -> (GameInput,IL b) -> (a,c))
  -> SF (GameInput,IL b) c
makeCamSF a0 f = proc input -> do
  rec
    t2 <- iPre 0 -< t
    t <- time -< ()
  let delta = t - t2
  rec
    (a1,cam) <- iPre (f a0 0 (emptyGameInput,emptyIL)) <<< arr (\(state,time,oi) -> f state time oi) -< (a1,delta,input)
  returnA -< cam
