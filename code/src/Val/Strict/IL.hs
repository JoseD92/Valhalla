-------------------------------------------------------------------
-- |
-- Module      :  Val.Strict.IL
-- Copyright   :  Copyright (c) 2017, Jose Daniel Duran Toro
-- License     :  BSD3
--
-- Maintainer  :  Jose Daniel Duran Toro <jose_daniel_d@hotmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- A module that makes OpenGL easier in haskell.
--
-------------------------------------------------------------------

module Val.Strict.IL (
  ILKey,
  IL(..),
  emptyIL,
  lookupIL,
  insertIL,
  insertILWithKey,
  fromList,
  elemsIL,
  assocsIL,
  deleteIL,
  mapIL,
  mapILKeys,
  mapILWithKey,
  modifyIL,
  memberIL
) where

import Data.Map.Strict hiding (fromList)
import qualified Data.Map.Strict as Map
import Control.DeepSeq
import Control.Seq

-------------------------------------------------------------------
-- Container for objects.
-------------------------------------------------------------------

-- | Identity List id.
type ILKey = Integer

-- | Identity List, inspired by Yampa Arcade paper, contains objects that can be identifiable.
data IL a = IL {
    ilNext :: ILKey,
    ilAssocs :: Map ILKey a
  }

-- | Creates an empty IL.
emptyIL :: IL a
emptyIL = IL 0 Map.empty

-- | Looks up an element by key.
lookupIL :: ILKey -> IL a -> Maybe a
lookupIL key = Map.lookup key . ilAssocs

-- | Inserts an object into the IL.
insertIL :: a -> IL a -> IL a
insertIL a il = IL (ilNext il + 1) (Map.insert (ilNext il) a (ilAssocs il))

-- | Inserts an object into the IL with a specific key.
insertILWithKey :: a -> ILKey -> IL a -> IL a
insertILWithKey a k il@IL{ilAssocs=m} = il{ilAssocs=Map.insert k a m}

-- | Turns a list into an IL.
fromList :: [a] -> IL a
fromList l = IL (fromIntegral $ length l) (Map.fromList $ zip [0..] l)

-- | Returns elements in IL.
elemsIL :: IL a -> [a]
elemsIL = Map.elems . ilAssocs

-- | Returns all associations of keys and objects.
assocsIL :: IL a -> [(ILKey,a)]
assocsIL = Map.assocs . ilAssocs

-- | Deletes objects with given id.
deleteIL :: ILKey -> IL a -> IL a
deleteIL k il = il{ilAssocs=Map.delete k $ ilAssocs il}

-- | Maps a function to all elements in IL.
mapIL :: (a->b) -> IL a -> IL b
mapIL f il = il{ilAssocs=Map.map f $ ilAssocs il}

-- | Maps a function to all elements in IL.
mapILKeys :: (ILKey->b) -> IL a -> IL b
mapILKeys f il = il{ilAssocs=Map.mapWithKey (\k _ -> f k) $ ilAssocs il}

-- | Maps a function to all elements in IL.
mapILWithKey :: (ILKey -> a -> b) -> IL a -> IL b
mapILWithKey f il = il{ilAssocs=Map.mapWithKey f $ ilAssocs il}

-- | Modifies a single element with given function if it exist.
modifyIL :: ILKey -> (a -> a) -> IL a -> IL a
modifyIL key f il@IL{ilAssocs=m} = maybe il (\a -> il{ilAssocs=Map.insert key (f a) m} ) $ Map.lookup key m

-- | Returns whether an element is part of an IL.
memberIL :: ILKey -> IL a -> Bool
memberIL key IL{ilAssocs=m} = Map.member key m

instance Functor IL where
  fmap = mapIL

instance Foldable IL where
  foldMap f il = foldMap f (ilAssocs il)
  foldr f b il = Map.foldr f b (ilAssocs il)

instance Traversable IL where
  traverse f il = IL (ilNext il) <$> traverse f (ilAssocs il)

instance (NFData a) => NFData (IL a) where
  rnf (IL n assoc) = rnf n `seq` rnf assoc
