{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Domain.Lambda(Env, Clo, LamVal, clo, num, nonzero, inc, clos) where

import Data.Singletons
import Domain (inject)
import Lattice (Joinable, JoinLattice, joins, bottom)
import Lattice.ConstantPropagationLattice
import Data.TypeLevel.HMap
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Syntax.Lambda

import Control.Monad.Join

type Env adr = Map Ident adr
type Clo adr = (Env adr, Exp)

data LamKey = IntKey | CloKey deriving (Eq, Ord)
$(genHKeys ''LamKey)

type M adr = '[ IntKey ::-> CP Int, CloKey ::-> Set (Clo adr) ]

newtype LamVal adr = 
   LamVal (HMap (M adr))
   deriving (Eq, Joinable, JoinLattice)


-- | Apply the given function on the closures
-- in the value
clos :: forall a adr m . JoinLattice a => MonadJoin m => (Clo adr -> m a) -> LamVal adr -> m a
clos f (LamVal v) = mjoins (mapList select v)
   where select :: forall k . Sing (k :: LamKey) -> Assoc k (M adr) -> m a
         select SCloKey v' = mjoins $ Prelude.map f (Set.toList v')
         select _ _ = mzero

-- | Inject a closure in the domain
clo :: Clo adr -> LamVal adr
clo = LamVal . singleton @CloKey . Set.singleton

-- | Inject a number in the domain
num :: Int -> LamVal adr
num = LamVal . singleton @IntKey . inject

-- | Checks if the given value is nonzero
nonzero :: forall adr . LamVal adr -> CP Bool
nonzero (LamVal v) = joins (mapList select v)
   where select :: forall k . Sing (k :: LamKey) -> Assoc k (M adr) -> CP Bool
         select SCloKey _ = bottom
         select SIntKey v' = fmap (==0) v'

inc :: forall adr . (Ord adr) => LamVal adr -> LamVal adr
inc (LamVal v) = joins (mapList select v)
   where select :: forall k . Sing (k :: LamKey) -> Assoc k (M adr) -> LamVal adr
         select SCloKey _ = bottom
         select SIntKey v' = LamVal $ singleton @IntKey (fmap (+1) v')
