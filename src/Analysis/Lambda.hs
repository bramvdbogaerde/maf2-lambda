{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Analysis.Lambda where

import Control.Monad.Join
import Analysis.Monad hiding (eval)
import Syntax.Lambda
import Domain.Lambda
import Domain (isTrue)

import Control.Fixpoint.EffectDriven
import qualified Control.Monad.State.SVar as SVar

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Function ((&))
import Data.Functor.Identity
import Prelude hiding (iterate)
import Control.Monad


data Component = Component Exp (Env Adr)
               deriving (Eq, Ord, Show)

data Adr = Adr String 
         | RetAdr Component
         deriving (Eq, Ord, Show)

type LamM m = (Monad m,
               MonadJoin m,
               StoreM m () Adr V,
               EffectM m Component,
               EnvM m Adr (Env Adr))

type V = LamVal Adr

eval :: LamM m => Exp -> m V
eval lam@(Lam _ _) =
   clo . (,lam) <$> getEnv
eval (Num n)     = return (num n)
eval (Nonzero n) = do
   v <- eval n
   if isTrue (nonzero v)
   then return (num 1) else return (num 0)
eval (Inc e)     = 
   inc <$> eval e
eval (If e1 e2 e3) = 
   cond (nonzero <$> eval e1) (eval e2) (eval e3)
eval (App e1 e2) = do
   v1 <- eval e1
   v2 <- eval e2
   clos (`apply` v2) v1
eval (Var (Ident nam _)) = 
   lookupEnv nam >>= lookupAdr

apply :: LamM m => Clo Adr -> V -> m V
apply (env, Lam (Ident nam _) exp') v = 
   withEnv (const env) $
      withExtendedEnv [(nam, Adr nam)] $ do
         writeAdr (Adr nam) v
         env' <- getEnv
         spawn (Component exp' env')
         lookupAdr (RetAdr (Component exp' env'))
apply _ _ = error "not a valid closure"

analyze :: Exp -> Map Adr V
analyze prg = 
      let ((_, sto), vsto) = iterate intra
               & runEnv Map.empty 
               & runJoinT
               & runStoreT' 
               & (`runEffectT`initial)
               & runIdentity
      in SVar.unify sto vsto
   where initial = [Component prg Map.empty]
         intra cmp@(Component exp' env) = 
            void $ withEnv (const env) (eval exp') >>= (writeAdr (RetAdr cmp))
