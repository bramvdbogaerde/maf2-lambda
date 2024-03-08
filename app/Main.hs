{-# LANGUAGE GADTs #-}
module Main (main) where

import Lib
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Text.Printf
import qualified Data.TypeLevel.HMap as HMap
import Domain.Lambda
import Data.Singletons.Sigma

printSto :: (Show adr) => Map adr V -> String
printSto m =
   intercalate "\n" (map (\(k,v) -> printf "%*s | %s" indent (show k) (showIt v)) adrs) ++ "\n----\n"
   where adrs   = Map.toList m
         indent = maximum (map (length . show . fst) adrs) + 5

showIt :: V -> String 
showIt (LamVal hm) = intercalate "," (map select (HMap.toList hm))
   where select :: HMap.BindingFrom (M Adr) -> String
         select (SIntKey :&: i) = "int ↦ " ++ (show i)
         select (SCloKey :&: s) = "clo ↦ " ++ "λ"

program = 
   App (Lam (Ident "x" 1) (Inc (Var (Ident "x" 2))))
       (Num 0)

main :: IO ()
main = putStrLn (printSto $ analyze program)
