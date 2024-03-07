module Syntax.Lambda(Ident(..), Exp(..)) where

data Ident = Ident String Int
           deriving (Eq, Ord, Show)
data Exp = Lam Ident Exp | Num Int | If Exp Exp Exp | Nonzero Exp | Inc Exp | App Exp Exp | Var Ident
         deriving (Eq, Ord, Show)
   
