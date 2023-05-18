
module Printing (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar ex = getVar ex

showExp :: ComplexExp -> String
showExp (CX n) = showVar n
showExp (Nat n) = show (n)
showExp (CLam v ex) = "\\" ++ (showVar v) ++" -> "++ showExp(ex)
showExp (CApp ex1 ex2) = showExp(ex1) ++" "++ showExp (ex2)
showExp (Let v ex1 ex2) = "Let " ++ (showVar v) ++ " := " ++ showExp(ex1) ++ " in " ++ showExp (ex2)
showExp (LetRec v ex1 ex2) = "LetRec " ++ (showVar v) ++ " := " ++ showExp (ex1) ++ " in "  ++ showExp (ex2)
showExp (List l) = show l

instance Show ComplexExp where
 show = showExp
