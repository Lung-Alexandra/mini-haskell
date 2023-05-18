module Eval where

import Exp
import Data.List ( union, delete,nub )

vars :: Exp -> [IndexedVar]
vars (X num) =  [num]
vars (Lam num rest) = union [num] (vars rest)
vars (App rest1 rest) = union (vars rest1) (vars rest)

-- >>> vars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0},IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> vars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

freeVars :: Exp -> [IndexedVar]
freeVars (X num) = vars (X num)
freeVars (Lam num expr) = delete num (vars expr)
freeVars (App expr expr1) = union (freeVars expr) (freeVars expr1)

-- >>> freeVars (Lam (makeIndexedVar "x") (X (makeIndexedVar "y")))
-- [IndexedVar {ivName = "y", ivCount = 0}]

-- >>> freeVars  (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- [IndexedVar {ivName = "y", ivCount = 0},IndexedVar {ivName = "z", ivCount = 0}]

-- >>> freeVars (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- [IndexedVar {ivName = "x", ivCount = 0}]

-- >>> freeVars (Lam (IndexedVar {ivName = "x", ivCount = 0}) (App (X (IndexedVar {ivName = "x", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))))
-- []

occursFree :: IndexedVar -> Exp -> Bool
occursFree vari  expr = vari `elem` (freeVars expr)

-- >>> makeIndexedVar "x" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- False

-- >>> makeIndexedVar "y" `occursFree` Lam (makeIndexedVar "x") (X (makeIndexedVar "y"))
-- True


indiceNr num list = (maximum [ivCount x | x <- list , num == ivName x]) + 1

freshVar :: IndexedVar -> [IndexedVar] -> IndexedVar
freshVar num list
 | elem num list  = IndexedVar (ivName num) (indiceNr (ivName num) list)
 | otherwise = num

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x"]
-- IndexedVar {ivName = "x", ivCount = 1}

-- >>> freshVar (makeIndexedVar "x") [makeIndexedVar "x", IndexedVar {ivName = "x", ivCount = 1}, IndexedVar {ivName = "y", ivCount = 2}]
-- IndexedVar {ivName = "x", ivCount = 2}

renameVar :: IndexedVar -> IndexedVar -> Exp -> Exp
renameVar oldVar newVar (X var)
 | ivName var == ivName oldVar  = (X newVar)
 | otherwise = (X var)

renameVar oldVar newVar (Lam z e)
 | ivName z == ivName oldVar   = (Lam newVar (renameVar oldVar newVar e))
 | otherwise = (Lam z e)

renameVar oldVar newVar (App e e1)= App (renameVar oldVar newVar e) (renameVar oldVar newVar e1)


--- renameVar (IndexedVar {ivName = "x", ivCount = 0}) (IndexedVar {ivName = "z", ivCount = 0})  (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- >>> renameVar (IndexedVar {ivName = "x", ivCount = 0}) (IndexedVar {ivName = "z", ivCount = 0}) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "z", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

substitute :: IndexedVar -> Exp -> Exp -> Exp

substitute x y (X a)
    | a == x    =  y
    | otherwise = (X a)

-- Replace x with y in a lambda abstraction. We *do* need to
-- check to make sure that the lambda doesn't bind x. If the
-- lambda does bind x, then there's no possibility of x being
-- free in e, and we can leave the whole lambda as-is.
substitute x n (Lam y m)
    | y == x    = (Lam y m)
    | y `notElem` freeVars n = Lam y (substitute x n m)
    | otherwise = Lam yn (substitute x n m)  -- Recursively call subst here.
                    where yn =(freshVar y (freeVars n))
-- Replace x with y in an application. We just need to replace
-- x with y in z and in z', because an application doesn't
-- bind x (x stays free in z and z').
substitute x y (App z z1) = (App new_z new_z1)
    where new_z  = substitute x y z   -- Recursively call subst here.
          new_z1 = substitute x y z1  -- And recursively call subst here, too.


-- >>> substitute (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> substitute (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0})) (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 1}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- Call by value
-- - make sure operands are always evaluated before evaluating expression
-- - never reduce under a lambda-abstraction (it's already a value)
callByValueReduce :: Exp -> Exp
callByValueReduce (X x) = X x
callByValueReduce (App t1 t2)
  | Lam v t <- r1 = callByValueReduce (substitute v r2 t)
  | otherwise = App r1 r2
  where
    r1 = callByValueReduce t1
    r2 = callByValueReduce t2
callByValueReduce (Lam var t) = Lam var t

-- Call by name
-- - never evaluate right-hand-side of an application
-- - never reduce under a lambda-abstraction (it's already a value)
callByNameReduce :: Exp -> Exp
callByNameReduce (X x) = X x
callByNameReduce (App t1 t2)
  | Lam v t <- r1 = callByNameReduce (substitute v t2 t)
  | otherwise = App r1 t2
  where
    r1 = callByNameReduce t1
callByNameReduce (Lam var t) = Lam var t


normalize :: Exp -> Exp
normalize (X var)  = X var
normalize (Lam var body)  = Lam var (normalize body)
normalize (App left right) = case normalize left of
    Lam var body  -> normalize (substitute var (normalize right) body)
    otherwise -> App (normalize left) (normalize right)

-- >>> normalize (X (makeIndexedVar "x"))
-- X (IndexedVar {ivName = "x", ivCount = 0})

-- >>> normalize (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))) (App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0})))) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (App (X (IndexedVar {ivName = "y", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))))))
-- X (IndexedVar {ivName = "x", ivCount = 0})
