module Sugar where

import Exp

desugarVar :: Var -> IndexedVar
desugarVar (Var num) = IndexedVar{ivName= num, ivCount = 0}

-- >>> desugarVar (Var "x")
-- IndexedVar {ivName = "x", ivCount = 0}

sugarVar :: IndexedVar -> Var
sugarVar (IndexedVar num nr) = if nr == 0
                                then Var num
                                else Var (num++"_"++show nr)

-- >>> sugarVar (IndexedVar "x" 0)
-- Var {getVar = "x"}

-- >>> sugarVar (IndexedVar "x" 3)
-- Var {getVar = "x_3"}

consExp, nilExp, zeroExp, succExp, fixExp :: Exp
consExp = X (makeIndexedVar ":")  -- : :: a -> List a -> List a  list constructor
nilExp = X (makeIndexedVar "Nil") -- Nil :: List a               empty list
zeroExp = X (makeIndexedVar "Z")  -- Z :: Natural                zero
succExp = X (makeIndexedVar "S")  -- S :: Natural -> Natural     successor
fixExp = X (makeIndexedVar "fix") -- fix :: (a -> a) -> a        fixpoint fn.

desugarExp :: ComplexExp -> Exp
desugarExp (CX num) = X (desugarVar num)
desugarExp (CLam num compExp) = Lam (desugarVar num) (desugarExp compExp)
desugarExp (CApp compExp compExp1) = App (desugarExp compExp) (desugarExp compExp1)
desugarExp (Nat 0) = zeroExp
desugarExp (Nat x) = App succExp (desugarExp (Nat (x-1)))
desugarExp (List []) = nilExp
desugarExp (List (x:xs)) = App ( App consExp (desugarExp x)) (desugarExp (List xs))
desugarExp (Let num compExp1 compExp2) = App ( Lam (desugarVar num) (desugarExp compExp2)) (desugarExp compExp1)
desugarExp (LetRec num compExp1 compExp2) = App (desugarExp(CLam num compExp2)) (App fixExp (desugarExp(CLam num compExp1)))

-- >>> desugarExp (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0}))

-- >>> desugarExp (Nat 3)
-- App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (App (X (IndexedVar {ivName = "S", ivCount = 0})) (X (IndexedVar {ivName = "Z", ivCount = 0}))))

-- >>> desugarExp (List [CX (Var "y"), CX (Var "x")])
-- App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "y", ivCount = 0}))) (App (App (X (IndexedVar {ivName = ":", ivCount = 0})) (X (IndexedVar {ivName = "x", ivCount = 0}))) (X (IndexedVar {ivName = "Nil", ivCount = 0})))

-- >>> desugarExp (Let (Var "y") (CX (Var "x")) (CX (Var "z")))
-- App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (X (IndexedVar {ivName = "x", ivCount = 0}))

-- >>> desugarExp (LetRec (Var "y") (CX (Var "x")) (CX (Var "z")))
--App (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "z", ivCount = 0}))) (App (X (IndexedVar {ivName = "fix", ivCount = 0})) (Lam (IndexedVar {ivName = "y", ivCount = 0}) (X (IndexedVar {ivName = "x", ivCount = 0}))))

sugarExp :: Exp -> ComplexExp
sugarExp (X num) = CX (sugarVar num)
sugarExp (Lam num compExp) = CLam (sugarVar num) (sugarExp compExp)
sugarExp (App compExp compExp1) = CApp (sugarExp compExp) (sugarExp compExp1)


-- >>> sugarExp (App (X (IndexedVar "x" 0)) (X (IndexedVar "y" 1)))
-- CApp (CX (Var {getVar = "x"})) (CX (Var {getVar = "y_1"}))

-- >>> sugarExp (App (Lam (IndexedVar {ivName = "x", ivCount = 0}) (X (IndexedVar {ivName = "y", ivCount = 0}))) (X (IndexedVar {ivName = "z", ivCount = 0})))
-- (CApp (CLam (Var "x") (CX (Var "y"))) (CX (Var "z")))