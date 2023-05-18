import Prelude (undefined)
import Distribution.Compat.Lens (_1)

newtype False = False { getVoid :: forall t. t }
newtype True = True { getTrue :: forall t . t -> t }
newtype And a b = And { getAnd :: forall t. (a -> b -> t) -> t }
newtype Or a b = Or { getOr :: forall t . (a -> t) -> (b -> t) -> t}
type Not a = a -> False
type Iff a b = And (a -> b) (b -> a)

trueIntro :: True                                   -- true introduction
trueIntro = True (\y->y)

falseElim :: False -> b                             -- false elimination
falseElim fa = getVoid fa
--falseElim fa = getFalse fa

implIntro :: (a -> b) -> (a -> b)                   -- implication introduction
implIntro = (\f -> f)

implElim :: (a -> b) -> a -> b                      -- implication elimination
implElim f a = f a

andIntro :: a -> b -> And a b                       -- and introduction
andIntro a b = And (\f -> f a b)

andElimL :: And a b -> a                            -- and elimination 1
andElimL andt = getAnd andt (\a b -> a)

andElimR :: And a b -> b                            -- and elimination 2
andElimR andt = getAnd andt (\a b -> b)

orIntroL :: a -> Or a b                             -- or introduction 1
orIntroL a = Or (\fa fb -> fa a)

orIntroR :: b -> Or a b                             -- or introduction 2
orIntroR b =  Or (\fa fb -> fb b)

orElim :: (a -> c) -> (b -> c) -> Or a b -> c       -- or elimination
orElim fa fb ort = getOr ort fa fb

notElim :: p -> Not p -> c                          -- not elimination 
notElim p nott = falseElim (nott p)

notIntro :: (forall p. a -> p) -> Not a             -- not introduction
notIntro f = f

iffIntro :: (a -> b) -> (b -> a) -> Iff a b         -- iff introduction
iffIntro fa fb = andIntro fa fb

iffElimL :: a -> Iff a b -> b                       -- iff elimination 1
iffElimL a ifft = (andElimL ifft) a

iffElimR :: b -> Iff a b -> a                       -- iff elimination 1
iffElimR b ifft = (andElimR ifft) b

-- Hilbert-style axiomatization for intuitionistic propositional logic


ax1 :: a -> b -> a
ax1 a b = a

ax2 :: (a -> b) -> (a -> (b -> c)) -> a -> c
ax2 fa fab a = fab a (fa a)

ax3 :: a -> b -> And a b
ax3 a b  = andIntro a b

ax4 :: And a b -> a
ax4 andt = andElimL andt

ax5 :: And a b -> b
ax5 andt = andElimR andt

ax6 :: a -> Or a b
ax6 a = orIntroL a

ax7 :: b -> Or a b
ax7 b = orIntroR b

ax8 :: (a -> c) -> (b -> c) -> Or a b -> c
ax8  fa fb ort = orElim fa fb ort

ax9 :: (a -> b) -> (a -> Not b) -> Not a
ax9 fab fanb = (\a -> pNPFalse (fab a) (fanb a))

ax10 :: Not a -> a -> b
ax10 nota a = notElim a nota

modusPonens :: (a -> b) -> a -> b
modusPonens f a = implElim (implIntro f) a

-- Several tautologies


pNPFalse :: p -> Not p -> False
pNPFalse x f = f x


deMorgan1 :: And (Not p) (Not q) -> Not (Or p q)
deMorgan1 andt = (\ort ->  orElim (\p -> (andElimL andt) p) (\q -> (andElimR andt) q) ort)

c1 :: Not (Or p q) -> Not p
c1 nott = (\p -> nott (orIntroL p))
c2 :: Not (Or p q) -> Not q
c2 nott = (\q -> nott (orIntroR q))

deMorgan2 :: Not (Or p q) -> And (Not p) (Not q)
deMorgan2 nott  = andIntro (c1 nott) (c2 nott)

deMorgan3 :: Or (Not p) (Not q) -> Not (And p q)
deMorgan3 ort = (\andt ->  orElim (\p -> p (andElimL andt)) (\q -> q (andElimR andt)) ort)



-- ((a -> False) -> False)
excludedMiddleImplDoubleNeg :: Or a (Not a) -> (Not (Not a) -> a)
excludedMiddleImplDoubleNeg ort =  (orElim (\a f -> a ) (\na f -> falseElim (f na)) ort)


lemma :: Not(Not(Or a (Not a)))
lemma def = pNPFalse na a
    where na = andElimL (deMorgan2 def)
          a = andElimR (deMorgan2 def)

doubleNegImplExcludedMiddle ::  (forall a. Not (Not a) -> a) -> Or b (Not b)
doubleNegImplExcludedMiddle dne = dne lemma
