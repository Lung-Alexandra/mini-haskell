import Prelude (undefined)

data False                                        -- empty type

data True = True                                  -- unit type

data And a b = And { proj1 :: a, proj2 :: b }     -- product

data Or a b                                       -- sum
  = Left a
  | Right b

type Not a = a -> False
type Iff a b = And (a -> b) (b -> a)

trueIntro :: True                                   -- true introduction
trueIntro = True

falseElim :: False -> b                             -- false elimination
falseElim var = case var of

implIntro :: (a -> b) -> (a -> b)                   -- implication introduction
implIntro = (\f -> f)

implElim :: (a -> b) -> a -> b                      -- implication elimination
implElim f a = f a

andIntro :: a -> b -> And a b                       -- and introduction
andIntro a b = And a b

andElimL :: And a b -> a                            -- and elimination 1
andElimL andt = proj1 andt

andElimR :: And a b -> b                            -- and elimination 2
andElimR andt = proj2 andt

orIntroL :: a -> Or a b                             -- or introduction 1
orIntroL a = Left a

orIntroR :: b -> Or a b                             -- or introduction 2
orIntroR b = Right b

orElim :: (a -> c) -> (b -> c) -> Or a b -> c       -- or elimination
orElim fa fb (Left a) = fa a
orElim fa fb (Right b) = fb b

notElim :: p -> Not p -> c                          -- not elimination 
notElim p nott = falseElim (nott p)

notIntro :: (forall p. a -> p) -> Not a             -- not introduction
notIntro f = f

iffIntro :: (a -> b) -> (b -> a) -> Iff a b         -- iff introduction
iffIntro fa fb = And fa fb

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
ax8 fa fb ort = orElim fa fb ort

ax9 :: (a -> b) -> (a -> Not b) -> Not a
ax9 fab fanb = (\a -> pNPFalse (fab a) (fanb a))

ax10 :: Not a -> a -> b
ax10 nota a = notElim a nota

modusPonens :: (a -> b) -> a -> b
modusPonens  f a = implElim (implIntro f) a

-- Several tautologies

pNPFalse :: p -> Not p -> False
pNPFalse x f = f x

deMorgan1 :: And (Not p) (Not q) -> Not (Or p q)
deMorgan1 andt (Left a) = proj1 andt a
deMorgan1 andt (Right b) = proj2 andt b

c1 :: Not (Or p q) -> Not p
c1 nott = (\p -> nott (orIntroL p))
c2 :: Not (Or p q) -> Not q
c2 nott = (\q -> nott (orIntroR q))

deMorgan2 :: Not (Or p q) -> And (Not p) (Not q)
deMorgan2 nott  = andIntro (c1 nott) (c2 nott)

deMorgan3 :: Or (Not p) (Not q) -> Not (And p q)
deMorgan3 (Left na) andt  = na (andElimL andt)
deMorgan3 (Right nb) andt  = nb (andElimR andt)

type DeMorgan4 = forall p q . Not (And p q) -> Or (Not p) (Not q)

-- Classical axioms

---- ((a -> False) -> False)
--excludedMiddleImplDoubleNeg :: Or a (Not a) -> (Not (Not a) -> a)
--excludedMiddleImplDoubleNeg (Left a) = (\_ -> a)
--excludedMiddleImplDoubleNeg (Right b) = (\f -> falseElim (f b))

type ExcludedMiddle = forall a. Or a (Not a)
type DoubleNegation = forall a. Not (Not a) -> a
type PeirceLaw = forall p q. ((p -> q) -> p) -> p
----                                             ((a -> False) -> False)
excludedMiddleImplDoubleNeg :: ExcludedMiddle -> DoubleNegation
excludedMiddleImplDoubleNeg (Left a) = (\_ -> a)
excludedMiddleImplDoubleNeg (Right b) = (\f -> falseElim (f b))
--
--lem :: Not(Not(Or a (Not a)))
--lem notor = notElim
--        (\a -> notor (orIntroL a))
--        (\na :: Not a -> notor (orIntroR na)) -- not a -> flase


lemma :: Not(Not(Or a (Not a)))
lemma def = pNPFalse na a
    where na = andElimL (deMorgan2 def)
          a = andElimR (deMorgan2 def)

doubleNegImplExcludedMiddle :: DoubleNegation -> ExcludedMiddle -- hard
doubleNegImplExcludedMiddle dne = dne lemma

lemma2 :: a -> (Not (And a b)) -> (Not b)
lemma2  a notand = (\b -> notand (andIntro a b))
--                  Or a (Not a)      Not (And p q) -> Or (Not p) (Not q)
classicDeMorgan4 :: ExcludedMiddle -> DeMorgan4
classicDeMorgan4 (Left a) =  (\notand -> orIntroR (lemma2 a notand))
classicDeMorgan4 (Right na) = (\notand -> orIntroL na)

