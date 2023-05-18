{-# LANGUAGE RankNTypes #-}
module ListClass where

import Prelude (Show(..), (<>), undefined)
import qualified Data.List as List

import MyPrelude
import BoolClass
import MaybeClass
import NatClass
import PairClass

-- | The class of list-like types (types having a notion of
-- 'nil', 'cons'tructor, and aggregation --- 'foldr').
-- Instances should satisfy the following:
--
-- [Fold nil] @'foldr' f i 'nil' = i@
-- [Fold cons]  @'foldr' f i ('cons' x l) = f x ('foldr' f i l)@
class ListClass l where
  nil :: l a
  cons :: a -> l a -> l a
  foldr :: (a -> b -> b) -> b -> l a -> b

instance ListClass [] where
  nil = []
  cons = (:)
  foldr = List.foldr

-- | Append two lists
(++) :: (ListClass l) => l a -> l a -> l a
(++) a b = foldr cons b a

-- >>> cons 2 (cons 3 nil) ++ cons 1 (cons 4 (cons 0 nil)) :: [CNat]
-- [C2,C3,C1,C4,C0]

-- | Returns the length of a list as a 'NatClass'
length :: ListClass l => l a -> CNat
length = foldr (flip  (const . (add 1))) 0

-- >>> length (cons 1 (cons 4 (cons 0 nil)) :: [CNat]) :: ListClass
-- C3

-- | Test whether the list is empty.
isNull :: ListClass l => l a -> CBool
isNull li = ite (isZero (length li)) true false

-- >>> isNull (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- CFalse

-- | 'map' @f@ @xs@ is the list obtained by applying @f@ to each element of @xs@
map :: ListClass l => (a -> b) -> l a -> l b
map f l = foldr (cons . f) nil l

-- >>> map (mul 2) (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- [C2,C8,C0]

-- | Applied to a predicate and a list, returns the list of those elements that satisfy the predicate
filter :: ListClass l => (a -> CBool) -> l a -> l a
filter f l = foldr (\x y -> (ite (f x) (cons x y)  y) ) nil l

-- >>> filter (not . isZero :: CNat -> CBool) (cons 1 (cons 4 (cons 0 nil))) :: [CNat]
-- [C1,C4]

-- | Left-associative fold of a list.
-- @foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn@
foldl :: (ListClass l) => (b -> a -> b) -> b -> l a -> b
foldl f a bs = foldr (\b g x -> g (f x b)) id bs a -- hard; use foldr to compute a function which we then apply on z

-- >>> foldl exp 2 (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- C1

-- | Decompose a list into its head and tail ('nothing' for the empty list).
uncons :: ListClass l => l a -> CMaybe (CPair a (l a))
uncons li = maybeFMap (\headval -> pair (headval) (fromMaybe nil (tail li))) (head li)

-- >>> uncons (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- CJust <(C1,[C4,C0])>

-- | Extract the first element of a list into a 'MaybeClass' ('nothing' for the empty list)
head :: ListClass l => l a -> CMaybe a
head li =  foldr (\x y -> just x) nothing li

-- >>> head (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- CJust C1

-- | Extract the elements after the head of a list into a 'MaybeClass' ('nothing' for the empty list)
tail :: ListClass l => l a -> CMaybe (l a)
tail li = maybeFMap reverse (foldl (\x y -> ite (isNothing x :: CBool) (just nil) (maybeFMap (\lista -> cons y lista) x)) nothing li)

-- >>> tail (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- CJust [C4,C0]

-- | returns the elements of a list in reverse order.
reverse :: ListClass l => l a -> l a
reverse = foldl (\x y -> (cons y) x) nil

-- >>> reverse (cons 1 (cons 4 (cons 0 nil))) :: [CNat]
-- [C0,C4,C1]

-- | The 'sum' function computes the sum of a list of numbers.
sum :: ListClass l => l CNat -> CNat
sum = foldr (add) 0

-- >>> sum (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- C5

-- | The 'product' function computes the product of a list of numbers.
product :: ListClass l => l CNat -> CNat
product = foldr (mul) 1

-- >>> product (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- C0

-- | 'maximum' returns the maximum value from a list of naturals ('zero' for the empty list),
maximum :: ListClass l => l CNat -> CNat
maximum = foldr max 0

-- >>> maximum (cons 1 (cons 4 (cons 0 nil)) :: [CNat])
-- C4

-- | @'natlist' n@ generates a list containing the numbers from @1@ to @n@, in reverse order.
natToList :: ListClass l => CNat -> l CNat
natToList nr = iter (\lista -> cons (add 1 (fromMaybe 0 (head lista))) lista) (cons 1 nil) (fromMaybe 0 (pred nr))

-- >>> natToList 10 :: [CNat]
-- [C10,C9,C8,C7,C6,C5,C4,C3,C2,C1]

newtype CList a = CList { getCList :: forall b . (a -> b -> b) -> b -> b}

instance ListClass CList where
  foldr f i l = getCList l f i
  nil = CList (\f i -> i)
  cons val lista = CList (\f i -> f val (foldr f i lista))

-- | converting between different instances of 'ListClass'
fromListClass :: (ListClass l1, ListClass l2) => l1 a -> l2 a
fromListClass = foldr cons nil

-- | 'Show' instance for 'CList' (via transformation into Haskell lists)
--instance Show a => Show (CList a) where
--  show cl = "{" <> show (fromListClass cl :: [a]) <> "}"

-- | computes the factorial of a number
factorial :: CNat -> CNat
factorial n =  product (natToList n :: [CNat])


-- >>> factorial 5
-- C120

-- >>> cons 2 (cons 3 nil) ++ cons 1 (cons 4 (cons 0 nil)) :: CList CNat
-- {[C2,C3,C1,C4,C0]}

-- >>> length (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C3

-- >>> isNull (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- CFalse

-- >>> map (mul 2) (cons 1 (cons 4 (cons 0 nil))) :: CList CNat
-- {[C2,C8,C0]}

-- >>> filter (not . isZero :: CNat -> CBool) (cons 1 (cons 4 (cons 0 nil))) :: CList CNat
-- {[C1,C4]}

-- >>> foldl exp 2 (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C1

-- >>> uncons (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- CJust <(C1,{[C4,C0]})>

-- >>> head (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- CJust C1

-- >>> tail (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- CJust {[C4,C0]}

-- >>> reverse (cons 1 (cons 4 (cons 0 nil))) :: CList CNat
-- {[C0,C4,C1]}

-- >>> sum (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C5

-- >>> product (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C0

-- >>> maximum (cons 1 (cons 4 (cons 0 nil)) :: CList CNat)
-- C4

-- >>> natToList 10 :: CList CNat
-- {[C10,C9,C8,C7,C6,C5,C4,C3,C2,C1]}