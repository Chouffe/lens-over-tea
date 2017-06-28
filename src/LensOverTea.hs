-- Needed to get constraints in type synonyms
-- Resource: https://artyom.me/lens-over-tea-1
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

module LensOverTea where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Functor.Identity
import qualified Data.Traversable as T

-- Lenses 101

-- A lens is a ds that packages a getter and a setter
-- data Lens s a = Lens
--   { getter :: s -> a
--   , setter :: a -> s -> s }

-- setIth :: Int -> a -> [a] -> [a]
-- setIth index new list
--   | index < 0 = error "setIth: negative index"
--   | null list = error "setIth: index too large"
--   | x:xs <- list = if index == 0
--                    then new : xs
--                    else x : setIth (index - 1) new xs

-- ix :: Int -> Lens [a] a
-- ix i = Lens { getter = (!! i)
--             , setter = setIth i }

-- Bad Approach: to increment the 1000th element of a list, we need to find it using getter then increment it, then seeter would have to find it again in order to set the new value

-- What about replacing setter :: a -> s -> s with modifier :: (a -> a) -> s -> s

-- type Lens s a = (a -> a) -> s -> (a, s)

-- ix :: Int -> Lens [a] a
-- ix index f list
--   | index < 0 = error "ix: negative index"
--   | null list = error "ix: index too large"
--   | old:rest <- list = if index == 0
--                        then (old, f old : rest)
--                        else second (old:) $ ix (index - 1) f rest


-- API

-- Getting a value using the id function
-- ghci> fst $ ix 3 id [7,4,1,8]

-- Setting a value using const
-- ghci> snd $ ix 3 (const 42) [7,4,1,8]

-- It works well but what if we want to change values multiple times??
-- type Lens s a = Monad m => (a -> m a) -> s -> (a, m s)

-- ix :: Int -> Lens [a] a
-- ix index f list
--   | index < 0 = error "ix: negative index"
--   | null list = error "ix: index too large"
--   | old:rest <- list = if index == 0
--                        then (old, liftM (:rest ) (f old))
--                        else second (liftM (old :)) $ ix (index - 1) f rest

-- Why Monad when a Functor could do it?

-- type Lens s a = forall f. Functor f => (a -> f a) -> s -> (a, f s)

-- ix :: Int -> Lens [a] a
-- ix index f list
--   | index < 0 = error "ix: negative index"
--   | null list = error "ix: index too large"
--   | old:rest <- list = if index == 0
--                        then (old, fmap (:rest ) (f old))
--                        else second (fmap (old :)) $ ix (index - 1) f rest

-- Is it really necessary to explicitely return the original value?
-- It is not! A functor can hide that in a context

-- Storey is the composition of (,) (a bifunctor) and a functor
-- type Storey x f = Compose ((,) x) f
data Storey x f a = Storey x (f a)
  deriving Show

instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)

-- Storey x f a ~ (x, f a)
--
-- ghci> (\x -> Storey x [1..x])

-- type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

ix :: Int -> Lens' [a] a
ix index f list
  | index < 0 = error "ix: index is negative"
  | null list = error "ix: index too large"
  | old:rest <- list = if index == 0
                       then (:rest) <$> (f old)
                       else (old:) <$> ix (index - 1) f rest

-- Why preserve the types?
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s

-- Set / Modify the value
-- ghci> runIdentity $ ix 2 (\x -> Identity 42) [0..4]
-- ghci> runIdentity $ ix 2 (\x -> Identity (x * 42)) [0..4]

-- over :: Lens s t a b -> (a -> b) -> s -> t
-- over l f = runIdentity . l (Identity . f)
-- ghci> over (ix 2) (const 88) [0..4]
-- ghci> over (ix 2) (* 2) [0..4]

-- Viewing the value
-- view lens s = x
--   where
--     Const x = lens (\x -> Const x) s

-- Exercises

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
-- _1 h (a, x) = (\b -> (b, x)) <$> (h a)
_1 h (a, x) = (,x) <$> (h a)

-- ghci> view _1 (1, 2)
-- ghci> over _1 (+41) (1, 2)


-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
-- _2 h (x, a) = (\b -> (x, b)) <$> (h a)
_2 h (x, a) = (x,) <$> (h a)

-- type Lens s t a b = (a -> f b) -> s -> f t
-- Make a lens out of a getter and setter - isomorphism
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = (\h s -> set s <$> h (get s))

choosing :: Lens s1 t1 a b ->
            Lens s2 t2 a b ->
            Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 h s = case s of
                       Left  s1 -> Left <$> l1 h s1
                       Right s2 -> Right <$> l2 h s2

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
-- (<%~) l f s = l (\a -> bimap f f (a, a)) s
(<%~) l f s = l (liftA2 (,) f f) s

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = l (\a -> (a, f a)) s

-- Lens' s () ~ (() -> f ()) -> s -> f s
united :: Lens' s ()
united h s = const s <$> h ()

-- Traversals 101

-- You can't execute an action several times using only the functor interface

launchMissiles :: IO ()
launchMissiles = putStrLn "Kaboom!"

-- Monad
-- ghci> launchMissiles >> launchMissiles

-- Applicative
-- ghci> launchMissiles *> launchMissiles

-- Functor: cant do it using functors... the only thing one can do is replace the returned ()...
-- ghci> ":(" <$ launchMissiles

_allM :: (Monad m, Eq a) => a -> (a -> m a) -> [a] -> m [a]
_allM ref f s = mapM update s
  where
    update old = if old == ref
                 then f old
                 else return old

-- Applicative version of _allM
_all' :: (Applicative f, Eq a) => a -> (a -> f a) -> [a] -> f [a]
_all' ref f s = traverse update s
  where
    update old = if old == ref
                 then f old
                 else pure old

-- updating view, over and set
-- The defined combinators wont work because an Applicative is stricter than a functor

-- view only uses `Const` functor
-- So its type would be
-- view :: ((a -> Const a a) -> s -> Const a s) -> s -> a

-- over only uses Identity functor, so its type would be:
-- over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t

-- Type synonyms
type Getting s a = (a -> Const a a) -> s -> Const a s
type Setting s t a b = (a -> Identity b) -> s -> Identity t

-- view :: Getting s a -> s -> a
-- over :: Setting s t a b -> (a -> b) -> s -> t
-- set :: Setting s t a b -> b -> s -> t

view :: Getting s a -> s -> a
view l = getConst . l Const

over :: Setting s t a b -> (a -> b) -> s -> t
over l f s = runIdentity (l (\a -> Identity (f a)) s)

set :: Setting s t a b -> b -> s -> t
set l x = over l (const x)

-- The view mistery
-- A monoid is needed to comine values

-- :t view (_all' 0) [0, 1, 2]
-- view (_all' 0) [0, 1, 2] :: (Data.Monoid.Monoid a, Num a, Eq a) => a

toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf l s = getConst (l (\a -> Const [a]) s)

-- ghci> toListOf (_all' 0) [0, 3, 1, 0]

-- preview: one can use an appropriate monoid to get the answer, First
-- preview :: ((a -> Const (First a) a) -> s -> Const (First a) s)
--         -> s
--         -> Maybe a

-- lets redefine Getting with the idea of a monoid

-- Getting2 r s a is a funcion which, given some way to get r from an a, will go over a in some s and return their combined rs.

type Getting2 r s a = (a -> Const r a) -> s -> Const r s
-- view     :: Getting2 a         s a -> s -> a
-- toListOf :: Getting2 [a]       s a -> s -> [a]
-- preview  :: Getting2 (First a) s a -> s -> Maybe a
-- has      :: Getting2 Any       s a -> s -> Bool
--

type Traversal s t a b = forall f. Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a


-- Each = every elements
--
-- ghci> set each 8 [1..3]
-- ghci> over each toUpper (Data.Text.pack "blah")
-- ghci> each (\x -> print x >> retur (-x)) (8, 19)

-- We need to define functional dependencies
-- Resource: https://wiki.haskell.org/Functional_dependencies
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b

instance T.Traversable t => Each (t a) (t b) a b where
  each = T.traverse

-- forall f. Applicative f => (a -> f a) -> s -> f s
_head :: Traversal' [a] a
_head h [] = pure []
_head h (x:xs) = liftA2 (:) (h x) (pure xs)

-- Recap:
-- A lens focuses on 1 value VS a traversal on many (potentially 0)
-- Traversals are Lenses with Applicative instead of Functor
-- In order to let view, over, set work with both lenses and traversals, their types are specialized to Getting and Setting
-- The Applicative instance for Const uses monoids to combine results, which means that by simply varying the monoid you can
--     - get all traversed values (Endo [], toListOf)
--     - get the first value (First: function preview)
--     - check for a value (Any, has)
--     - find the sum of traversed values (Sum)
--     - find the product of traversed values (Product)

-- Appendix: Operators
-- view: ^.
-- over: %~
-- set: .~
-- <%~  over + returns the new value in a tuple
-- <<%~ over + returns the old value
-- &: backward function application (flip ($))
--   ```
--   blah & somePart  %~ f
--        & otherPart %~ g
--        & ...
--   ```
