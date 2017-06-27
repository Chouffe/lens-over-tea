-- Needed to get constraints in type synonyms
-- Resource: https://artyom.me/lens-over-tea-1
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module LensOverTea where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Functor.Identity

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

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
-- ghci> over (ix 2) (const 88) [0..4]
-- ghci> over (ix 2) (* 2) [0..4]

-- Viewing the value
view lens s = x
  where
    Const x = lens (\x -> Const x) s

-- Exercises

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 h (a, x) = (\b -> (b, x)) <$> (h a)

-- ghci> view _1 (1, 2)
-- ghci> over _1 (+41) (1, 2)


-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 h (x, a) = (\b -> (x, b)) <$> (h a)

-- type Lens s t a b = (a -> f b) -> s -> f t
-- Make a lens out of a getter and setter - isomorphism
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = (\h s -> set s <$> h (get s))

choosing :: Lens s1 t1 a b ->
            Lens s2 t2 a b ->
            Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = (\h s -> case s of
                            Left  s1 -> Left <$> l1 h s1
                            Right s2 -> Right <$> l2 h s2)

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = l (\a -> bimap f f (a, a)) s

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = l (\a -> (a, f a)) s

-- Lens' s () ~ (() -> f ()) -> s -> f s
united :: Lens' s ()
united h s = (const s) <$> h ()
