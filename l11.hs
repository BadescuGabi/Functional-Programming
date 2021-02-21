{-# LANGUAGE FlexibleInstances #-}

import Data.Char (isUpper)
import Data.Foldable (foldMap, foldr)
import Data.Monoid
import Data.Semigroup (Max (..), Min (..))

-- Exercitii pentru Foldable

--ex1
elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- elem x = foldr (\a b -> b || a== x) False
elem x = getAny . foldMap (Any . (== x))

null :: (Foldable t) => t a -> Bool
-- null = foldr (\a b -> False) True
null = getAll . foldMap (All . const False)

length :: (Foldable t) => t a -> Int
-- length = foldr (\a b -> b + 1) 0
length = getSum . foldMap (Sum . const 1)

toList :: (Foldable t) => t a -> [a]
-- toList = foldr (\a b -> a : b) []
toList = foldMap (: [])

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

--ex2
newtype Constant a b = Constant b

instance Foldable (Constant a) where
  -- foldr f b (Constant x) = f x b
  foldMap f (Constant x) = f x

data Two a b = Two a b

instance Foldable (Two a) where
  -- foldr f b (Two _ y) = f y b
  foldMap f (Two _ y) = f y

data Three a b c = Three a b c

instance Foldable (Three a b) where
  -- foldr f b (Three _ _ z) = f z b
  foldMap f (Three _ _ z) = f z

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  -- foldr f b (Three' _ y z) = f z (f y b)
  foldMap f (Three' _ y z) = f z <> f y

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  -- foldr f b (Four' _ y z t) = f t $ f z $ f y b
  foldMap f (Four' _ y z t) = f t <> f z <> f y

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Foldable GoatLord where
  -- foldr f b = go
  --   where
  --     go NoGoat = b
  --     go (OneGoat a) = f a b
  --     go (MoreGoats a1 a2 a3) = foldr f (foldr f (foldr f b a1) a2) a3
  foldMap f = go
    where
      go NoGoat = mempty
      go (OneGoat a) = f a
      go (MoreGoats a1 a2 a3) = go a1 <> go a2 <> go a3

--ex3
filterF ::
  ( Applicative f,
    Foldable t,
    Monoid (f a)
  ) =>
  (a -> Bool) ->
  t a ->
  f a
filterF f = foldMap select
  where
    select a
      | f a = pure a
      | otherwise = mempty

unit_testFilterF1 = filterF Data.Char.isUpper "aNA aRe mEre" == "NARE"

unit_testFilterF2 = filterF Data.Char.isUpper "aNA aRe mEre" == First (Just 'N')

unit_testFilterF3 = filterF Data.Char.isUpper "aNA aRe mEre" == Min 'A'

unit_testFilterF4 = filterF Data.Char.isUpper "aNA aRe mEre" == Max 'R'

unit_testFilterF5 = filterF Data.Char.isUpper "aNA aRe mEre" == Last (Just 'E')

-- Exercitii pentru Functor
newtype Identity a = Identity a

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a1 a2) = Pair (f a1) (f a2)

-- data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

-- data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

-- data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b1 b2) = Three' a (f b1) (f b2)

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four'' a b = Four'' a a a b

instance Functor (Four'' a) where
  fmap f (Four'' a1 a2 a3 b) = Four'' a1 a2 a3 (f b)

-- newtype Constant a b = Constant b

instance Functor (Constant a) where
  fmap f (Constant b) = Constant (f b)

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

newtype K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- pentru Flip nu trebuie să faceți instanță

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip (K (f b))

newtype LiftItOut f a = LiftItOut (f a)

instance Functor fa => Functor (LiftItOut fa) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor fa, Functor ga) => Functor (Parappa fa ga) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor gb => Functor (IgnoreOne fa gb a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap f = go
    where
      go NoGoat = NoGoat
      go (OneGoat a) = OneGoat (f a)
      go (MoreGoats a1 a2 a3) = MoreGoats (go a1) (go a2) (go a3)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
  fmap f = go
    where
      go Halt = Halt
      go (Print s a) = Print s (f a)
      go (Read sa) = Read (f . sa)
