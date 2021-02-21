-- Semigrup (M, <>), este necesar ca <> sa fie asociativa
-- Monoid (M, <>, mempty): (M, <>) trebuie sa fie semigrup si, in plus, mempty trebuie sa fie element neutru la stanga si la dreapta
-- pentru orice x din M: m <> mempty = mempty <> m = m

import Test.QuickCheck
import Test.QuickCheck.Gen 
import Test.QuickCheck.Arbitrary
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool 
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidLeftIdentity a = (mempty <> a) == a 

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool 
monoidRightIdentity a = (a <> mempty) == a 

------------------------------
-- Exercitiul 1

data Trivial = Trivial 
    deriving (Eq, Show) 

instance Semigroup Trivial where 
    _ <> _ = Trivial 

instance Monoid Trivial where 
    mempty = Trivial

instance Arbitrary Trivial where 
    arbitrary = elements [Trivial]

type TrivialAssoc    = Trivial -> Trivial -> Trivial -> Bool 
type TrivialIdentity = Trivial -> Bool 

test_trivial_assoc = quickCheck (semigroupAssoc :: TrivialAssoc)
test_trivial_ml    = quickCheck (monoidLeftIdentity :: TrivialIdentity)
test_trivial_mr    = quickCheck (monoidRightIdentity :: TrivialIdentity)
--ex2
newtype BoolConj = BoolConj Bool
    deriving (Eq, Show)
instance Semigroup BoolConj where
    BoolConj a <> BoolConj b = BoolConj (a && b)
instance Monoid BoolConj where
    mempty = BoolConj True
instance Arbitrary BoolConj where
    arbitrary = MkGen ( \s i -> BoolConj ((unGen arbitrary) s i))
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjId = BoolConj -> Bool
a=quickCheck (semigroupAssoc :: BoolConjAssoc)
b=quickCheck (monoidLeftIdentity :: BoolConjId)
c=quickCheck (monoidRightIdentity :: BoolConjId)

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
    BoolDisj a <> BoolDisj b= BoolDisj (a||b)
instance Monoid BoolDisj where
    mempty=BoolDisj False
instance Arbitrary BoolDisj where
    arbitrary =  BoolDisj <$> arbitrary
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjId = BoolDisj -> Bool
d=quickCheck (semigroupAssoc :: BoolDisjAssoc)
e=quickCheck (monoidLeftIdentity :: BoolDisjId)
f=quickCheck (monoidRightIdentity :: BoolDisjId)
newtype Identity a = Identity a 
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where 
    (Identity x) <> (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a) where 
    mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary = Identity <$> arbitrary

type IdentityAssoc    a = Identity a -> Identity a -> Identity a -> Bool 
type IdentityIdentity a = Identity a -> Bool 

test_identity_assoc = quickCheck (semigroupAssoc      :: IdentityAssoc [Int])
test_identity_ml    = quickCheck (monoidLeftIdentity  :: IdentityIdentity String)
test_identity_mr    = quickCheck (monoidRightIdentity :: IdentityIdentity String)

-- Exercitiul 5 
-- Tipul Two 

data Two a b = Two a b 
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where 
    (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where 
    mempty = Two mempty mempty

-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
--     arbitrary = MkGen (\s i -> Two ((unGen arbitrary) s i) ((unGen arbitrary) s i))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where 
    arbitrary = Two <$> arbitrary <*> arbitrary

type TwoAssoc    a b = Two a b -> Two a b -> Two a b -> Bool 
type TwoIdentity a b = Two a b -> Bool 

test_two_assoc = quickCheck (semigroupAssoc      :: TwoAssoc [Int] String)
test_two_ml    = quickCheck (monoidLeftIdentity  :: TwoIdentity String [Int])
test_two_mr    = quickCheck (monoidRightIdentity :: TwoIdentity String String)

-- Exercitiul 6
-- Tipul Triple

data Triple a b c = Triple a b c
    deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Triple a b c) where
    (Triple x1 y1 z1) <> (Triple x2 y2 z2) = Triple (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Triple a b c) where
    mempty = Triple mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Triple a b c) where
    arbitrary = MkGen (\s i -> Triple ((unGen arbitrary) s i) ((unGen arbitrary) s i) ((unGen arbitrary) s i))
    -- arbitrary = Triple <$> arbitrary <*> arbitrary <*> arbitrary 

type TripleAssoc    a b c = Triple a b c -> Triple a b c -> Triple a b c -> Bool 
type TripleIdentity a b c = Triple a b c -> Bool 

test_triple_assoc = quickCheck (semigroupAssoc      :: TripleAssoc [Int] String String)
test_triple_ml    = quickCheck (monoidLeftIdentity  :: TripleIdentity String [Int] [Int])
test_triple_mr    = quickCheck (monoidRightIdentity :: TripleIdentity String String [Int])

-- Exercitiul 7
data Or a b = Fst a | Snd b 
    deriving (Eq, Show)

-- trebuie sa implementez operatia pentru Semigroup (<>) astfel incat
-- Fst 1 <> Snd 2 => Snd 2
-- Fst 1 <> Fst 2 => Fst 2 
-- Snd 1 <> Fst 2 => Snd 1
-- Snd 1 <> Snd 2 => Snd 1 

instance Semigroup (Or a b) where 
    Fst _ <> x = x 
    y     <> _ = y

-- nu exista niciun element neutru mempty astfel incat
-- oricum aleg x de tip Or a b, x <> mempty = mempty <> x = x 

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where 
    arbitrary = oneof [arbitraryFst, arbitrarySnd]
        where
            arbitraryFst = Fst <$> arbitrary -- MkGen (\s i -> ((unGen arbitrary) s i))
            arbitrarySnd = Snd <$> arbitrary

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool 

test_or_assoc = quickCheck (semigroupAssoc :: OrAssoc [Int] [Int])