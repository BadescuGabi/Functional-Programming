import Data.Char (isUpper)
import Data.Foldable (foldMap, foldr)
import Data.Monoid
import Data.Semigroup (Max (..), Min (..))
import Test.QuickCheck
import Test.QuickCheck.Gen 
import Test.QuickCheck.Arbitrary

data Arbore a=
    Nod (Arbore a) a (Arbore a)
    |Frunza a
    |Vid


{-data Arbore a = 
    Nod (Arbore a) a (Arbore a)
    | Frunza a 
    | Vid

arbore1 = Nod (Nod (Frunza 2) 4 Vid) 5 (Nod (Nod (Frunza 7) 8 Vid) 9 (Nod (Frunza 10) 11 (Frunza 12)))
arbore2 = Nod (Nod (Frunza 2) 4 Vid) 4 (Nod (Nod (Frunza 7) 8 Vid) 9 (Nod (Frunza 10) 11 (Frunza 12)))
arbore3 = Nod (Nod (Frunza 2) 4 Vid) 5 (Nod (Nod (Frunza 7) 8 Vid) 9 (Nod (Frunza 13) 11 (Frunza 12)))

-- a
fArbToList :: (Arbore a) -> [a]
fArbToList Vid = []
fArbToList (Frunza val) = [val]
fArbToList (Nod arb1 val arb2) = fArbToList arb1 ++ [val] ++ fArbToList arb2
fVerifArb :: Ord a => (Arbore a) -> Bool
fVerifArb arb = length (nub (fArbToList arb)) == length (fArbToList arb) && fVerif (fArbToList arb)
    where 
        fVerif :: Ord a => [a] -> Bool
        fVerif (x : []) = True
        fVerif (x : xx : []) = x < xx
        fVerif (x : xx : xxx) = x < xx && fVerif (xx : xxx) -}
--a
verifArb :: Ord a => Arbore a -> Bool
verifArb arb = isOrdered (inorder arb)
                where
                    isOrdered :: Ord a => [a] -> Bool
                    isOrdered ls 
                        | null ls = True
                        | length(ls) == 1 = True
                        | (head ls) < (head (tail ls)) = isOrdered (tail ls)
                        | otherwise = False

inorder Vid = []
inorder(Frunza n) = [n]
inorder(Nod t0 n t1) = inorder t0 ++ [n] ++ inorder t1


--b
arb1::Arbore Integer
arb1=  (Nod (Nod(Frunza 1) 3 (Frunza 10)) 5 (Nod (Frunza 2) 8 (Frunza 7)))
cinsert::(Ord a)=>a->Arbore a->Arbore a
cinsert k Vid  = Nod Vid k Vid
cinsert k (Frunza v)
    |k>v =Nod Vid k (Frunza v)
    |otherwise= Nod (Frunza v) k Vid
cinsert k (Nod l v r)
        | v==k = (Nod l k r)
        | v<k =( Nod  (cinsert k l) v r)
        |otherwise= (Nod l v (cinsert k r))



instance  Functor Arbore where
    fmap f = go
        where
            go Vid=Vid
            go (Frunza v)=Frunza (f v)
            go (Nod l v r)=Nod (go l) (f v) (go r)

instance Foldable Arbore where
    foldMap f = go
        where
        go Vid = mempty
        go (Frunza a) = f a
        go (Nod a1 a2 a3) = go a1 <> f a2 <> go a3

instance (Show a, Ord a) => Show (Arbore a) where
        show arb = if verifArb arb then show' arb 
                else "NU ESTE"
                where
                        show' Vid = ""
                        show' (Frunza x) = show x
                        show' (Nod Vid x Vid) = show x
                        show' (Nod Vid x arb2) = show x ++ ", " ++ show' arb2
                        show' (Nod arb1 x Vid) = show' arb1 ++ ", " ++ show x
                        show' (Nod arb1 x arb2) = show' arb1 ++ ", " ++ show x ++ ", " ++ show' arb2