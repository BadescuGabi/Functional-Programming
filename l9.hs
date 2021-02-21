import Data.Char
import Data.List
import Data.Maybe
import Prelude hiding (lookup)
import qualified Data.List as List

data Expr = Const Int -- integer constant
    | Expr :+: Expr -- addition
    | Expr :*: Expr -- multiplication
    deriving Eq
data Operation = Add | Mult deriving (Eq, Show)
data Tree = Lf Int -- leaf
    | Node Operation Tree Tree -- branch
    deriving (Eq, Show)
--11
instance Show Expr where
    show (Const p)= show p   
    show (p :+: q)= "(" ++ show p ++ " + " ++ show q ++ ")"
    show (p :*: q)= "(" ++ show p ++ " * " ++ show q ++ ")"

evalExp :: Expr -> Int
evalExp (Const p)=p
evalExp (p:+:q)=evalExp p + evalExp q
evalExp (p:*:q)=evalExp p * evalExp q
exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

--13
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add treeLeft treeRight)  = evalArb treeLeft + evalArb treeRight 
evalArb (Node Mult treeLeft treeRight) = evalArb treeLeft * evalArb treeRight 
--14
expToArb :: Expr -> Tree 
expToArb (Const x)=(Lf x)
expToArb (x :+: y)=Node Add  (expToArb x) (expToArb y)
expToArb (x :*: y)=Node Mult  (expToArb x) (expToArb y)
--15
class MySmallCheck a where
    smallValues :: [a]
    smallCheck :: ( a -> Bool ) -> Bool
    smallCheck prop = and [ prop x | x <- smallValues ]
instance MySmallCheck Expr where 
    smallValues = [exp1, exp2, exp3, exp4]

-- sa se verifice ca evaluarea unei expresii este egala cu evaluarea arborelui asociat expresiei
-- sa se verifice, ulterior, cu smallCheck
checkExpr :: Expr -> Bool 
type Key = Int          -- Key este acum un sinonim in program pentru Int
type Value = String 
checkExpr exp = evalExp exp == evalArb (expToArb exp)
class Collection c where 
    cempty :: c 
    csingleton :: Key -> Value -> c 
    cinsert :: Key -> Value -> c -> c 
    cdelete :: Key -> c -> c 
    clookup :: Key -> c -> Maybe Value 
    cToList :: c -> [(Key, Value)]

    ckeys :: c -> [Key]
    ckeys c = [fst p | p <- cToList c]
    cvalues :: c -> [Value]
    cvalues c = [snd p | p <- cToList c]
    cFromList :: [(Key, Value)] -> c 
    cFromList [] = cempty 
    cFromList ((k, v):kvs) = cinsert k v (cFromList kvs)
newtype PairList = PairList { getPairList :: [(Key, Value)] }

-- data PairList = PairList [(Key, Value)]
-- getPairList :: [(Key, Value)]
-- getPairList (PairList xs) = xs
instance Show PairList where 
    show (PairList pairList) = "PairList " ++ show pairList 

instance Collection PairList where 
    cempty = PairList []
    csingleton k v = PairList [(k, v)]
    cinsert k v (PairList list) = if elem k (map fst list) then PairList list else PairList ((k, v) : list)
    cdelete k (PairList list) = PairList [(k1, v1) | (k1, v1) <- list, k1 /= k]
    clookup k (PairList list) = lookup k list
    cToList = getPairList

--3


data SearchTree 
    = Empty 
    | Node1 SearchTree Key (Maybe Value) SearchTree 
    deriving Show 

-- in stanga sunt elementele cu cheie mai mica
-- in dreapta sunt elementele cu cheie mai mare 
-- sa se instantieze clasa Collection pentru tipul de date SearchTree 

instance Collection SearchTree where 
    cempty = Empty 
    csingleton k v = Node1 Empty k (Just v) Empty 
    clookup k Empty = Nothing 
    clookup k (Node1 treeLeft k1 v1 treeRight)
        | k == k1 = v1 
        | k < k1 = clookup k treeLeft 
        | otherwise = clookup k treeRight 
    cinsert k v Empty = csingleton k v
    cinsert k v (Node1 lefttree key value righttree)
        | k < key = Node1 (cinsert k v lefttree) key value righttree
        | k > key = Node1 lefttree key value (cinsert k v righttree)
        | otherwise = Node1 lefttree key value righttree

{-
instance Collection SearchTree where
    cempty = Empty
    csingleton k v  = Node Empty k (Just v) Empty 
    cinsert k v Empty = csingleton k v
    cinsert k v (Node arb1 key val arb2)
        | k > key   = Node arb1 key val (cinsert k v arb2)
        | k < key   = Node (cinsert k v arb1) key val arb2
        | otherwise = Node arb1 key val arb2
    cdelete k Empty = Empty
    cdelete k (Node arb1 key val arb2)
        | k > key   = Node arb1 key val (cdelete k arb2)
        | k < key   = Node (cdelete k arb1) key val arb2
        | otherwise = Node arb1 key Nothing arb2
    clookup k (Node arb1 key val arb2)
        | k > key   = clookup k arb2
        | k < key   = clookup k arb1
        | otherwise = val
    ctoList Empty = []
    ctoList (Node arb1 key Nothing arb2) = ctoList arb1 ++ ctoList arb2
    ctoList (Node arb1 key (Just value) arb2) = ctoList arb1 ++ [(key, value)] ++ ctoList arb2
    ckeys arb = [key | (key, value) <- ctoList arb]
    cvalues arb = [value | (key, value) <- ctoList arb]
    cfromList [] = Empty
    cfromList list = cinsert key value Empty
        where (key, value) = head list-}
-- Functorii 

data Arbore a
    = Frunza a
    | Nod a (Arbore a) (Arbore a) 
    deriving (Eq, Show)

-- vreau sa fac o instanta a clasei Functor pentru tipul de date Arbore 
-- primind o functie (a -> b), un Arbore a => Arbore b 

instance Functor Arbore where 
    fmap f (Frunza x) = Frunza (f x)
    fmap f (Nod x left right) = Nod (f x) (fmap f left) (fmap f right)

-- pot utiliza <$> 

arbore = Nod 2 (Nod 3 (Frunza 2) (Frunza 3)) (Frunza 5)

-- instance Functor Maybe where 
--     fmap f Nothing = Nothing 
--     fmap f (Just x) = Just (f x)