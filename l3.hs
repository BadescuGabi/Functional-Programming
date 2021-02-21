import Data.Char
import Data.List

{-
[x^2 | x <- [1 .. 10], x `rem` 3 == 2]
[(x, y) | x <- [1 .. 5], y <- [x .. (x+2)]]
[(x, y) | x <- [1 .. 3], let k = x^2, y <- [1 .. k]]
[x | x <- "Facultatea de Matematica si Informatica", elem x ['A' .. 'Z']]
[[x .. y] | x <- [1 .. 5], y <- [1 .. 5], x < y ]
-}

factori::Int->[Int]
factori l=[x|x<-[2..l],l`rem`x==0]

prim::Int->Bool
prim n= if length(factori n)==1 then True else False

numerePrime::Int->[Int]
numerePrime l=[x|x<-[2..l],prim x]

myzip3::[Int]->[Int]->[Int]->[(Int,Int,Int)]
myzip3 [] _ _=[]
myzip3 _ [] _=[]
myzip3 _ _ []=[]
myzip3 (x:xs) (x1:xs1) (x2:xs2)=(x,x1,x2) :myzip3 xs xs1 xs2

aplica2 :: (a -> a) -> a -> a
--aplica2 f x = f (f x)
--aplica2 f = f.f
--aplica2 f = \x -> f (f x)
aplica2  = \f x -> f (f x)

-- L3.3
{-

map (\ x -> 2 * x) [1 .. 10]
map (1 `elem` ) [[2, 3], [1, 2]]
map ( `elem` [2, 3] ) [1, 3, 4, 5]

-}
-- firstEl [ ('a', 3), ('b', 2), ('c', 1)]
-- sumList [[1, 3],[2, 4, 5], [], [1, 3, 5, 6]]
-- prel2 [2,4,5,6]


firstEl l =map fst l
sumList l=map sum l
prel2 l=map aux l

aux::Int->Int
aux 0=0
aux x
    |odd x=x*2
    |otherwise=x`div` 2

--l34

ex1 c l=filter(c `elem`) l
ex3 l=map (\(x,y)->x*x) (filter(\(x,y)->odd y) (zip l [0..]))
ex2 l=map (^2) (filter (\x->odd x) l )
ex4 l=map ( \s->filter (\c->c `elem` "aeiou") s) l

mymap:: (a -> b) -> [a] -> [b] 
mymap _ []=[]
mymap f (h:t)=f h:mymap f t

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ []=[]
myfilter f (h:t)
    |f h = h : myfilter f t
    |otherwise=myfilter f t


