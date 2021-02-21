import Numeric.Natural
import Data.Char
import Data.List
logistic :: Num a => a -> a -> Natural -> a
logistic rate start = f
    where
    f 0 = start
    f n = rate * f (n - 1) * (1 - f (n - 1))

logistic0 :: Fractional a => Natural -> a
logistic0 = logistic 3.741 0.00079

ex1 :: Natural
ex1 = 9999999999999999999999999999999999

ex20 :: Fractional a => [a]
ex20 = [1, logistic0 ex1, 3]
ex21 :: Fractional a => a
ex21 = head ex20
ex22 :: Fractional a => a
ex22 = ex20 !! 2
ex23 :: Fractional a => [a]
ex23 = drop 2 ex20
ex24 :: Fractional a => [a]
ex24 = tail ex20

ex31 :: Natural -> Bool
ex31 x = x < 7 || logistic0 (ex1 + x) > 2
ex32 :: Natural -> Bool
ex32 x = logistic0 (ex1 + x) > 2 || x < 7

--baza foldr universal
foldr_ :: (a -> b -> b) -> b -> ([a] -> b)
foldr_ op unit = f
    where
    f [] = unit
    f (a:as) = a `op` f as


sumaPatrateImpare :: [Integer] -> Integer
sumaPatrateImpare [] = 0
sumaPatrateImpare (a:as)
    | odd a = a * a + sumaPatrateImpare as
    | otherwise = sumaPatrateImpare as

sumaPatrateImpareFold :: [Integer] -> Integer
sumaPatrateImpareFold = foldr op unit
    where
    unit = 0
    a `op` suma
        | odd a = a * a + suma
        | otherwise = suma

map_ :: (a -> b) -> [a] -> [b]
map_ f [] = []
map_ f (a:as) = f a : map_ f as


mapFold :: (a -> b) -> [a] -> [b]
mapFold f = foldr op unit
    where
    unit = []
    a `op` l = f a : l
    
filter_ :: (a -> Bool) -> [a] -> [a]
filter_ p [] = []
filter_ p (a:as)
    | p a = a : filter_ p as
    | otherwise = filter_ p as

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold p = foldr op unit
    where
    unit = []
    a `op` filtered
        | p a = a : filtered
        | otherwise = filtered


semn :: [Integer] -> String
semn[] = []
semn (h:t)
    |h>0 && h<=9='+':semn t
    |h==0 ='0':semn t
    |h<0 && h>=(-9) ='-':semn t
    |otherwise=semn t


semnFold :: [Integer] -> String
semnFold = foldr op unit
    where
         unit = ""
         op a b
          | a >= -9 && a < 0 = "-" ++ b
          | a > 0  && a <= 9 = "+" ++ b
          | a == 0           = "0" ++ b
          | otherwise        = b


corect :: [[a]] -> Bool
corect []=True
corect (l:ls)= and[length x==length y|(x,y)<-(l:ls) `zip` ls ]

el :: [[a]] -> Int -> Int -> a
el l a b=l !! a !!b


transforma :: [[a]] -> [(a, Int, Int)]
transforma mat = [(b, i, j) | (a, i) <- zip mat [0..], (b, j) <- zip a [0..]]


