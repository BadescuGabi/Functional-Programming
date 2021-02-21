import Data.List (intersect)
import Data.Char ()
import Numeric.Natural ()

produsRec::[Int]->Int
produsRec []=1
produsRec (h:t)=h*produsRec t

produsF l=foldr (*) 1 l

andRec::[Bool]->Bool
andRec []=True
andRec (h:t)=h && andRec t

andF l=foldr (&&) True l

concatRec::[[a]]->[a]
concatRec []=[]
concatRec (h:t)=h++concatRec t

concatF l=foldr (++) [] l

rmChar :: Char -> String -> String
rmChar c []=[]
rmChar c (h:t)
    |c/=h =h:rmChar c t
    |otherwise=rmChar c t

rmCharsRec :: String -> String -> String
rmCharsRec cs [] = []
rmCharsRec cs (x:xs) 
    | elem x cs     = rmCharsRec cs xs
    | otherwise     = x : rmCharsRec cs xs

rmCharF :: String -> String -> String
rmCharF cs cuv = foldr (:) [] (filter (\ x -> elem x cs == False) cuv) 

numerePrimeCiur ::Int->[Int]
numerePrimeCiur 1=[]
numerePrimeCiur 0=[]
numerePrimeCiur n=auxCiur [2..n]
    where 
        auxCiur::[Int]->[Int]
        auxCiur []=[]
        auxCiur (h:t)=h: auxCiur [x|x<-t ,x `mod`h>0]

ordonataNat l=and [(x<=y)|(x,y)<-l `zip` tail l]
        

ordonataNat1 []=True
ordonataNat1 [x]=True
ordonataNat1 (h:t)=h<head (t) && ordonataNat1 (t)


ordonata [] _ = True
ordonata [_] _ = True
ordonata (x:xs) rel = and [a `rel` b | (a, b) <- zip (x:xs) xs] --for in for

(*<*) a b
    |(fst a)<(fst b) && (snd a)<(snd b)=True
    |otherwise=False

compuneList :: (b->c) -> [(a->b)] -> [(a->c)]
compuneList f lf = [ f.x  | x<- lf]

aplicaList :: a -> [(a->b)] ->[b]
aplicaList x l= [f x| f<-l]

myzip32 :: [a] -> [b] -> [c] -> [(a, b, c)]
myzip32 la lb lc = map (\((a, b), c) -> (a, b, c)) (zip (zip la lb) lc)