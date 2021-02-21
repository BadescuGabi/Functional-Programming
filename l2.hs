import Data.List
import Data.Char
fibonacciCazuri::Integer->Integer
fibonacciCazuri n
    |n<2 =n
    |otherwise= fibonacciCazuri(n-1)+fibonacciCazuri(n-2)

fibonacciEc::Integer->Integer
fibonacciEc 0=0
fibonacciEc 1=1
fibonacciEc n=fibonacciEc (n-1)+fibonacciEc(n-2) 

fibonacciLiniar :: Integer -> Integer
fibonacciLiniar 0 = 0
fibonacciLiniar n = snd (fibonacciPereche n)
  where
    fibonacciPereche :: Integer -> (Integer, Integer)
    fibonacciPereche 1 = (0, 1)
    fibonacciPereche n = (b, a+b)
            where
                (a,b) = fibonacciPereche (n-1)

semiPareDestr::[Int]->[Int]
semiPareDestr l
    |null l=l
    |even h= h `div` 2: t'
    |otherwise=t'
    where
        h=head l
        t=tail l
        t'=semiPareDestr t

semiPareEq::[Int]->[Int]
semiPareEq []=[]
semiPareEq(h:t)
    |even h=h `div` 2 : semiPareEq t
    |otherwise=semiPareEq t

semiPareComp::[Int]->[Int]
semiPareComp l=[x `div` 2| x<-l  , even x]

inIntervalRec::Int->Int->[Int]->[Int]
inIntervalRec a b []=[]
inIntervalRec a b (h:t)
    |h>=a && h<=b=h:inIntervalRec a b t
    |otherwise=inIntervalRec a b t

inIntervalComp::Int->Int->[Int]->[Int]
inIntervalComp a b l=[x|x<-l,x>=a && x<=b]

pozitiveRec::[Int]->Int
pozitiveRec []=0
pozitiveRec (h:t)
    |h>0=1+ pozitiveRec t
    |otherwise=pozitiveRec t

pozitiveComp::[Int]->Int
pozitiveComp l=length[x|x<-l,x>0]

pozitiiImpareRec::[Int]->[Int]
pozitiiImpareRec []=[]
pozitiiImpareRec l=  pozitiiImpareAux l 0
    where 
        pozitiiImpareAux::[Int]->Int->[Int]
        pozitiiImpareAux [] i=[]
        pozitiiImpareAux (h:t) i
            |odd h= i:pozitiiImpareAux t (i+1)
            |otherwise=pozitiiImpareAux t (i+1)

pozitiiImpareComp::[Int]->[Int]
pozitiiImpareComp l=[i| (x,i)<- l `zip` [0..] ,odd x]

multiDigitsRec::[Char]->Int
multiDigitsRec []=1
multiDigitsRec (h:t)
    |isDigit h= digitToInt h * multiDigitsRec t
    |otherwise=multiDigitsRec t

multiDigitsComp::[Char]->Int
multiDigitsComp l=product [digitToInt x|x<-l, isDigit x]

discountRec::[Float]->[Float]
discountRec []=[]
discountRec (h:t)
    |h*0.75<200=h*0.75:discountRec t
    |otherwise=discountRec t

discountComp::[Float]->[Float]
discountComp l=[x*0.75|x<-l,x*0.75<200]