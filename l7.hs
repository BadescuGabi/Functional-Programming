import Test.QuickCheck
import Data.List
import Data.Char
import Test.QuickCheck.Gen (Gen(MkGen, unGen))
myreverse::[Int]->[Int]
myreverse []=[]
myreverse (x:xs)=(myreverse xs) ++[x]

prdef::[Int]->Bool
prdef xs= (myreverse xs==reverse xs)
wpr::[Int]->Bool
wpr xs=(myreverse xs==xs)

rval i = (7*i+3 ) `mod` 11 
genRandSeq 0 _ = []
genRandSeq n s = rval s:(genRandSeq (n-1) (rval s))
--laborator

--2
double :: Int -> Int
double x=x*2 
triple :: Int -> Int
triple x=x*3
penta :: Int -> Int
penta x= x*5

--3

test x = (double x + triple x) == (4* x)


--7
myLookUp :: Int -> [(Int, String)] -> Maybe String
myLookUp _ [] = Nothing
myLookUp nr (x:xs)
    | nr == fst x =Just (snd x)
    | otherwise = myLookUp nr xs

testLookUp :: Int -> [(Int,String)] -> Bool
testLookUp nr (xs)=(lookup nr (xs))==(myLookUp nr (xs)) 

testLookUpCond :: Int -> [(Int,String)] -> Property
testLookUpCond n list = n > 0 && n `div` 2 == 0 ==> testLookUp n list

myLookUp' :: Int -> [(Int, String)] -> Maybe String
myLookUp' _ [] = Nothing
myLookUp' nr (x:xs)
    | nr == fst x =Just (toUpper (head (snd x)):tail(snd x))
    | otherwise = myLookUp' nr xs    




testLookUpCap :: Int -> [(Int, String)] -> Property
testLookUpCap n list = list == (map(\(x,y) -> (x, cap y)) list) ==> testLookUp n list
cap :: String -> String
cap sir = toUpper (head sir) : tail sir

data ElemIS = I Int | S String
    deriving (Show,Eq)

instance Arbitrary ElemIS where 
    arbitrary = oneof [geni, gens]
        where 
            f = (unGen (arbitrary :: Gen Int))
            g = (unGen (arbitrary :: Gen String))
            geni = MkGen (\s i -> let x = f s i in (I x))
            gens = MkGen (\s i -> let x = g s i in (S x))
            
myLookUpElem ::  Int -> [(Int,ElemIS)]-> Maybe ElemIS
myLookUpElem _ []=Nothing 
myLookUpElem nr (x:xs)
    | nr == fst x =Just (snd x)
    | otherwise = myLookUpElem nr xs  

testLookUpElem :: Int -> [(Int,ElemIS)] -> Bool
testLookUpElem  x l= lookup x l==myLookUpElem x l
    