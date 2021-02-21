import Data.List
import Data.Char
data Fruct
    = Mar String Bool
    | Portocala String Int
    deriving(Show)

     
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar x y)=False
ePortocalaDeSicilia (Portocala x y)
    |x=="Tarocco" || x=="Sanguinello" ||x== "Moro" =True
    |otherwise=False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia []=0
nrFeliiSicilia  ((Mar x y):t) = 0+ nrFeliiSicilia t
nrFeliiSicilia  ((Portocala x y):t) = y + nrFeliiSicilia t


--ePortocalaDeSicilia :: Fruct -> Bool
--ePortocalaDeSicilia (Portocala soi _) = elem soi ["Tarocco", "Moro", "Sanguinello"]
--ePortocalaDeSicilia _ = False


nrMereViermi :: [Fruct] -> Int
nrMereViermi []=0
nrMereViermi ((Portocala x y): t)=0+ nrMereViermi t
nrMereViermi ((Mar x y):t) 
    |y==True = 1+ nrMereViermi t
    |otherwise=0+nrMereViermi t

--EX2
data Linie = L [Int]
    deriving Show
data Matrice = M [Linie]


verifica :: Matrice -> Int -> Bool
verifica (M l) n = foldr (&&) True [sum x == n | (L x) <- l]

instance Show Matrice where
      show (M[])=""
      show (M ls) = (showL(head ls)) ++ "\n" ++ show (M(tail ls))
                  where showL (L ln ) = show ln 

doar (M l) n = foldr (&&) True [ y>0 | (L x) <- l, y<-x ,length x==n]