import Data.List
myInt = 55555555555555555555555555555555555555555555555555555555555
double :: Integer->Integer
double x = x+x

maxim::Integer->Integer->Integer
maxim a b= if a>b then a else b

maxim3::Integer->Integer->Integer->Integer
maxim3 a b c=maxim a (maxim b c)

maxim4::Integer->Integer->Integer->Integer->Integer
maxim4 a b c d=
    let
        u =maxim a b 
        z=maxim c d
    in maxim u z

data Alegere
    = Piatra
    | Foarfeca
    | Hartie
    deriving (Eq, Show)

data Rezultat
    = Victorie
    |Infrangere
    |Egalitate
    deriving (Eq, Show)

partida::Alegere->Alegere->Rezultat
partida x y= 
    if x==y then Egalitate
    else if x==Hartie && y==Piatra then Infrangere
    else
        Victorie