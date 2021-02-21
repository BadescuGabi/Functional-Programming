import Test.QuickCheck
import Data.Char
import Data.List
import Data.Maybe
type Nume = String
data Prop= Var Nume
    | F
    | T
    | Not Prop
    | Prop :|: Prop
    | Prop :&: Prop
    |Prop :->: Prop
    |Prop :<->: Prop
    deriving Eq
infixr 2 :|:
infixr 3 :&:
infixr 4 :->:
infixr 5 :<->:
--1. (P ∨ Q) ∧ (P ∧ Q)

p1 :: Prop
p1 = (Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q")

--2. (P ∨ Q) ∧ (¬P ∧ ¬Q)
p2 :: Prop
p2 = (Var "P" :|: Var "Q") :&: (Not (Var "P") :&:Not (Var "Q"))
--3. (P ∧ (Q ∨ R)) ∧ ((¬P ∨ ¬Q) ∧ (¬P ∨ ¬R))
p3 :: Prop
p3 = (Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var"P") :|: Not (Var "Q")):|: (Not (Var "P" ):|: Not (Var "R")))

instance Show Prop where 
    show F = "F"
    show T = "T"
    show (Var p) = show p 
    show (Not prop) = "(~" ++ show prop ++ ")"
    show (prop1 :&: prop2) = "(" ++ show prop1 ++ " & " ++ show prop2 ++ ")"
    show (prop1 :|: prop2) = "(" ++ show prop1 ++ " | " ++ show prop2 ++ ")"
    show (prop1 :->: prop2) = "(" ++ show prop1 ++ " -> " ++ show prop2 ++ ")"
    show (prop1 :<->: prop2) = "(" ++ show prop1 ++ " <-> " ++ show prop2 ++ ")"
type Env = [(Nume, Bool)]

-- [("P", True), ("Q", False)]
-- Not (Var "P") :|: (Var "Q") = False 
-- Var "P" :|: Var "Q" = True 

env :: Env 
env = [("P", False), ("Q", False)]

impureLookup :: Eq a => a -> [(a, b)] -> b 
impureLookup x environment = fromJust (lookup x environment)

-- Exercitiul 3:
-- se dau o formula si un environment in care se face evaluarea
-- se de determine valoarea de adevar a formulei
eval :: Prop -> Env -> Bool 
eval (Var p) env = impureLookup p env 
eval T _ = True 
eval F _ = False 
eval (Not p) env = not (eval p env)
eval (p1 :&: p2) env = (eval p1 env) && (eval p2 env)
eval (p1 :|: p2) env = (eval p1 env) || (eval p2 env)
eval (p1 :->: p2) env=if ((eval p1 env)==True && (eval p2 env)==False)then True else False
eval (p1 :<->: p2) env=if ((eval p1 env)/=(eval p2 env)) then False else True
test_eval = eval (Var "P" :|: Var "Q") [("P", True), ("Q", False)] == True
--ex4
variabile :: Prop -> [Nume]
variabile (Var p)=[p]
variabile T=[]
variabile F=[]
variabile (Not p)=variabile p
variabile (p:|: q)=nub(variabile p ++ variabile q)
variabile (p:&: q)=nub(variabile p ++ variabile q)
variabile (p :->: q) = nub (variabile p ++ variabile q)
variabile (p :<->: q) = nub (variabile p ++ variabile q)
test_variabile =variabile (Not (Var "P") :&: Var "Q") == ["P", "Q"]

--ex5
envs :: [Nume] -> [Env]
envs [] = [[]]
envs ( x : xs ) = [ ( x , b ):e | b <- bs , e <- envs xs ]
    where
    bs = [ False , True ]
--ex6
satisfiabila :: Prop -> Bool
satisfiabila prop = or [eval prop enviroment | enviroment <- ls]
                        where ls = envs $ variabile $ prop
test_satisfiabila1 = satisfiabila (Not (Var "P") :&: Var "Q") == True
test_satisfiabila2 = satisfiabila (Not (Var "P") :&: Var "P") == False

--ex7
valida :: Prop -> [Bool]
valida prop=  [eval prop enviroment | enviroment <- ls]
    where ls = envs $ variabile $ prop
--test_valida1 = valida (Not (Var "P") :&: Var "Q") == False
--test_valida2 = valida (Not (Var "P") :|: Var "P") == True
valida1 :: Prop -> [([Nume],Bool,Env)]
valida1 prop=  [(nub(variabile prop),eval prop enviroment,enviroment) | enviroment <- ls]
    where ls = envs $ variabile $ prop

envir= [("P", True), ("Q", True)]
echivalenta :: Prop -> Prop -> [Bool]
echivalenta p1 p2 = valida ( p1 :<->: p2)

show_Bool :: Bool -> [Char]
show_Bool True = "T"
show_Bool False = "F"

tabelAdevar :: Prop -> String
tabelAdevar p = concat $ map (++ "\n") tabel
    where
    vars = variabile p
    afis_prima = concat $ (map (++ " ") vars) ++ [show p]
    evaluari = envs vars
    aux_af tv= (show_Bool tv) ++ " "
    afis_evaluari ev = concat $ (map aux_af [snd p | p <-ev]) ++ [show_Bool (eval p ev)]
    tabel = afis_prima : (map afis_evaluari evaluari)

echivalenta1 p1 p2 = foldr (&&) True [(eval p1 envir)==(eval p2 envir) | envir <- envs ( (variabile p1) ++ (variabile p2))]