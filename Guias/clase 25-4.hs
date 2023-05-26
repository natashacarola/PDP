{-
Concesionarias

Llegan las concesionarias que se dedican a vender autos. Cada concesionaria se dedica a una marca de auto que vende a sus clientes, 
pero no les venden a cualquiera, cada una tiene sus preferencias. Siempre son 0km.
-Por ejemplo, la concesionaria "pdepautos" vende vehiculos "Ferrari" a los clientes millonarios.
-Hay varias concesionarias que venden vehiculos de alguna marca en particular, pero a los clientes que ya tengan una cierta cantidad de autos.
-Por ej, la concesionaria "funcional" vende autos "Hasklell" a quienes tengan al menos 2 autos, y hay otra que vende a quienes tengan al menos 10 autos
1) Definir otras concesionarias que tengan otras preferencias de clientes
2) Implementar una funcion por la que una persona va a una concesionaria y en caso de satisfacer su preferencia, se compra 
un auto. Se quiere obtener como queda dicha persona.
3)Averiguar si una persona, luego de visitar una serie de concesionarias, no se compro nada
4) Dado un conjunto de personas, obtener el mejor auto entre todos los autos de dichas personas, con vaiantes acerca de 
que se considera mejor 
    a- El mejro es el de mas valor
    b- El mejor es el mas nuevo
    c- El mejor es el que tiene mas km recorridos
    d- Inventar un nuevo criterio 

preferencia :: Persona -> Bool --ejemplo 
-}
--Tipos de datos
data Auto = UnAuto {
    marca::String,
    modelo:: Int,
    kilometraje:: Float } deriving (Show, Eq)

data Persona = UnaPersona{
    nombre :: String ,
    pagaimpuesto :: Bool , 
    autos :: [Auto] } deriving(Show, Eq)

data Concesionaria = UnaConcesionaria{
    nombreC :: String,
    preferencias :: Persona -> Bool , 
    especializadaEn :: Auto }

--Concesionarias de ejemplo

fierrosJuan :: Concesionaria
fierrosJuan = UnaConcesionaria "FierrosJuan" esMillonarioHonesto lambo 
fierrosPepe :: Concesionaria
fierrosPepe = UnaConcesionaria "FierrosPepe" minimoXPersona haskell 
pdepautos = UnaConcesionaria "pdepautos" 
--Autos de ejemplo

ferrari, fitito, reno, bmw, lambo, haskell :: Auto
ferrari = UnAuto "ferrari" 1990 100
fitito = UnAuto "fiat" 1960 100000
reno = UnAuto "renault" 2023 0
bmw = UnAuto "bmw" 2021 200000
lambo = UnAuto "lambo" 2023 0
haskell = UnAuto "haskell" 2023 0

--Personas de ejemplo 

personaUno, personaDos, personaTres, personaCuatro :: Persona
personaUno = UnaPersona "Natasha" True [ferrari, fitito, reno, bmw, lambo] --millonario honesto
personaDos = UnaPersona "Federico" False [ferrari, fitito, reno, bmw] --millonario deshonesto(?)
personaTres = UnaPersona "Juana" True [bmw] -- no es millonario y encima no es honesto
personaCuatro = UnaPersona "Camilo" True [fitito, reno, bmw] --es Millonario pero solo tiene un importado(?)

-- Definicion de lista de autos importados

importado :: [String]
importado = ["ferrari", "bmw"]

-- Funciones 

esImportado :: Auto -> Bool
esImportado (UnAuto marca _ _) = marca `elem` importado

extraniaFormula :: Auto -> Float
extraniaFormula (UnAuto marcauto modeloauto kilometrajeauto)
    | esImportado (UnAuto marcauto modeloauto kilometrajeauto) = 800000 - (kilometrajeauto * 2) + fromIntegral modeloauto 
    | otherwise = 400000 - (kilometrajeauto * 2) + fromIntegral modeloauto

valorDeAutos :: [Auto] -> Float 
valorDeAutos listAuto = sum (map extraniaFormula listAuto) 

esMillonario :: [Auto] -> Bool 
esMillonario listaAutos = valorDeAutos listaAutos > 1000000

esMillonarioHonesto ::  Persona -> Bool
esMillonarioHonesto (UnaPersona _ pagaimpuesto listauto) = pagaimpuesto && esMillonario listauto

cuantosMillonariosHonestosHay :: [Persona] -> Int 
cuantosMillonariosHonestosHay listaPersonas = length (filter (== True) (map esMillonarioHonesto listaPersonas))

minimoXPersona :: Persona -> Bool
minimoXPersona p = length (autos p) > 2

--compraEnConcesionaria :: Persona -> Concesionaria -> [Auto]
--compraEnConcesionaria personarandom (UnaPersona a b c) concesionariaRandom (UnaConcesionaria d e f) autoRandom= 
  --      |d personarandom = autos autoRandom ++
    --    |otherwise = autos p

