--Ultima clase de funcional

{--

Listas infinitas, evaluacion diferida (lazy evaluation)

lo q plantea es que a diferencia de como se evalua en el paradigma
aca se hace de forma distinta y permite manejar listas infinitas. 

terminal de haskell: 
ghci > take 5 "hola que tal"
"hola "
ghci > take 6 "hola" ++ "que tal"
"hola q"
ghci > take 3 "hola que tal"
"hol"
ghci > take 3 "hola" ++ "que tal"
"hol"

los argumentos no se evaluan hasta que la funcion principal los requiera si yo quisiera hacer take 6 de algo que tengo concatenado, 
toma el 6, evalua la concatenacion y ccon las primera le alcanza, entonces hasta ahi concatena: 
take 6("hola" ++ "que tal" ++ "como estas") en este caso no le interesa el como estas, ya que con que tal alcanza.

potencia base 0 = 1
potencia base exponente =  base * potenncia base (exponente)

ghci>potencia (2/0) 0 
da 1 porque no necesita evaluar 2/0 si yo hiciera
ghci> (2/0) 3 devuelve infinito porque si necesita evaluar 2/0

precio "papa" =  20
precio "tomate" = 100 
ghci> potencia (precio "perejil") 0 
devuelve 1. cuando deberia dar un error, ya que perejil no esta definido

Una de las posibles aplicaciones de la evaluacion diferida es poder labuar con expresiones infinitas


listaInfinita = 1:listaInfinita
: dado un elemento y una lista, agrega ese elemento a la lista
en sintesis [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1...1]
ghci> take  5 listaInfinita 
[1,1,1,1,1]
ghci> take 400000 listaInfinita
tambien funciona
!!!!!!!!!!!ctrl c -> para interrumpir la ejecucion

ghci>take 20 (repeat 1)
[1,1,1....1] hasta 20

secuencia n = n: secuencia(n+1)
ghci>take 20 (secuencia 1)
[1..20]

secuencia n = n : secuencia (n*2)

ghci>(secuencia 1) !! 3 
1024 -> !! devuelve el elemento que esta en la posicion 3

potenciaDe2 n = secuencia 1 !! n

aproximacion n  = n : aproximacion (1/(n+1))
ghci> take 10 (aproximacion 1)
..probar 
ghci> (aproximacion 1) !! 10 

--Lista infinita de autos: 

data Auto = UnAuto {
    completar
}

crearAuto n = UnAuto{
    marca = "renault",
    kilometraje = n,
    modelo = 2023
}

ghci>take 10 (map crearAuto [1..])     --probar

Problemas: cuando queremos ver el final de la lista aparecen los problemas, de otra manera siempre se pueden usar listas
infinitas, siempre y cuando no implique llegar al final 


un any nunca da False en una lista infinita, pues nunca puede terminar de recorrerla -> es posible que explote primero xd
foldl va a fallar ya que requiere llegar al final 
anotar: map  filter de la grabacion



--}

--datos

data Auto = UnAuto {
    marca::String,
    modelo:: Int,
    kilometraje:: Float } deriving (Show, Eq)

data Persona = UnaPersona{
    nombre :: String ,
    pagaimpuesto :: Bool ,
    autos :: [Auto] } deriving(Show, Eq)

--actividad y funciones

--construir persona con infinitos autos
crearAuto :: Float -> Auto
crearAuto n = UnAuto{
    marca = "renault",
    kilometraje = n,
    modelo = 2023
}

ferrari,fitito, reno,bmw :: Auto
ferrari = UnAuto "ferrari" 1990 100
fitito = UnAuto "fiat" 1960 1000000
reno = UnAuto "renault" 2023 0
bmw = UnAuto "bmw" 2021 20000

personaConInfinitosAutos :: Float -> Persona
personaConInfinitosAutos n = UnaPersona{
    nombre = "juan",
    pagaimpuesto = True,
    autos = [crearAuto n]
}
--como calcular su patrimonio (contemplar algunos autos)
--comprarAuto

autosInfinitos :: [Auto]
autosInfinitos = map crearAuto [1..]
--Averiguar si persona tiene algun auto de mas de 10000 km
--que la persona venda todos sus autos con cantidad par de km

cachito :: Persona
cachito = UnaPersona "cacho" True autosInfinitos

lola :: Persona
lola = UnaPersona "lola" False [ferrari, fitito]


otraLista = [x*10| x <- [1..10],even x]

h =  [(y,x)| x <- [1..12], y<-["oro","copa","espada","basto"] , x<8 || x >9]

g :: Num a => [a] -> a
g (cabeza:_) = cabeza + 1


f :: Num a1 => [a2] -> a1
f [] = 0
f (_:cola) = f cola + 1


head' (x:_) = x

--

instance Show Auto where
    show a = "Hola, soy un auto"