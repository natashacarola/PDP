data Pelicula = UnaPelicula{
    nombre :: String,
    actores :: [String],
    duracion :: Int,
    anio :: Int
} deriving (Show, Eq)

taxi :: Pelicula
taxi = UnaPelicula "Taxi Driver" ["De Niro", "Foster", "Otro"] 113 1976

machete :: Pelicula
machete = UnaPelicula "Machete" ["De Niro" , "Rodriguez"] 105 2010

hp :: Pelicula
hp = UnaPelicula "Harry Patter 9" ["Daniel Radcliffe", "Emma Watson", "Rupert Grint"] 1000 2022

--Se quiere saber si un determinado actor trabajo en una pelicula

estaEnLaPelicula :: String -> Pelicula -> Bool
estaEnLaPelicula actor pelicula = actor `elem` actores pelicula

-- Se quiere verificar si es cierto que una pelicula es de cierto genero, lo cual
-- se deduce de sus actores. Si la mayoria de los actores son de un determinado
-- genero, entonces la pelicula es de dicho genero. Se cuenta con informacion acerca de
-- los actores de cada genero de la siguiente manera: 

type Genero = String
type Actores  = [String]

todosLosActores :: [(Genero , Actores)]
todosLosActores = [("comedia", ["Carrey", "Grint", "Stiller"]), ("accion" , ["Stallone", "Willis", "Suar"]) , ("drama", ["De Niro", "Foster"])]

{-
Se quiere verificar si es cierto que una película es de cierto género, lo cual se deduce de sus actores.
Si la mayoría de los actores son de un determinado género, entonces la película es de dicho género. 
Se cuenta con la información acerca de los actores de cada género, de la siguiente manera: 

-}


{-
esCiertoPeliculaEsDeGenero pelicula genero 
    | actores pelicula `elem` actoresXGenero genero todosLosActores = filter (`elem` ) 

-}

peliculaEsDeGenero :: Pelicula -> Genero -> Bool
peliculaEsDeGenero pelicula genero = length (filter (`elem`actores pelicula) (actoresXGenero todosLosActores genero ) ) > length (actoresXGenero todosLosActores genero ) `div` 2 --ojo aca la division es entera 3/2 = 1 deberia ser 1,5 

actoresXGenero :: [(Genero , Actores)] -> Genero -> Actores
actoresXGenero listactores genero 
    | genero == fst (head listactores) = snd (head listactores)
    | otherwise = actoresXGenero (tail listactores) genero 

{-
Existen diversos premios a las películas, como ser:
- Clásico setentista: cuando la película se estrenó entre 1970 y 1979
- Plomo: si la película dura más de 3 horas
- Tres son multitud: cuando una película tiene tres actores.
- N son multitud: cuando una película tiene N actores
-}

premioClasicoSetentista :: Pelicula -> Bool
premioClasicoSetentista pelicula = anio pelicula >= 1970  && anio pelicula <= 1979 

premioPlomo :: Pelicula -> Bool
premioPlomo pelicula = duracion pelicula > 180 

premioNsonMultitud :: Int -> Pelicula ->  Bool
premioNsonMultitud n pelicula = length (actores pelicula) > n

premioTresSonMultitud :: Pelicula -> Bool
premioTresSonMultitud = premioNsonMultitud 3 
{-
a) Definir lo que sea necesario para poder averiguar si una película ganó un determinado premio.
c) Mostrar ejemplos de invocación y respuesta
d) Inventar un nuevo premio, consistente con los anteriores.
premioTaquilla : dada una pelicula, es de taquilla si tiene mas de 2 actores estrellas: 

Actores estrellas = ["De Niro" , "Cruise" , "Otro"]
-}

ganoUnPremio :: Pelicula -> Int -> Bool
ganoUnPremio pelicula  n
    | premioClasicoSetentista pelicula = True
    | premioPlomo pelicula = True
    | premioNsonMultitud n pelicula  = True
    | otherwise = False

{-
ghci> ganoUnPremio taxi 5
True

en este caso taxi tiene premio setentista y plomo
ghci> ganoUnPremio hp 5  
True

hp tiene premio plomo 

ghci> ganoUnPremio machete 5
False

machete no tiene ningun premio
-}

actoresEstrella :: Actores
actoresEstrella = ["De Niro", "Emma Watson" , "Otro"]

premioTaquilla :: Pelicula -> Bool
premioTaquilla pelicula = length (filter ( `elem` actoresEstrella) (actores pelicula)) > 1 

{-
En el mundo hay festivales de cine que otorgan ciertos premios a las películas. Por ejemplo, Cannes otorga
los premios 3 son multitud y Clásico setentista. mientras que el festival de Berlín entrega los premios 4 son multitud,
plomo y Clasico setentista.

-}

festivalCannes :: [Pelicula -> Bool]
festivalCannes = [premioClasicoSetentista, premioTresSonMultitud]

festivalBerlin :: [Pelicula -> Bool]
festivalBerlin = [premioNsonMultitud 4, premioPlomo, premioClasicoSetentista]

{-
a) Definir la función que permita averiguar cuántos premios de los que otorga un festival puede recibir 
una película dada y mostrar al menos dos ejemplos de cómo utilizarla para diferentes festivales. 
-}


cuantosPremiosOtorgaUnFestival :: Pelicula -> [Pelicula -> Bool] -> Int
cuantosPremiosOtorgaUnFestival peli festival = length (filter (==True) (map (f peli) festival))  

f :: Pelicula -> (Pelicula -> Bool) -> Bool
f peli premio = premio peli

-- d) Representar un nuevo festival con al menos dos premios, en el que sea necesario utilizar composición de funciones

--festivalNatasha = [premioTaquilla, ]

{-

¿Qué pasaría si en una película hay infinitos actores? Justificar y ejemplificar con las funciones definidas

estaEnLaPelicula -> esta funcion podria nunca terminar de evaluarse pues, al ser infinita, no se podria asegurar que un 
actor no esta en la pelicula

premioNsonMultitud -> esta funcion tamspoco podria calcular la longitud de actores por ser infinita 

ganoUnPremio -> esta funcion fallaria en caso de llegar a la tercera guarda (y por ende tampoco llegaria a evaluar la cuarta jamas)

premioTaquilla -> esta funcion tampoco podria evaluarse ya que nunca podria saber si todos los actores de la pelicula son estrellas o no

cuantosPremiosOtorgaUnFestival -> fallaria si en el festival hay premios que requieran utilizar los actores, como es el caso
de premiosNSonMultitud.





-}
