--Desarrollar los siguientes deseos y declarar el data Chico

data Chico = UnChico {
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [Chico -> Int]
}

--aprenderHabilidades habilidades unChico : agrega una lista de habilidades nuevas a las que ya tiene el chico

aprenderHabilidades :: Chico -> String -> [String]
aprenderHabilidades chico habilidad = habilidad : habilidades chico

--serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades de jugar a todas las versiones pasadas y futuras del Need For Speed, que son: "jugar need for speed1", "jugar need for speed 2", etc

serGrosoEnNeedForSpeed :: String -> Chico -> [String]
serGrosoEnNeedForSpeed nfs chico = agregarJueguito nfs : habilidades chico

agregarJueguito :: [Char] -> [Char]
agregarJueguito nfs = "Jugar need for speed " ++ nfs


--serMayor unChico : hace que el chico tenga 18 años

serMayor :: Chico -> Int
serMayor chico   
    | edad chico > 18 = edad chico
    | otherwise = 18


--2) los padrinos son seres magicos capaces de cumplir los deseos de sus ahijados

--wanda : dado un chico, wanda le comple el primer deseo y lo hace madurar (crecer un año de edad)

timmy :: Chico
timmy = UnChico "Timmy" 12 ["nadar", "jugar"] [serMayor]

type Wanda = Chico -> Chico

eliminarDeseo :: Chico -> [Chico -> Int]
eliminarDeseo chico 
    | length (deseos chico) > 1 = tail (deseos chico)
    | otherwise = []

madurarUnAño :: Chico -> Int
madurarUnAño chico = edad chico + 1

wanda :: Chico -> Chico
wanda chico = UnChico (nombre chico) (madurarUnAño chico) (habilidades chico) (eliminarDeseo chico)

--cosmo: dado un chico, lo hace "des"madurar, quedando con la mitad de de años de edad. Como es olvidadizo, no le concede ningun deseo

type Cosmo = Chico -> Chico

desmadurar :: Chico -> Int
desmadurar chico = div (edad chico) 2

cosmo :: Chico -> Chico
cosmo chico = UnChico (nombre chico) (desmadurar chico) (habilidades chico) (deseos chico)

--muffinMagico: dado un chico le concede todos sus deseos

muffinMagico :: Chico -> Chico
muffinMagico chico = UnChico (nombre chico) (edad chico) (habilidades chico) []

--PARTE B: En busqueda de pareja

--Se acerca el baile de fin de año y se quiere saber cuales van a ser las parejas. Para esto, las chicas tienen condiciones para elegir al chico
--con el que van a salir, algunas de ellas son: 

--tieneHabilidad unaHabilidad unChico : Dado un chico y una habilidad dice si la posee

tieneHabilidad :: String -> Chico -> Bool
tieneHabilidad habilidad chico= habilidad `elem` habilidades chico

--esSuperMaduro: dado un chico dice si es mayor de edad (es decir, tiene mas de 18 años) y ademas sabe manejar

esSuperMaduro :: Chico -> Bool
esSuperMaduro chico = edad chico >=18 && ( "sabe manejar" `elem` habilidades chico) 

--Las chicas tienen  un nombre, y una condicion para elegir al chico con el que van a ir al baile
--Ej: para Trixie la unica condicion es que el chico no sea Timmy ya que nunca saldria con el.
--trixie = Chica "Trixie Tang" noesTimmy 
--vicky = Chica "Vicky" (tieneHabilidad "ser un supermodelo noruego") 

data Chica = UnaChica{
    nombreChica :: String,
    condicion :: Chico -> Bool
}

noesTimmy :: Chico -> Bool
noesTimmy chico 
    | nombre chico == "Timmy" = False
    | otherwise = True

trixie :: Chica
trixie = UnaChica "Trixie Tang" noesTimmy


vicky :: Chica
vicky = UnaChica "Vicky" (tieneHabilidad "ser un supermodelo noruego")

--Se pide definir y desarrollar las siguientes funciones: 
-- quienConquistaA unaChica losPretendientes : dada una chica y una lista de pretendientes, devuelve al que se queda con la chica (chico que cumple con la condicion
--que ella quiere). Si no hay ninguno que la cumpla, devuelve el ultimo pretendiente --> Recursividad

cumpleCondicionChica :: Chica -> [Chico] -> [Chico]
cumpleCondicionChica chica = filter (condicion chica)


quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA chica (pretendiente: pretendientes) 
    | length (cumpleCondicionChica chica (pretendiente: pretendientes)) == 1 = pretendiente
    | otherwise = ultimoChicoDeLaLista pretendientes
    
ultimoChicoDeLaLista :: [Chico] -> Chico
ultimoChicoDeLaLista [primerPretendiente] = primerPretendiente
ultimoChicoDeLaLista (pretendiente : pretendientes) 
    | length (pretendiente: pretendientes) >1 = ultimoChicoDeLaLista pretendientes
    |otherwise = pretendiente

jorgen :: Chico
jorgen = UnChico "jorgen" 40 ["ser un supermodelo noruegoo"] [serMayor]

juanito :: Chico
juanito = UnChico "juanito" 22 ["no tiene"] [serMayor]

listaChicos :: [Chico]
listaChicos = [jorgen, timmy, juanito]

--Dar un ejemplo de consulta para una nueva chica cuya condicion es que sepa cocinar

nuevaChica :: Chica
nuevaChica = UnaChica "Nueva chica" (tieneHabilidad "sepa cocinar")



--PARTE 3: Da Rules
--infractoresDeDaRules : Dada una lista de chicos, devuelve la lista de los nombres 
-- de aquellos que tienen deseos prohibidos. Un deseo esta prohibido si, al aplicarlo,
-- entre las cinco primeras habilidades, hay alguna prohibida
-- Habilidades prohibidas: enamorar, matar y dominar el mundo

habilidadesProhibidas :: [String]
habilidadesProhibidas = ["enamorar", "matar" ,"dominar el mundo"]

{-
infractoresDeDaRules :: [String] -> [Chico] -> Bool
infractoresDeDaRules habilidadesProhibidas chicos 
    | not (elem head habilidadesProhibidas (map habilidades chicos)) = infractoresDeDaRules (tail habilidadesProhibidas) chicos
    | elem head habilidadesProhibidas (map habilidades chicos) = True 
    |otherwise = False

-}