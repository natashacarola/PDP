--funciones de orden superior 

-- map recibe una funcion y una lista y aplica esa funcion en la lista
-- filter toma una funcion que devuelve un booleano y una lista y devuelve los elementos que satisfacen la funcion
-- $ aplica una funcion, tambien se usa para no t ener q usar parentesis indica el orden de precedencia a la derecha? !!
--concatMap recibe una funcion y una lista, aplica la funcion en la lista y retorna una lista nueva quue concatena los elem
--que respondan a esa funcion
-- zipWith recibe una funcion y dos listas, retorna una lista q aplica la funcion elemento a elemento [f(elem1lista1, elem1lista1),f(elem2Lista1,elem2Lista2)]
--any toma una funcion que retorna un booleano, una lista, y retorna si algun elemento de la lista satisface el predicado


--tp
data Auto = UnAuto {
    marca::String,
    modelo:: Int,
    kilometraje:: Float } deriving (Show, Eq)

data Persona = UnaPersona{
    nombre :: String ,
    pagaimpuesto :: Bool ,
    autos :: [Auto] } deriving(Show, Eq)


ferrari,fitito, reno,bmw :: Auto
ferrari = UnAuto "ferrari" 1990 100
fitito = UnAuto "fiat" 1960 1000000
reno = UnAuto "renault" 2023 0
bmw = UnAuto "bmw" 2021 20000

personaUno, personaDos, personaTres :: Persona
personaUno = UnaPersona "Natasha" True [ferrari, fitito, reno]
personaDos = UnaPersona "Federico" False []
personaTres = UnaPersona "Juan" True [ferrari,reno]

ejemploTupla :: (a, b) -> b
ejemploTupla (_ , x) = x


listadoAutosxPersona :: Persona -> [Auto]
listadoAutosxPersona (UnaPersona _ _ listaAutos) = listaAutos

calcularElValor x = kilometraje + modelo 
-- valorAuto UnaPersona = 
--Se considera millonario a una persona si la sumatoria del valor de sus autos es mayor a 1000000
--Se considera honesto a quien paga sus impuestos
--El valor de un auto se calcula con una extraña fórmula en la que interviene el kilometraje y modelo del año, 
--con un incremento si se trata de una marca importada. (Inventarla!!)
--Se sabe cuáles son las marcas importadas.