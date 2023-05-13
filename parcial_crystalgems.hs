data Aspecto = UnAspecto {
  tipoDeAspecto :: String,
  grado :: Float
} deriving (Show, Eq)

type Situacion = [Aspecto]

mejorAspecto :: Aspecto -> Aspecto -> Bool
mejorAspecto mejor peor = grado mejor < grado peor


mismoAspecto :: Aspecto -> Aspecto -> Bool
mismoAspecto aspecto1 aspecto2 = tipoDeAspecto aspecto1 == tipoDeAspecto aspecto2


buscarAspecto :: Aspecto -> [Aspecto] -> Aspecto
buscarAspecto aspectoBuscado = head.filter (mismoAspecto aspectoBuscado)


buscarAspectoDeTipo :: String -> [Aspecto] -> Aspecto
buscarAspectoDeTipo tipo = buscarAspecto (UnAspecto tipo 0)


reemplazarAspecto :: Aspecto -> [Aspecto] -> [Aspecto]
reemplazarAspecto aspectoBuscado situacion =
    aspectoBuscado : filter (not.mismoAspecto aspectoBuscado) situacion

{-

Definir modificarAspecto que dada una función de tipo (Float -> Float) y un aspecto, modifique el aspecto alterando su grado en base
a la función dada.

-}

funcion :: Float -> Float 
funcion = (*2).(+1)


--modificarAspecto :: (Float -> Float) -> Aspecto -> Aspecto

{-
Saber si una situación es mejor que otra: esto ocurre cuando, para la primer situación, cada uno de los aspectos, es mejor que
ese mismo aspecto en la segunda situación.
Nota: recordar que los aspectos no necesariamente se encuentran en el mismo orden para ambas situaciones. 
Sin embargo, las situaciones a comparar siempre tienen los mismos aspectos.

-}

{-
Definir una función modificarSituacion que a partir de una situación permita obtener otra de modo que se modifique de 
cierta forma el grado correspondiente a un tipo de aspecto buscado. La alteración a realizar sobre el grado actual de ese aspecto debe 
poder ser indicada al usar la función.
-}
