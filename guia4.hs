--Definir una funcion que sume una lista de numeros 

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista lista = head lista + sumaLista (tail lista)

--Durante un entrenamiento físico de una hora, cada 10 minutos de entrenamiento se tomóo la frecuencia cardíaca de uno de los participantes 
--obteniéndose un total de 7 muestras que son las siguientes:
--frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 
--Comienza con un frecuencia de 80 min 0. 
--A los 10 min la frecuencia alcanza los 100 
--A los 20 min la frecuencia es de 120, 
--A los 30 min la frecuencia es de 128
--A los 40 min la frecuencia es de 130, …etc.. 
--A los 60 min la frecuencia es de 125 frecuenciaCardiaca es un función constante. 


--Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca. 
--Main> promedioFrecuenciaCardiaca 
--115.285714285714

--assasdas

a :: Integer -> Integer
a = (+1)

promedioFrecuenciaCardiaco :: [Int] -> Float
promedioFrecuenciaCardiaco lista2 = fromIntegral (sum lista2) / fromIntegral (length lista2)


{- Definir la función esCapicua/1, si data una lista de listas, me devuelve si la concatenación de las 
sublistas es una lista capicua..Ej: 
Main> esCapicua ["ne", "uqu", "en"] 
True 
Porque “neuquen” es capicua.
Ayuda: Utilizar concat/1, reverse/1. -}


esCapicua :: [String] -> Bool
esCapicua lista = concat lista == reverse (concat lista)

{-Se tiene información detallada de la duración en minutos de las llamadas que se llevaron a cabo en un 
período determinado, discriminadas en horario normal y horario reducido. 
duracionLlamadas = (("horarioReducido",[20,10,25,15]),(“horarioNormal”,[10,5,8,2,9,10])). -}

duracionllamadas :: ((String, [Int]), (String, [Int]))
duracionllamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

{-Definir la función cuandoHabloMasMinutos, devuelve en que horario se habló más cantidad de minutos, 
en el de tarifa normal o en el reducido. 
Main> cuandoHabloMasMinutos 
“horarioReducido” -}