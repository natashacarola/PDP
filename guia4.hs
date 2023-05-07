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

a = a+1

promedioFrecuenciaCardiaco :: [Int] -> Float
promedioFrecuenciaCardiaco lista2 = fromIntegral (sum lista2) / fromIntegral (length lista2)
