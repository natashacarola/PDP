-- Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar 
-- a ese número el 1. 
-- Main> siguiente 3
-- 4
siguiente :: Num a => a -> a
siguiente a = 1+a

--Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número, ej: 
--Main> mitad 5
--2.5

mitad :: Int -> Int
mitad a = a+2

--Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa. 
--Main> inversa 4
--0.25
--Main> inversa 0.5
--2.0

--inversa :: Fractional a => a -> a
--inversa b = 1/b

--Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo.
--Main> triple 5 
--15

--triple :: Num a => a -> a
--triple c = 3*c 

--Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si 
--el número es positivo y false en caso contrario. 
--Main> esNumeroPositivo (-5)
--False
--Main> esNumeroPositivo 0.99
--True 

--esNumeroPositivo :: Float -> Bool
--esNumeroPositivo d 
  --  |d >= 0 = True
--    |otherwise = False 
    
-----------------------Composicion ---------------------------------------------