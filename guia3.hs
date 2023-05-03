--Tuplas

--Definir las funciones fst3, snd3, trd3, que dada una tupla de 3 elementos devuelva el elemento correspondiente, p.ej. 
--Main> snd3 (4,5,6) 
--5
--Main> trd3(4,5,6)
--6

fst3 :: (a,a,a) -> a
fst3 (a ,_ ,_) = a

snd3 :: (a,a,a) -> a
snd3 (_,e,_) = e

trd3 :: (a,a,a) -> a
trd3 (_,_,i) = i

--Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones y un entero, 
--me devuelve como resultado una tupla con el resultado de aplicar el elemento a cada una de la funciones, ej: 
--Main> aplicar (doble,triple) 8 
--(16,24) 
--Main> aplicar ((3+),(2*)) 8 
--(11,16)

aplicar :: (t -> a, t -> b) -> t -> (a, b)
aplicar (x , z) y = (x y , z y )


--Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es mayor al segundo devuelve la suma, si el segundo le lleva más de 10 
--al primero devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega a llevarle 10, devuelve el producto. Ej: 
--Main> cuentaBizarra (5,8)
--40
--Main> cuentaBizarra (8,5)
--13
--Main> cuentaBizarra (5,29)
--24


cuentaBizarra :: (Ord a, Num a) => (a, a) -> a
cuentaBizarra (x ,y) 
    |x>y =  x+y
    |x<y && x< (y-x) = y-x
    |otherwise = y*x


--Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en caso contrario. No vale usar guardas. 

esNotaBochazo ::  Int -> Bool
esNotaBochazo nota = nota < 6

--Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo. 

aprobo :: (Int, Int) -> Bool
aprobo (nota1,nota2) = not (esNotaBochazo nota1) && not (esNotaBochazo nota2)

--Definir la función promociono, que indica si promocionó, para eso tiene las dos notas 
--tienen que sumar al menos 15 y además haberse sacado al menos 7 en cada parcial. 

promociono :: (Int,Int) -> Bool
promociono (nota1,nota2)
    | nota1+nota2 >= 15 && nota1 > 6 && nota2 >6 = True
    | otherwise = False 

--Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo y composición. 
--La consulta tiene que tener esta forma (p.ej. para el par de notas (5,8)) 
--Main> (... algo ...) (5,8) 

consulta :: Bool
consulta = (esNotaBochazo.fst) (5,8)


--Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios, lo representamos mediante 
--un par de pares ((parc1,parc2),(recup1,recup2)). 
--Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente. 
--Observamos que con la codificación elegida, siempre la mejor nota es el máximo entre nota del parcial y nota del recuperatorio. 
--Considerar que vale recuperar para promocionar. En este ejercicio vale usar las funciones que se definieron para el anterior. 


type Alumno = ((Int,Int),(Int,Int))

--Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a las notas finales del alumno para 
--el 1er y el 2do parcial. P.ej. 
--Main> notasFinales ((2,7),(6,-1)) 
--(6,7) 
--Main> notasFinales ((2,2),(6,2)) 
--(6,2) 
--Main> notasFinales ((8,7),(-1,-1)) 
--(8,7)

notasFinales :: Alumno -> (Int, Int)
notasFinales ((parc1,parc2),(recu1,recu2)) 
    |aprobo (parc1,parc2) = (parc1,parc2)
    |aprobo(parc1,recu2) = (parc1,recu2)
    |not (aprobo (parc1,recu2)) && recu2>=parc2 && parc1>=6 = (parc1,recu2)
    |not (aprobo (parc1,recu2)) && recu2<parc2 && parc1 >=6 = (parc1,parc2)
    |aprobo (recu1,recu2)  = (recu1,recu2)
    |aprobo(recu1,parc2) = (recu1,parc2)
    |not (aprobo (recu1,parc2)) && recu1>=parc1 && parc2>=6 = (recu1,parc2)
    |otherwise = (recu1,parc2)
    
--Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recursa o no. O sea, la respuesta debe ser 
--True si recursa, y False si no recursa. Usar las funciones definidas en este punto y el anterior, y composición. La consulta debe tener esta forma:
--Main> (... algo ...) ((2,7),(6,-1)) 

consulta2 :: Bool
consulta2 = (aprobo.notasFinales) ((2,7),(6,-1))

--Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recuperó el primer parcial. 
--Usar composición. La consulta debe tener esta forma:
--Main> (... algo ...) ((2,7),(6,-1)) 

