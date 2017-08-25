import Diccionario
import Data.Maybe
import Arbol23
import Test.HUnit

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}

--En este caso el iterate lo que hace es crear la lista infinita de pistas y sus resultados a partir de la pista inicial,
--a esta lista se la recorre con foldr. Se utilizo este tipo de fold ya que por la evaluacion lazy el mismo funcion sobre
--listas infinitas, la funcion del fold se encarga de chequear si es una pista o un tesoro, en caso de tener una pista no
--definida se retorna Nothing dando por finalizada la busqueda, si es un tesoro tambien se detiene la recursion y se retorna
--el valor, por ultimo, si es una pista valida y aun no se llego al tesoro, se continua la busqueda con la proxima pista.
listaDePistas::Eq a=>a->Diccionario a a->[Maybe a]
listaDePistas pista dict = iterate (\pista -> case pista of
                                                Nothing -> Nothing
                                                Just datoPista -> obtener datoPista dict)
                                   (Just pista)

busquedaDelTesoro::Eq a=>a->(a->Bool)->Diccionario a a->Maybe a 
busquedaDelTesoro pista esTesoro dict = foldr (\datoBusqueda rec -> case datoBusqueda of
                                                                        Nothing -> Nothing
                                                                        Just dato -> if (esTesoro dato) then
                                                                                            Just dato
                                                                                        else
                                                                                            rec)
                                              Nothing
                                              (listaDePistas pista dict)


{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio9" ~: testsEj9,
  "ejercicio10" ~: testsEj10
  ]

testsEj2 = test [
  [0,1,2,3,4,5,6,7] ~=? internos arbolito1,
  "abcdefghi" ~=? hojas arbolito1,
  [True,False,True] ~=? internos arbolito2,
  [1,2,3,2,3,4,3,4,5,4] ~=? take 10 (hojas arbolito3)
  ]

testsEj3 = test [
  [0,1,-1,5] ~=? hojas (incrementarHojas arbolito2)
  ]

testsEj4 = test [
  [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] ~=? hojas (truncar 0 6 arbolito3)
  ]

testsEj5 = test [
  22 ~=? evaluar (truncar 0 6 arbolito3)
  ]

testsEj6 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj7 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj8 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]
  
testsEj9 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]
  
testsEj10 = test [
  Just "alfajor" ~=? busquedaDelTesoro "inicio" ((=='a').head) dicc2,
  Nothing ~=? busquedaDelTesoro "inicio" ((=='w').head) dicc2
  ]