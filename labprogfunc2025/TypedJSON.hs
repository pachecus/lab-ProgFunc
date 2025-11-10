{- Grupo: X
   Integrante(s):
     Apellido, Nombre, XXXXXXXX
     Apellido, Nombre, XXXXXXXX
-}

module TypedJSON where

import AST
import JSONLibrary
import Control.Monad
import Data.List


-- Tipos JSON
data JSONType
  = TyString
  | TyNum
  | TyObject (Object JSONType)
  | TyArray JSONType
  | TyBool
  | TyNull
  deriving (Show, Eq)

-- funcion que verifica si hay elementos repetidos en una lista 
-- se usa para verificar que las claves no esten duplicadas
tieneRepetidas::[String] -> Bool
tieneRepetidas [] = False
tieneRepetidas (x:xs)
  | x `elem` xs = True -- verificar si x esta en xs
  | otherwise = tieneRepetidas xs


-- ordenar usando merge sort
ordenar:: [(String, a)] -> [(String, a)]
ordenar [] = []
ordenar [x] = [x]
-- ordenar [(k,v)] = [(k,v)]
ordenar xs =
    let (izq, der) = partes xs -- dividir partes para el merge sort
    in merge (ordenar izq) (ordenar der) -- ordenar ambas partes y mergear

-- dividir un arreglo a la mitad
partes:: [a] -> ([a], [a])
partes [] = ([], [])
partes (x:xs) =
    let (a, b) = partes xs
    in (x:b, a)

-- merge ordenado de dos listas, la idea es ir pasando elementos de una lista a la otra hasta que una este vacia
merge:: [(String, a)] -> [(String, a)] -> [(String, a)]
merge [] ys = ys -- si priemra lista es vacia se retorna la segunda
merge xs [] = xs -- si segunda lista es vacia se retorna la primera
merge ((k1, v1):xs) ((k2, v2):ys) 
  | k1 <= k2 = (k1,v1) : merge xs ((k2,v2):ys) -- clave del primer elemento menor que la del segundo
  | otherwise = (k2, v2) : merge ((k1, v1):xs) ys

procesarCampos :: [(String, JSON)] -> Maybe [(String, JSONType)]
procesarCampos [] = Just []
procesarCampos ((k, v):xs) =
    case typeOf v of
        Nothing -> Nothing -- si v tiene tipo Nothing retornar Nothing
        Just t ->
            case procesarCampos xs of 
                Nothing -> Nothing -- si xs es Nothing retornar Nothing
                Just resto -> Just ((k, t) : resto) -- sino colocar el tipo (t) de v


-- dado un valor JSON se infiere el tipo. Se devuelve
-- Nothing si el valor está mal tipado
typeOf :: JSON -> Maybe JSONType
typeOf (JString _) = Just TyString
typeOf (JNumber _) = Just TyNum
typeOf (JBoolean _) = Just TyBool
typeOf (JNull) = Just TyNull
typeOf (JArray []) = Nothing --chequear si la lista es vacía
typeOf (JArray (x:xs)) =
    let tipos = map typeOf (x:xs) -- lista de tipos de todos los elementos de la lista
    in if any (== Nothing) tipos -- si algun tipo es invalido devolver Nothing
      then Nothing
      else
          let tiposSJ = [t | Just t <- tipos] --solo el t, sin el Just
              primero = tiposSJ !! 0 -- primer tipo que aparece, uso esto en vez de head tiposSJ para quitar el warning de si tiposSJ es vacio
          in if all (== primero) tiposSJ -- si todos los tipos son iguales al primero
              then Just (TyArray primero) -- devolver arreglo del tipo del primer elemento
              else Nothing

typeOf (JObject elems)
    | null elems = Nothing -- no se permiten objetos vacios
    | tieneRepetidas (map fst elems) = Nothing -- si hay claves repetidas
    | otherwise =
      case procesarCampos elems of
            Nothing -> Nothing
            Just camposTipados ->
                let ordenado = ordenar camposTipados
                in Just (TyObject ordenado)


-- decide si las claves de un objeto están ordenadas
-- lexicográficamente y no se repiten.
objectWf :: Object JSONType -> Bool
objectWf [] = False
objectWf [(_,_)] = True -- un solo campo siempre esta ordenado
objectWf ((k1,_):(k2,v2):xs)
  | k1 == k2 = False -- clave repetida
  | k1 > k2 = False -- desorden
  | otherwise = objectWf ((k2,v2):xs)


-- decide si todos los tipos objeto contenidos en un tipo JSON
-- están bien formados.
typeWf :: JSONType -> Bool
typeWf TyString = True
typeWf TyNum = True
typeWf TyBool = True
typeWf TyNull = True
typeWf (TyArray t) = typeWf t -- chequear que el tipo del arreglo sea válido
typeWf (TyObject obj) =
  objectWf obj -- si todas las claves estan correctas
  && all (\(_,t) -> typeWf t) obj -- si todos los elementos dentro del objeto tienen tipo adecuado


-- dado un valor JSON v, y un tipo t, decide si v tiene tipo t.
hasType :: JSON -> JSONType -> Bool
hasType v t =
  case typeOf v of
    Nothing -> False -- si el tipo de v es NOthing entonces no es un tipo valido
    Just tipo -> tipo == t && typeWf t -- si typeOf v retorna Just tipo entonces verifico que el tipo == t y que ademas el t este bien formado
