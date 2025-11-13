{- Grupo: 64
   Integrante(s):
     Pacheco, Juan Manuel, XXXXXXXX
     Duarte, Santiago, 49937830
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
    | tieneRepetidas (keysOf elems) = Nothing
    | otherwise =
      case procesarCampos elems of
            Nothing -> Nothing
            Just camposTipados ->
                let ordenado = sortKeys camposTipados
                in Just (TyObject ordenado)


-- decide si las claves de un objeto están ordenadas
-- lexicográficamente y no se repiten.
objectWf :: Object JSONType -> Bool
objectWf obj =
  let keys = map fst obj -- keys son las claves del type del objeto
  in not (null obj) -- el objeto no es null
     && keys == sort keys -- las claves estan ordenadas
     && not (tieneRepetidas keys) -- no hay claves repetidas




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
