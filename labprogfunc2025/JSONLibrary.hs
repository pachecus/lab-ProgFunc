{- Grupo: X
   Integrante(s):
     Apellido, Nombre, XXXXXXXX
     Apellido, Nombre, XXXXXXXX
-}

module JSONLibrary
 (lookupField,
  lookupFieldObj,
  keysOf,
  valuesOf,
  entriesOf,
  leftJoin,
  rightJoin,
  filterArray,
  insertKV,
  sortKeys,
  mkJString, mkJNumber, mkJBoolean, mkJNull, mkJObject, mkJArray,
  fromJString, fromJNumber, fromJBoolean, fromJObject, fromJArray,
  isJString, isJNumber, isJBoolean, isJNull, isJObject, isJArray,
  JSON(),importJSON,
  Object()
 )
where

import AST


{- lookupField:
 Cuando el primer argumento es un objeto y tiene como clave el valor
 dado como segundo argumento, entonces se retorna el valor JSON
 correspondiente (bajo el constructor {\tt Just}). De lo contrario se
 retorna {\tt Nothing}. Si un objeto tiene claves repetidas, se
 retorna el valor de más a la derecha.
-}
lookupField :: JSON -> Key -> Maybe JSON
lookupField = undefined

-- Análoga a la anterior, pero el primer argumento es un objeto.
lookupFieldObj :: Object JSON -> Key -> Maybe JSON
lookupFieldObj = undefined

-- retorna la lista de claves de un objeto, manteniendo el orden en el
-- que se encontraban.
keysOf :: Object JSON -> [Key]
keysOf = undefined

-- Retorna una lista con los valores contenidos en los campos de un objeto,
-- manteniendo el orden en el que se encontraban.
valuesOf :: Object JSON -> [JSON]
valuesOf = undefined

-- retorna todos los campos de un objeto, en el orden en que se encontraban.
entriesOf :: Object JSON -> [(Key,JSON)]
entriesOf = undefined

-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del primer objeto.
leftJoin :: Object a -> Object a -> Object a
leftJoin = undefined

-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del segundo objeto.
rightJoin :: Object a -> Object a -> Object a
rightJoin = undefined

-- Dado un predicado sobre objetos JSON, y un arreglo, construye el
-- arreglo con los elementos que satisfacen el predicado.
filterArray :: (JSON -> Bool) ->  Array -> Array
filterArray = undefined

-- Se inserta un campo en un objeto. Si las claves del objeto están
-- ordenadas lexicográficamente, el resultado debe conservar esta
-- propiedad.
insertKV :: (Key, v) -> Object v -> Object v
insertKV = undefined

-- Se inserta un campo en un objeto, al inicio
consKV :: (Key, v) -> Object v -> Object v
consKV = undefined

-- ordena claves de un objeto
sortKeys :: Object a -> Object a
sortKeys = undefined


-- constructoras
mkJString :: String -> JSON
mkJString = undefined

mkJNumber :: Integer -> JSON
mkJNumber = undefined

mkJBoolean :: Bool -> JSON
mkJBoolean = undefined

mkJNull :: () -> JSON
mkJNull = undefined

mkJArray :: [JSON] -> JSON
mkJArray = undefined

mkJObject :: [(Key, JSON)] -> JSON
mkJObject = undefined


-- destructoras
fromJString :: JSON -> Maybe String
fromJString = undefined

fromJNumber :: JSON -> Maybe Integer
fromJNumber = undefined

fromJBoolean  :: JSON -> Maybe Bool
fromJBoolean = undefined

fromJObject :: JSON -> Maybe (Object JSON)
fromJObject = undefined

fromJArray :: JSON -> Maybe [JSON]
fromJArray = undefined


-- predicados
isJNumber :: JSON -> Bool
isJNumber = undefined

isJNull :: JSON -> Bool
isJNull = undefined

isJString :: JSON -> Bool
isJString  = undefined

isJObject :: JSON -> Bool
isJObject  = undefined

isJArray :: JSON -> Bool
isJArray   = undefined

isJBoolean :: JSON -> Bool
isJBoolean = undefined

