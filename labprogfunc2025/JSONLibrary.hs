{- Grupo: 64
   Integrante(s):
     Pacheco, Juan Manuel, XXXXXXXX
     Duarte, Santiago, 49937830
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
lookupField (JObject o) k = lookupFieldObj o k
lookupField _ _ = Nothing

-- Análoga a la anterior, pero el primer argumento es un objeto.
lookupFieldObj :: Object JSON -> Key -> Maybe JSON
lookupFieldObj o k = lookupFieldObjAux o k Nothing 

lookupFieldObjAux :: Object JSON -> Key -> Maybe JSON -> Maybe JSON
lookupFieldObjAux [] _ m  = m
lookupFieldObjAux (x:xs) k m
      | k == fst x = lookupFieldObjAux xs k (Just (snd x))
      | otherwise = lookupFieldObjAux xs k m

-- retorna la lista de claves de un objeto, manteniendo el orden en el
-- que se encontraban.
keysOf :: Object JSON -> [Key]
keysOf = keysOfObject 

keysOfObject :: Object a -> [Key]
keysOfObject [] = []
keysOfObject (a:as) = (fst a):(keysOfObject as)

-- Retorna una lista con los valores contenidos en los campos de un objeto,
-- manteniendo el orden en el que se encontraban.
valuesOf :: Object JSON -> [JSON]
valuesOf [] = []
valuesOf (j:js) = (snd j):(valuesOf js)

-- retorna todos los campos de un objeto, en el orden en que se encontraban.
entriesOf :: Object JSON -> [(Key,JSON)]
entriesOf o = o

-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del primer objeto.
leftJoin :: Object a -> Object a -> Object a
leftJoin l r = l ++ raux
      where
            kl = keysOfObject l
            raux = [ j | j <- r, not (elem (fst j) kl)]

-- Se combinan dos objetos, en orden.  En caso que haya claves
-- repetidas en ambos objetos, en la unión tienen prioridad los
-- campos del segundo objeto.
rightJoin :: Object a -> Object a -> Object a
rightJoin l r = laux ++ r
      where
            kr = keysOfObject r
            laux = [ j | j <- l, not (elem (fst j) kr)]

-- Dado un predicado sobre objetos JSON, y un arreglo, construye el
-- arreglo con los elementos que satisfacen el predicado.
filterArray :: (JSON -> Bool) -> Array -> Array
filterArray p [] = []
filterArray p (a:as)
      | p a = a:(filterArray p as)
      | otherwise = filterArray p as

-- Se inserta un campo en un objeto. Si las claves del objeto están
-- ordenadas lexicográficamente, el resultado debe conservar esta
-- propiedad.
insertKV :: (Key, v) -> Object v -> Object v
insertKV (k, v) [] = [(k, v)]
insertKV (k, v) (o:os)
      | k < fst o = (k, v):(o:os)
      | otherwise = o:insertKV (k, v) os

-- Se inserta un campo en un objeto, al inicio
consKV :: (Key, v) -> Object v -> Object v
consKV c o = (c:o)

-- ordena claves de un objeto
sortKeys :: Object a -> Object a
sortKeys [] = []
sortKeys (o:os) = (sortKeys (menores o)) ++ [o] ++ (sortKeys (mayores o))
      where
            menores o = [ x | x <- os, (fst x) < (fst o) ]
            mayores o = [ x | x <- os, (fst x) >= (fst o) ]


-- constructoras
mkJString :: String -> JSON
mkJString s = (JString s)

mkJNumber :: Integer -> JSON
mkJNumber i = (JNumber i)

mkJBoolean :: Bool -> JSON
mkJBoolean b = (JBoolean b)

mkJNull :: () -> JSON
mkJNull () = JNull

mkJArray :: [JSON] -> JSON
mkJArray a = (JArray a)

mkJObject :: [(Key, JSON)] -> JSON
mkJObject o = (JObject o)


-- destructoras
fromJString :: JSON -> Maybe String
fromJString (JString s) = (Just s)
fromJString _ = Nothing

fromJNumber :: JSON -> Maybe Integer
fromJNumber (JNumber i) =  (Just i)
fromJNumber _ = Nothing

fromJBoolean :: JSON -> Maybe Bool
fromJBoolean (JBoolean b) = (Just b)
fromJBoolean _ = Nothing

fromJObject :: JSON -> Maybe (Object JSON)
fromJObject (JObject o) = (Just o) 
fromJObject _ = Nothing

fromJArray :: JSON -> Maybe [JSON]
fromJArray (JArray a) = (Just a)
fromJArray _ = Nothing


-- predicados
isJNumber :: JSON -> Bool
isJNumber (JNumber n) = True
isJNumber _ = False

isJNull :: JSON -> Bool
isJNull JNull = True
isJNull _ = False

isJString :: JSON -> Bool
isJString (JString s) = True
isJString _ = False

isJObject :: JSON -> Bool
isJObject (JObject o) = True
isJObject _ = False

isJArray :: JSON -> Bool
isJArray (JArray a) = True
isJArray _ = False

isJBoolean :: JSON -> Bool
isJBoolean (JBoolean b) = True
isJBoolean _ = False

