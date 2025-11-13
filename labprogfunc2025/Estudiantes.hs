{- Grupo: 64
   Integrante(s):
     Pacheco, Juan Manuel, XXXXXXXX
     Duarte, Santiago, 49937830
-}

module Estudiantes where

import JSONLibrary
import TypedJSON



---------------------------------------------------------------------------------------
-- Importante:
-- Notar que NO se puede importar el módulo AST, que es interno a la biblioteca.
---------------------------------------------------------------------------------------

-- valores que representan los "tipos" JSON de un curso y de un estudiante
-- Se usan como valores de tipo `JSONType` (no son nuevas definiciones de
-- tipos de Haskell). Usar `data` aquí es incorrecto porque `data` crea
-- nuevos constructores de tipos; lo que queremos es un valor ya construido
-- usando los constructores definidos en `TypedJSON`.

tyCurso :: JSONType
tyCurso = TyObject
   [ ("anio", TyNum)
   , ("codigo", TyNum)
   , ("nombre", TyString)
   , ("nota", TyNum)
   , ("semestre", TyNum)
   ]

tyEstudiante :: JSONType
tyEstudiante = TyObject
   [ ("CI", TyNum)
   , ("apellido", TyString)
   , ("cursos", TyArray tyCurso)
   , ("nombre", TyString)
   ]


estudiante1 :: JSON
estudiante1 = mkJObject
  [ ("CI", mkJNumber 12345678)
  , ("nombre", mkJString "Juan")
  , ("apellido", mkJString "Pacheco")
  , ("cursos", mkJArray
      [ mkJObject
          [ ("anio", mkJNumber 2024)
          , ("codigo", mkJNumber 101)
          , ("nombre", mkJString "Paradigmas de Programación")
          , ("nota", mkJNumber 10)
          , ("semestre", mkJNumber 1)
          ],
        mkJObject
          [ ("anio", mkJNumber 2022)
          , ("codigo", mkJNumber 101)
          , ("nombre", mkJString "Programacion 2")
          , ("nota", mkJNumber 3)
          , ("semestre", mkJNumber 1)
          ]
      ])
  ]


-- decide si un valor que representa un estudiante esta bien formado
estaBienFormadoEstudiante :: JSON -> Bool  
estaBienFormadoEstudiante a = undefined


-- getters
getCI :: JSON -> Maybe Integer
-- falta verificar que esta bien formado
getCI obj = case lookupField obj "CI" of
  Nothing -> Nothing
  Just i -> fromJNumber i

getNombre :: JSON -> Maybe String
getNombre obj = case lookupField obj "nombre" of
  Nothing -> Nothing
  Just i -> fromJString i

getApellido :: JSON -> Maybe String
getApellido obj = case lookupField obj "apellido" of
  Nothing -> Nothing
  Just i -> fromJString i

getCursos :: JSON -> Maybe JSON
getCursos obj = lookupField obj "cursos"

getNota :: JSON -> Maybe Integer
getNota obj =
  case lookupField obj "nota" of
    Nothing -> Nothing
    Just i -> fromJNumber i

getAnio :: JSON -> Maybe Integer
getAnio obj =
  case lookupField obj "anio" of
    Nothing -> Nothing
    Just i -> fromJNumber i

-- obtiene arreglo con cursos que fueron aprobados
aprobados :: JSON -> Maybe JSON
aprobados obj =
  case getCursos obj of
    Nothing -> Nothing
    Just c ->
      case fromJArray c of
        Nothing -> Nothing
        Just arr ->
          let aprobados = [ curso | curso <- arr, Just n <- [getNota curso], n >= 3 ]
          in Just (mkJArray aprobados)

-- obtiene arreglo con cursos rendidos en un año dado
enAnio :: Integer -> JSON -> Maybe JSON
enAnio anio obj =
  case getCursos obj of
    Nothing -> Nothing
    Just c ->
      case fromJArray c of
        Nothing -> Nothing
        Just arr ->
          let cursosAnio = [ curso | curso <- arr, Just a <- [getAnio curso], a == anio ]
          in Just (mkJArray cursosAnio)

-- retorna el promedio de las notas de los cursos
promedioEscolaridad :: JSON -> Maybe Float
promedioEscolaridad obj =
  case getCursos obj of
    Nothing -> Nothing
    Just c ->
      case fromJArray c of
        Nothing -> Nothing
        Just arr ->
          let cant = length arr
              total = sum [n | Just n <- [getNota curso | curso <- arr]]
          in Just (fromIntegral total / fromIntegral cant)


-- agrega curso a lista de cursos de un estudiante
addCurso :: Object JSON -> JSON -> JSON
addCurso = undefined
