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
estudianteMalo :: JSON
estudianteMalo = mkJObject
        [ ("CI", mkJString "12345678")  -- debería ser número
        , ("nombre", mkJString "Juan")
        , ("apellido", mkJString "Pacheco")
        , ("cursos", mkJArray [])
        ]

estudianteMalo2 :: JSON
estudianteMalo2 = mkJObject
        [ ("CI", mkJNumber 12345678)
        , ("nombre", mkJString "Juan")
        , ("apellido", mkJString "Pacheco")
        , ("cursos", mkJArray
            [ mkJObject
                [ ("anio", mkJNumber 2024)
                , ("nombre", mkJString "Paradigmas de Programación")
                ] -- faltan campos
            ])
        ]

-- Estudiante al cual le falta la nota
estudianteMalo3 :: JSON
estudianteMalo3 = mkJObject
  [ ("CI", mkJNumber 12345678)
  , ("nombre", mkJString "Juan")
  , ("apellido", mkJString "Pacheco")
  , ("cursos", mkJArray
      [ mkJObject
          [ ("anio", mkJNumber 2024)
          , ("codigo", mkJNumber 101)
          , ("nombre", mkJString "Paradigmas")
          , ("nota", mkJNumber 10)
          , ("semestre", mkJNumber 1)
          ],
        mkJObject
          [ ("anio", mkJNumber 2022)
          , ("codigo", mkJNumber 101)
          , ("nombre", mkJString "Programación 2")
          , ("semestre", mkJNumber 1)  -- ❌ falta "nota"
          ]
      ])
  ]



--tyEstudiante =
--TyObject [ ("nombre", TyString),
--("apellido", TyString),
--("CI", TyNum),
--("cursos", TyArray tyCurso) ]
--tyCurso =
--TyObject [("nombre", TyString),
--("codigo", TyNum),
--("anio", TyNum),
--("semestre", TyNum),
--("nota", TyNum)]

-- decide si un valor que representa un estudiante esta bien formado
estaBienFormadoEstudiante :: JSON -> Bool  
<<<<<<< HEAD
estaBienFormadoEstudiante = undefined
=======
estaBienFormadoEstudiante a = hasType a tyEstudiante
>>>>>>> dbacc9a861415d1b03f0a72f512f61f82911813a


-- getters
getCI :: JSON -> Maybe Integer
<<<<<<< HEAD
getCI o = case lookupField o "CI" of
=======
-- falta verificar que esta bien formado
getCI obj
  | not (estaBienFormadoEstudiante obj) = Nothing
  | otherwise = case lookupField obj "CI" of
>>>>>>> dbacc9a861415d1b03f0a72f512f61f82911813a
    Nothing -> Nothing
    Just i -> fromJNumber i

getNombre :: JSON -> Maybe String
<<<<<<< HEAD
getNombre o = case lookupField o "nombre" of
    Nothing -> Nothing
    Just s -> fromJString s

getApellido :: JSON -> Maybe String
getApellido o = case lookupField o "apellido" of
    Nothing -> Nothing
    Just s -> fromJString s

getCursos :: JSON -> Maybe JSON
getCursos o = lookupField o "cursos"

-- obtiene arreglo con cursos que fueron aprobados
aprobados :: JSON -> Maybe JSON
aprobados e = (
  case getCursos e of
=======
getNombre obj
  | not (estaBienFormadoEstudiante obj) = Nothing
  | otherwise = case lookupField obj "nombre" of
    Nothing -> Nothing
    Just i -> fromJString i

getApellido :: JSON -> Maybe String
getApellido obj
  | not (estaBienFormadoEstudiante obj) = Nothing
  | otherwise = case lookupField obj "apellido" of
    Nothing -> Nothing
    Just i -> fromJString i

getCursos :: JSON -> Maybe JSON
getCursos obj
  | not (estaBienFormadoEstudiante obj) = Nothing
  | otherwise = lookupField obj "cursos"


-- Getters de Curso para implememtar aprobados, enAnio, promedio
getNota :: JSON -> Maybe Integer
getNota obj
  | not (hasType obj tyCurso) = Nothing -- chequear si obj es de tipo curso
  | otherwise = case lookupField obj "nota" of
      Nothing -> Nothing
      Just i -> fromJNumber i

getAnio :: JSON -> Maybe Integer
getAnio obj
  | not (hasType obj tyCurso) = Nothing -- chequear si obj es de tipo curso
  | otherwise = case lookupField obj "anio" of
    Nothing -> Nothing
    Just i -> fromJNumber i

getSemestre :: JSON -> Maybe Integer
getSemestre obj
  | not (hasType obj tyCurso) = Nothing
  | otherwise = case lookupField obj "semestre" of
    Nothing -> Nothing
    Just i -> fromJNumber i

getCodigo :: JSON -> Maybe Integer
getCodigo obj
  | not (hasType obj tyCurso) = Nothing
  | otherwise = case lookupField obj "codigo" of
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
promedioEscolaridad obj
  | not (estaBienFormadoEstudiante obj) = Nothing
  | otherwise = case getCursos obj of
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
