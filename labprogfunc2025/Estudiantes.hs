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
estaBienFormadoEstudiante = undefined


-- getters
getCI :: JSON -> Maybe Integer
getCI o = case lookupField o "CI" of
    Nothing -> Nothing
    Just i -> fromJNumber i

getNombre :: JSON -> Maybe String
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
    Nothing -> Nothing
    Just c ->
      case fromJArray c of
        Nothing -> Nothing
        Just a ->
          let getNota (Just n) = n
              filtrado = filterArray (\o -> getNota (lookupField o "nota") >= 3) a
          in Just (mkJArray filtrado))


-- obtiene arreglo con cursos rendidos en un año dado
enAnio :: Integer -> JSON -> Maybe JSON
enAnio = undefined

-- retorna el promedio de las notas de los cursos
promedioEscolaridad :: JSON -> Maybe Float
promedioEscolaridad = undefined 

-- agrega curso a lista de cursos de un estudiante
addCurso :: Object JSON -> JSON -> JSON
addCurso = undefined
