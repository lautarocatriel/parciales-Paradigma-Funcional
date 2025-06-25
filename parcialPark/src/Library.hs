{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Fuse foldr/map" #-}
{-# HLINT ignore "Hoist not" #-}
module Library where
import PdePreludat


-- Disclaimer: En este parcial se evaluarán los conceptos de aplicación parcial, composición, reutilización, recursividad y orden superior para el modelado del mismo. En caso de no estar presentes estos conceptos se descontará puntaje. 

-- Modelamos una aplicación de parques de diversiones donde registramos sus atracciones, de las cuales conocemos su nombre, altura mínima requerida para la persona que ingrese medida en centímetros, duración en minutos, una serie de opiniones que le da la gente (“entretenida”, “veloz”, ”un embole”, etc) y si está en mantenimiento o no lo cual permite el acceso por parte de las personas. Un operador determina si el entretenimiento requiere atención y lo pasa a su estado de mantenimiento. En algún momento pasan los técnicos y asignan las reparaciones, que tiene una duración determinada en días y el trabajo que se realiza.
-- Punto 1: Más bueno que … (2 puntos)
-- Queremos saber qué tan buena es una atracción. Para eso utilizamos un sistema de scoring que tiene un modo muy particular para calcularlo: Si la atracción tiene una duración prolongada (es decir que dura más de 10 minutos) valen 100 puntos. Si no es así, pero tiene menos de 3 órdenes de reparaciones el puntaje es 10 puntos por cada letra del nombre más 2 puntos por cada opinión que tiene. Caso contrario es 10 veces la altura mínima requerida.
-- Punto 2: Iguana fissss (4 puntos)
-- Los técnicos tienen diversos tipos de trabajo que pueden desarrollar en cada reparación sobre las atracciones. Algo muy importante a tener en cuenta es que luego de que realizan cualquier trabajo siempre ocurren dos cosas: se elimina la última reparación de la lista (no importa cual fuere) y se verifica que no tenga reparaciones pendientes. Si quedan pendientes debe mantener el indicador que está en mantenimiento, caso contrario no. Los posibles trabajos son :
-- ajusteDeTornillería que como le refuerza aún más estructura a la atracción, prolonga su duración en 1 minuto por cada tornillo apretado pero no pudiendo superar los 10 minutos porque no es rentable. Es decir que si una atracción dura 3 minutos y ajusta 4 tornillos la misma pasa a durar 7 minutos. Pero sí una atracción dura 8 minutos y el técnico logra apretar 5 tornillos pasa a durar solamente 10 minutos.
-- engrase que vuelve más veloz al entretenimiento, por lo tanto aumenta en 0,1 centímetros la altura mínima requerida por cada gramo de grasa utilizada en el proceso y le agrega la opinión “para valientes”. La cantidad de grasa requerida puede variar según el criterio del técnico.
-- mantenimientoElectrico repara todas las bombitas de luz y su cableado. Como es un lavado de cara y una novedad para la gente, solo se queda con las dos primeras opiniones y el resto las descarta.
-- mantenimientoBásico que consiste en ajustar la tornillería de 8 tornillos y hacer un engrase con 10 gramos de grasa. 
-- Punto 3: ¿Qué oooooonda este parque? (3 puntos)
-- Esa me da miedito
-- Queremos saber si una atracción meDaMiedito, esto implica que alguna de las inspecciones que se le hicieron le asignó más de 4 días de mantenimiento.
-- Acá cerramos… 
-- Cerramos una atracción si la sumatoria de tiempo de las reparaciones pendientes para dicha atracción es de 7 días. 

-- Disney no esistis
-- Tenemos que determinar disneyNoEsistis para un parque. Esto ocurre cuando todas las atracciones de nombre cheto (con más de 5 letras) no tienen reparaciones pendientes.
-- En este punto no puede utilizar funciones auxiliares ni recursividad, solo composición y aplicación parcial.

-- Punto 4: Reparaciones peolas (2 puntos)
-- Una atracción tiene reparaciones peolas si luego de cada una está más buena, esto implica que luego de hacer el trabajo de cada reparación el puntaje mejora con respecto a la reparación previa. 
-- En este punto debe resolver exclusivamente con recursividad

-- Punto 5: Manny a la obra (2 puntos)
-- Queremos modelar un proceso que realice los trabajos de las reparaciones pendientes sobre una atracción. Se pide que además muestre un ejemplo de cómo podría evaluar por consola el proceso para cada una de las actividades resueltas en el punto anterior.
-- En este punto NO puede utilizar recursividad

-- Punto 6: Estoy cansado jefe… (1 puntos)
-- Si una atracción tiene una cantidad infinita de trabajos, ¿sería posible obtener un valor computable para la función del punto anterior? ¿Qué ocurriría con una lista de trabajos infinita en el punto 4? Justifique sus respuestas relacionándolo con un concepto visto en la materia.



data Atraccion = Atraccion{
    nombre :: String,
    alturaMin :: Number,
    duracion :: Number,
    opiniones :: [String],
    estaEnMantenimiento :: Bool,
    reparaciones :: [Reparacion]
}deriving(Eq, Ord, Show)
data Reparacion = Reparacion{
    dias :: Number,
    trabajo :: Atraccion -> Atraccion
} deriving( Eq, Ord, Show)
type Trabajo = Atraccion -> Atraccion

requiereAtencion :: Atraccion -> Reparacion -> Atraccion
requiereAtencion atraccion reparacion = atraccion {
    estaEnMantenimiento = True,
    reparaciones = reparacion : reparaciones atraccion
}

queTanBuenaAtraccion :: Atraccion -> Number
queTanBuenaAtraccion atraccion
 |((>10) . duracion) atraccion = 100
 |((3>) . length . reparaciones) atraccion = ((*10) . length . nombre) atraccion + ((*2) . length . opiniones) atraccion
 |otherwise = ((*10) . alturaMin) atraccion

-- eliminarReparacion :: [Reparacion] -> [Reparacion]
-- eliminarReparacion [] = []
-- eliminarReparacion lista = (drop 1 . reverse) lista

eliminarReparacion ::[Reparacion] -> [Reparacion]
eliminarReparacion =  init 
tieneReparacionesPendientes :: [Reparacion] -> Bool
tieneReparacionesPendientes = not . null 

noTieneReparacionesPendientes :: Atraccion -> Bool
noTieneReparacionesPendientes = (== 0) . length . reparaciones

reparar :: Trabajo
reparar atraccion = atraccion{
    reparaciones= (eliminarReparacion . reparaciones) atraccion,
    estaEnMantenimiento = (tieneReparacionesPendientes . eliminarReparacion . reparaciones) atraccion
}

-- ajusteDeTornilleria :: Number -> Atraccion -> Atraccion
-- ajusteDeTornilleria numero atraccion = atraccion {
--     duracion = (min 10 . (+numero) . duracion) atraccion,
--     reparaciones = (eliminarReparacion . reparaciones) atraccion,
--     estaEnMantenimiento = noTieneReparacionesPendientes atraccion
--  }
ajusteDeTornilleria :: Number -> Trabajo
ajusteDeTornilleria tornillos atraccion = reparar atraccion{
    duracion= (min 10 . (tornillos+) . duracion) atraccion
}


-- engrase :: Number -> Atraccion -> Atraccion
-- engrase numero atraccion = atraccion {
--     opiniones = ("para valientes" : opiniones atraccion),
--     alturaMin = (((0.1*numero)+) . alturaMin) atraccion,
--     reparaciones = (eliminarReparacion . reparaciones) atraccion,
--     estaEnMantenimiento = noTieneReparacionesPendientes atraccion
-- }

engrase :: Number -> Trabajo
engrase grasa atraccion = reparar atraccion {
    alturaMin = (((0.1 * grasa)+) . alturaMin) atraccion,
    opiniones = "para valientes" : opiniones atraccion
}



mantenimientoElectrico :: Trabajo
mantenimientoElectrico atraccion = reparar atraccion{
    opiniones = (take 2 . opiniones) atraccion
}



mantenimientoBasico :: Trabajo
mantenimientoBasico = (ajusteDeTornilleria 8) . (engrase 10)

meDaMiedito :: Atraccion -> Bool
meDaMiedito = any ((>4) . dias) . reparaciones

cierraAtraccion :: Atraccion -> Bool
cierraAtraccion = (>7) . sumOf dias . reparaciones
-- meDaMiedito :: Atraccion -> Bool
-- meDaMiedito = ((any (>4)) . (map fst) . reparaciones)

disneyNoEsistis :: [Atraccion] -> Bool
disneyNoEsistis = all (not . tieneReparacionesPendientes . reparaciones) . filter ((>5) . length . nombre)

-- cierreAtraccion :: Atraccion -> Bool
-- cierreAtraccion = ((>= 7). sum . (map fst) . reparaciones)

-- disneyNoEsistis :: [Atraccion] -> Bool
-- disneyNoEsistis = ((all noTieneReparacionesPendientes)  . filter(((>5) . length . nombre )))

-- mejoraScoring :: Atraccion -> Reparacion -> Reparacion -> Bool
-- mejoraScoring atraccion reparacion1 reparacion2 = queTanBuenaAtraccion (reparacion1 atraccion) > queTanBuenaAtraccion . (trabajo . reparacion2) atraccion 


mannyALaObra' :: Trabajo
mannyALaObra' atraccion = (foldr trabajo atraccion . reparaciones) atraccion 
-- reparacionesPeolas :: Atraccion -> [Reparacion]->Bool
-- reparacionesPeolas _ [] = True
-- reparacionesPeolas atraccion (reparacion : reparaciones) = mejoraPuntaje atraccion reparacion && reparacionesPeolas (snd reparacion atraccion) reparaciones

-- mejoraPuntaje :: Atraccion -> Reparacion -> Bool
-- mejoraPuntaje atraccion reparacion = (queTanBuenaAtraccion (snd reparacion atraccion)) > queTanBuenaAtraccion atraccion

-- tieneReparacionesPeolas :: Atraccion -> Bool
-- tieneReparacionesPeolas atraccion = (reparacionesPeolas atraccion . reparaciones) atraccion

-- mannyALaObra :: Atraccion -> Atraccion
-- mannyALaObra  atraccion= foldr ($) atraccion (((map snd) . reparaciones) atraccion)