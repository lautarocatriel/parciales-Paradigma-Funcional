{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Library where
import PdePreludat

-- Parcial: DeliveryConF 🍕
-- Disclaimer: En este parcial se evalúan los conceptos de composición, aplicación parcial, orden superior, recursividad, modelado de información, evaluación diferida y sistema de tipos. La ausencia de estos será penalizada.

-- Enunciado General
-- Estamos desarrollando una plataforma de delivery de comidas. De cada comida nos interesa saber su nombre, el tiempo de preparación (en minutos), la cantidad de calorías y una lista de etiquetas que la describen (por ejemplo: “vegana”, “rápida”, “picante”, etc).

-- Las sucursales de esta empresa ofrecen un conjunto de comidas y tienen un historial de modificaciones (actualizaciones) que se han ido realizando sobre ellas (por ejemplo: hacer que todas las comidas bajen en calorías, o que se les agregue una etiqueta nueva a algunas).

-- Punto 1: Buen provecho (2 puntos)
-- Una comida puede ser considerada saludable si tiene menos de 500 calorías y tiene una etiqueta relacionada con “vegana” o “light”.
-- Una sucursal es saludable si todas las comidas que ofrece lo son.

-- Punto 2: ¡Te toca cocinar! (3 puntos)
-- Modelar distintas transformaciones que podrían aplicarse a una comida desde el backend de la plataforma:

-- Reducir las calorías en un porcentaje dado.

-- Marcar una comida como “rápida” si tarda menos de 15 minutos en prepararse, agregando esa etiqueta si no la tiene.

-- Reconvertir una comida para celíacos: eliminar la etiqueta “gluten” si la tiene y agregar la etiqueta “sin TACC”.

-- Una actualización es un proceso que modifica a todas las comidas de una sucursal según un criterio.
-- Modelar cómo una sucursal recibe una actualización, la aplica a todas sus comidas, y la guarda en su historial.

-- Punto 3: Mmm… esto es raro (3 puntos)
-- Responder a las siguientes consultas:

-- ¿Cuál es la comida más calórica de una sucursal?

-- ¿Hay alguna comida que tenga la etiqueta “picante” y también sea considerada saludable?

-- ¿Existen comidas con etiquetas contradictorias como “vegana” y “carne”?

-- Resolver todas utilizando composición y orden superior, sin funciones auxiliares.

-- Punto 4: Chef temporal (2 puntos)
-- Queremos saber si el historial de actualizaciones de una sucursal está ordenado de peor a mejor, entendiendo que una actualización es mejor que otra si reduce el promedio de calorías del menú que deja luego de aplicarse.
-- Resolver este punto utilizando recursividad.

-- Punto 5: ¡Modo turbo activado! (2 puntos)
-- La empresa está evaluando agregar actualizaciones infinitas que optimicen constantemente el menú.
-- ¿Qué sucede si a una sucursal se le aplican infinitas actualizaciones y se quiere calcular la comida más calórica del resultado?
-- ¿Qué pasaría si se evalúa el punto 4 sobre una lista infinita de actualizaciones?
-- Justificar ambos casos en relación a conceptos vistos en la cursada.


data Comida = Comida{
    tiempo :: Number,
    calorias :: Number,
    etiquetas ::[String]
}deriving (Eq,Ord)

data Sucursal = Sucural{
    comidas :: [Comida],
    actualizaciones :: [Comida -> Comida]
}deriving(Eq, Ord)

comidaSaludable :: Comida -> Bool
comidaSaludable comida = ((500>) . calorias) comida && ((elem "vegana"  . etiquetas) comida || (elem "light" . etiquetas) comida)

sucursalSaludable :: Sucursal -> Bool
sucursalSaludable = all comidaSaludable . comidas

type Transformacion = Comida -> Comida

reducirCalorias :: Number -> Transformacion
reducirCalorias porcentaje comida = comida{
    calorias = calorias comida - calorias comida * porcentaje / 100
}

comidaRapida :: Transformacion
comidaRapida comida
 | ((15>) . tiempo) comida = comida{
    etiquetas = (agregarEtiquetas "rapida" . etiquetas) comida
 }
 |otherwise = comida


agregarEtiquetas :: String -> [String] -> [String]
agregarEtiquetas elemento lista
 | elem elemento lista = lista
 |otherwise = elemento : lista

paraCeliacos :: Transformacion
paraCeliacos comida
 |(elem "gluten" . etiquetas) comida = comida{
     etiquetas =(eliminarEtiqueta "gluten" . agregarEtiquetas "sin TACC" . etiquetas) comida
     }
 |otherwise = comida{
    etiquetas = (agregarEtiquetas "sin TACC" . etiquetas) comida
 }

eliminarEtiqueta :: String -> [String] -> [String]
eliminarEtiqueta etiqueta  = filter (/= etiqueta)

masCalorica :: Sucursal -> Comida
masCalorica  = maximum . comidas

picanteYsaludable :: Sucursal -> Bool
picanteYsaludable = any comidaSaludable . filter (elem "picante" . etiquetas) . comidas

etiquetasContradictorias :: String-> String -> Sucursal -> Bool
etiquetasContradictorias etiqueta1 etiqueta2 = any (elem etiqueta1 . etiquetas) . filter (elem etiqueta2 . etiquetas) . comidas

historialOrdenado :: Sucursal -> Bool
historialOrdenado sucursal= (estaOrdenado (actualizaciones sucursal) . comidas)sucursal

estaOrdenado :: [(Comida -> Comida)] -> [Comida] -> Bool
estaOrdenado [] _ = True
estaOrdenado (actualizacion1 : actualizacion2 : actualizaciones) listaComida = sumOf calorias (aplicarActualizacion actualizacion1 listaComida) > sumOf calorias (aplicarActualizacion actualizacion2 listaComida) && estaOrdenado (actualizacion2 : actualizaciones) listaComida

aplicarActualizacion :: (Comida -> Comida) -> [Comida] -> [Comida]
aplicarActualizacion _ [] = []
aplicarActualizacion actualizacion (comida : comidas) = actualizacion comida : aplicarActualizacion actualizacion comidas

--5) el algoritmo diverge ya que nunca va a poder dejar de comparar las calorias de cada comida porque esta buscando el maximo
--5)b) el algoritmo podria coverger en caso de que encuentre un caso en el que no este ordenado, ya que al ser lazy evaluation, al encontrar un false dejara de evaluar