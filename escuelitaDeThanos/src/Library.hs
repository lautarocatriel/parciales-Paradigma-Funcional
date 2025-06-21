{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Library where
import PdePreludat

data Guantelete = Guantelete{
    material :: String,
    gemas :: [Poder]
}deriving(Eq,Ord)

data Personaje = Personaje{
    edad :: Number,
    energia :: Number,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving(Eq,Ord)

data Universo = Universo{
    habitantes :: [Personaje]
}deriving(Eq,Ord)


chasquearUniverso :: Guantelete -> Universo -> Universo
chasquearUniverso guante universo
 |((==6) . length . gemas) guante && ((=="uru") . material) guante = universo{
    habitantes = drop (div ((length . habitantes) universo) 2) (habitantes universo)
 }
 |otherwise = universo




aptoParaPendex :: Universo -> Bool
aptoParaPendex universo = any ((< 45) . edad) (habitantes universo)

energiaUniverso :: Universo -> Number
energiaUniverso universo = sumOf energia ((filter ((>1) . length .  habilidades) . habitantes) universo)

type Poder = Personaje -> Personaje

laMente :: Number -> Poder
laMente debilitador personaje = personaje{
    energia = energia personaje - debilitador
}

elAlma :: String -> Poder
elAlma habilidad personaje = laMente 10 personaje{
    habilidades = filter (/= habilidad) (habilidades personaje)
}

elEspacio :: String -> Poder
elEspacio planeta personaje = laMente 20 personaje{
    planeta = planeta
}



elPoder :: Poder
elPoder personaje
 | ((<=2) . length . habilidades) personaje = (quitarTodaEnergia . quitarHabilidades) personaje
 | otherwise = quitarTodaEnergia personaje
 where quitarTodaEnergia = laMente ((max 0 . energia) personaje)
 
quitarHabilidades :: Poder
quitarHabilidades personaje = personaje{
    habilidades = []
 }


-- elTiempo :: Poder
-- elTiempo personaje
--  | divisionEdad >= 18 = personaje{
--     edad = divisionEdad
--  }
--  |otherwise = personaje{
--     edad =18
--  }
--  where divisionEdad = div (edad personaje) 2

tiempo :: Poder
tiempo personaje = laMente 50 personaje {
  edad = (max 18.div (edad personaje)) 2 
}

laGemaLoca :: Poder -> Poder
laGemaLoca gema = gema . gema

utilizarPoder :: [Poder] -> Poder
utilizarPoder listaPoderes personaje = foldr ($) personaje listaPoderes


gemaMasPoderosa :: Personaje -> Guantelete -> Poder
gemaMasPoderosa personaje guante = gemaQueMasSaca personaje (gemas guante)

gemaQueMasSaca :: Personaje -> [Poder] -> Poder
gemaQueMasSaca _ [poder] = poder
gemaQueMasSaca personaje (poder1:poder2:poderes)
 | (energia . poder1) personaje < (energia . poder2) personaje = gemaQueMasSaca personaje (poder1 : poderes)
 |otherwise = gemaQueMasSaca personaje (poder2:poderes)

 --punto 7) la primera no podria ya que tendira que comparar todos los elementos de la lista de poderes y el algoritmo divergeria
--en cambio la segunda podria ya que al ser lazy evaluation va a evaluar solo lo que utiliza y se quedara solo con las primeras 3


