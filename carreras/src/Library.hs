{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Library where
import PdePreludat

data Auto = Auto {
    color :: String,
    velocidad :: Number,
    distancia :: Number
}deriving(Eq, Ord)

data Carrera = Carrera{
    autos :: [Auto]
}

estaCerca :: Auto -> Auto -> Bool
estaCerca auto1 auto2 =  auto1 /= auto2 && distanciaAuto auto1 auto2 < 10

distanciaAuto :: Auto -> Auto -> Number
distanciaAuto auto1 = abs . (distancia auto1 -) . distancia

vaTranquilo :: Auto -> Carrera -> Bool
vaTranquilo  auto carrera= not (any (estaCerca auto) (autos carrera)) && all (estaAdelante auto) (autos carrera)

estaAdelante :: Auto -> Auto -> Bool
estaAdelante auto1 = (distancia auto1 >=) . distancia

puesto :: Auto -> Carrera -> Number
puesto auto = (1+) . length . filter(not . estaAdelante auto) . autos

corre :: Number -> Auto -> Auto
corre tiempo auto = auto{
    distancia  = velocidad auto * tiempo + distancia auto
    }

type Modificador = Number -> Number
alterarVelocidad :: Auto ->Modificador 
alterarVelocidad auto numero = velocidad auto - numero

bajarVelocidad :: Auto-> Number -> Auto
bajarVelocidad   auto numero = auto{
    velocidad = max 0 (alterarVelocidad auto numero)
}