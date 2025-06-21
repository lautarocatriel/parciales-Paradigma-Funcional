module Library where
import PdePreludat

data Pais = Pais{
    ingresoPerCapita :: Number,
    deudaConFMI :: Number,
    poblacionActivaSectorPublico :: Number,
    poblacionActivaSectorPrivado :: Number,
    recursosNaturales :: [String]
}deriving(Eq, Ord)

nambia :: Pais
nambia = Pais 4140 50000000 400000 650000 ["mineria", "ecoturismo"]

type Estrategia = Pais -> Pais

prestamo :: Number -> Estrategia
prestamo plata pais = pais{
    deudaConFMI = (((plata * 1.5) +) . deudaConFMI) pais
}

reduccionSectorPublico :: Number -> Estrategia
reduccionSectorPublico cantidad pais = pais {
    poblacionActivaSectorPublico = poblacionActivaSectorPublico pais - cantidad,
    ingresoPerCapita = disminuirIngreso cantidad (ingresoPerCapita pais)
}

disminuirIngreso :: Number -> Number -> Number
disminuirIngreso cantidad ingreso
 |cantidad > 100 = ((ingreso -) . (*0.2)) ingreso
 |otherwise = ((ingreso -) . (*0.15)) ingreso


explotacionRecurso :: String -> Estrategia
explotacionRecurso recurso pais = pais{
    deudaConFMI = deudaConFMI pais - 2000000,
    recursosNaturales = filter (/=recurso) (recursosNaturales pais)
}

blindaje :: Estrategia
blindaje pais = (prestamo (pBI pais *0.5) . reduccionSectorPublico 500) pais 

pBI :: Pais -> Number
pBI pais = ingresoPerCapita pais * (poblacionActivaSectorPrivado pais + poblacionActivaSectorPublico pais)

receta :: Estrategia
receta = prestamo 200000 . explotacionRecurso "mineria"

tienenPetroleo :: [Pais] -> [Pais]
tienenPetroleo = filter (elem "petroleo" . recursosNaturales)

fMIAFavor :: [Pais] -> Number
-- fMIAFavor  = sumOf deudaConFMI 
fMIAFavor = sum . map deudaConFMI

ordenRecetas :: Pais -> [Estrategia] -> Bool
ordenRecetas _ [] = True
ordenRecetas pais listaRecetas = compararPBI pais listaRecetas

compararPBI :: Pais ->[Estrategia] -> Bool
compararPBI pais (receta1:receta2:recetas)
 |(pBI . receta1) pais < (pBI . receta2) pais = compararPBI pais (receta2 : recetas)
 |otherwise = False


--6)a)el algoritmo converge ya que al momento de encontrar "petroleo" en la lista infinita deja de evaluar debido a que es lazy evaluation
--6)b)el algoritmo diverge ya que nunca podra terminar de efectuar la suma de la lista infinita