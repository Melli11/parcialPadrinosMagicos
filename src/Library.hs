module Library where
import PdePreludat

-- data Deseos = aprenderHabilidades Habilidades Chico | serGrosoEnNeedForSpeed Chico | serMayor Chico

type Habilidades = String
type Deseos = Chico -> Chico
data Chico = Chico {
    nombre :: String,
    edad :: Number,
    habilidad :: [Habilidades]
    } deriving (Show,Eq)

-- chico = Chico {
--     nombre = "bobi",
--     edad = 10 ,
--     habilidad = [],
--     deseos = []
--     }
-- chicoConHabilidad  = Chico {
--     nombre = "bobiHabilidoso",
--     edad = 10 ,
--     habilidad = ["Prog Haskell Super Sr"],
--     deseos = []
--     }

-- chicoMocoso = Chico {
--     nombre = "bobiConMocos",
--     edad = 10 ,
--     habilidad = ["Sacarse los mocos"],
--     deseos = []
--     }

aprenderHabilidades :: [Habilidades] -> Chico -> Chico
-- aprenderHabilidades nuevasHabilidades unChico = unChico {habilidad = nuevasHabilidades ++ habilidad unChico} 
aprenderHabilidades nuevasHabilidades unChico = unChico {habilidad = nuevasHabilidades ++ habilidad unChico} 

-- serGrosoEnNeedForSpeed unChico = aprenderHabilidades 

serMayor unChico = unChico {edad = 18} 

--timmy = Chico “Timmy” 10 [“mirar television”, “jugar en la pc”] [serMayor]