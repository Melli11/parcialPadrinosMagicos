module Library where
import PdePreludat


type Habilidades = String

type Deseo = Chico -> Chico

data Chico = Chico {
    nombre :: String,
    edad :: Number,
    habilidad :: [Habilidades],
    deseo :: [Deseo]
    } deriving (Show,Eq)

chico :: Chico
chico = Chico "bobi" 10 [] [serMayor]
    
chicoConHabilidad :: Chico
chicoConHabilidad  = Chico "bobiHabilidoso" 10 ["Prog Haskell Super Sr"] []

chicoPeligroso :: Chico
chicoPeligroso = Chico "bobiPeligroso" 10 ["Conducir"] [serMayor]

timmy :: Chico
timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]

taxiDriver :: Chico
taxiDriver  = Chico "bobiPeligroso" 19 ["Sabe manejar"] []

chicoSinHabilidadCon2Deseos :: Chico
chicoSinHabilidadCon2Deseos = Chico "bobi" 10 [] [aprenderHabilidades ["Andar en bicicleta"],serMayor]
-- 1. **Concediendo deseos**
aprenderHabilidades :: [Habilidades] -> Chico -> Chico
aprenderHabilidades nuevasHabilidades unChico = unChico {habilidad = nuevasHabilidades ++ habilidad unChico} 

serMayor :: Chico -> Chico
serMayor unChico = unChico {edad = 18} 
-- serGrosoEnNeedForSpeed unChico = aprenderHabilidades 

--  1. wanda: dado un chico, wanda le cumple el primer deseo 
-- y lo hace madurar (crecer un año de edad).

wanda :: Deseo
wanda = controlarEdad (+) 1 . cumplirDeseo Uno 

data NumeroDESEO = Uno | Ninguno | Todos 
cumplirDeseo :: NumeroDESEO -> Deseo
cumplirDeseo Uno unChico = (head $deseo unChico) unChico  
cumplirDeseo Ninguno unChico =  unChico  
cumplirDeseo Todos unChico =  foldl aplicarUnDeseo unChico $deseo unChico  
-- cumplirDeseo Todos unChico = head $deseo unChico  
    
aplicarUnDeseo :: Chico -> Deseo -> Chico
aplicarUnDeseo unChico unDeseo = unDeseo unChico

controlarEdad :: (Number-> Number-> Number) -> Number ->  Chico -> Chico 
controlarEdad operacion numero unChico =  unChico {edad = operacion numero (edad unChico)} 

--   2. cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de edad. 
-- Como es olvidadizo, no le concede ningún deseo.

cosmo :: Deseo
cosmo = controlarEdad (*) 0.5 . cumplirDeseo Ninguno 

--   3. muffinMagico: dado un chico le concede todos sus deseos. 

muffinMagico :: Deseo
muffinMagico = cumplirDeseo Todos

-- 2. **En busqueda de pareja** 
-- 1. tieneHabilidad unaHabilidad unChico : Dado un chico y una habilidad, dice si la posee.

tieneHabilidad :: Habilidades -> Chico -> Bool
--tieneHabilidad unaHabilidad unChico = elem unaHabilidad $habilidad unChico
tieneHabilidad unaHabilidad  = elem unaHabilidad . habilidad 

esSuperMaduro :: Chico -> Bool
esSuperMaduro unChico = tieneHabilidad "Sabe manejar" unChico && ((>18).edad) unChico

-- para Trixie la única condición es que el chico no sea Timmy, 

data Chica = Chica {
    nombreChica :: String,
    condicionDeEleccion :: Chico -> Bool
    } deriving (Show,Eq)

trixie = Chica "Trixie Tang" noEsTimmy
vicky = Chica "Vicky" (tieneHabilidad "ser un supermodelo noruego") 

noEsTimmy =  (/= "Timmy").nombre  
noEsTimmy' unChico = nombre unChico /= "Timmy"

-- quienConquistaA :: Chica -> [Chico] -> Chico
-- quienConquistaA unaChica losPretendientes = head.filter (condicionDeEleccion unaChica) losPretendientes