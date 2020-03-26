{-|
  Dolor en el pecho: esta variable de entrada admite 
  4 tipos de dolor en el pecho. Se ha definido un valor 
  en este sistema para cada tipo de dolor torácico que 
  utilizamos estos valores para las pruebas del sistema. 
-}
painChest :: String -> Integer
painChest dolor =
	if dolor == "Angina típica"
		then 1
		else if dolor == "Angina atípica"
			then 2
			else if dolor == "Dolor no anginal"
				then 3
				else if dolor == "Asintomático"
					then 4
					else -1

{-|
  Presión arterial: diferentes valores de presión arterial 
  cambian el resultado fácilmente. 
  En este campo, utilizamos la presión arterial sistólica.
  Los conjuntos difusos son "Bajo", "Medio", "Alto" y "Muy alto".
-}
bloodPressure :: Integer -> String
bloodPressure arterialS =
	if arterialS > 110 && arterialS < 127
		then "Baja"
		else if arterialS > 126 && arterialS < 142
			then "Media"
			else if arterialS > 141 && arterialS < 173
				then "Alta"
				else if arterialS > 172
					then "Muy alta"
					else "¿Estás muerto? :o"

{-|
  Colesterol: el colesterol tiene un efecto importante en el 
  resultado y puede cambiarlo fácilmente. 
  Para este campo de entrada, utilizamos el valor del colesterol de 
  lipoproteínas de baja densidad (LDL). 
  Se divide en Bajo, Medio, Alto y Muy alto
-}
cholesterol :: Integer -> String
cholesterol ldl =
	if ldl < 100 && ldl > 50
		then "Bajo"
		else if ldl > 99 && ldl < 130
			then "Medio"
			else if ldl > 129 && ldl < 160
				then "Alto"
				else if ldl > 159 && ldl < 190
					then "Muy alto"
					else "¿Estás muerto? :o"

{-|
  Azúcar en la sangre (diabetes): el campo de azúcar en la 
  sangre es uno de los factores más importantes en este sistema
  que cambia el resultado.
  Sí la azúcar en la sangre excede los 120, se considera que es alto.
-}
bloodSugar :: Integer -> Bool
bloodSugar diabetes =
	if diabetes > 120
		then True
		else False

{-|
  Electrocardiografía de reposo (ECG): en este campo, tenemos 
  3 campos (Normal, ST_T anormal e hipertrofia).
-}
restingElectrocardiography :: Float -> String
restingElectrocardiography electroReposo = 
	if electroReposo >= -0.5 && electroReposo <= 0.4
		then "Normal"
		else if electroReposo >= 2.45 && electroReposo <1.8
			then  "ST-T anormal"
			else if electroReposo >= 1.4 && electroReposo<=2.5
				then "Hipertrofia"
				else "Valor no válido"

{-|
  Frecuencia cardíaca máxima: el valor de este campo de entrada 
  es la frecuencia cardíaca máxima del humano en 24 horas. 
  Al aumentar la edad, se disminuye la frecuencia cardíaca
  máxima en 24 horas. En este campo, tenemos 3 variables lingüísticas
  (conjuntos difusos) (Baja, Media y Alta).
-}
maximumHeartRate :: Integer -> String
maximumHeartRate frecuenciaCardiaca =
	if frecuenciaCardiaca < 111 && frecuenciaCardiaca > 50
		then "Baja"
		else if frecuenciaCardiaca > 110 && frecuenciaCardiaca < 153
			then "Media"
			else if frecuenciaCardiaca > 151
				then "Alta"
				else "Valor no válido"

{-|
  Creo que esta se explica solita.
-}
exercise :: String -> Bool
exercise ejercicio =
	if ejercicio == "Si"
		then True
		else False

{-|
  Este campo de entrada significa depresión del ST inducida
  por el ejercicio en relación con el descanso. 
-}
depressionST :: Float -> String
depressionST oldpeak =
	if oldpeak >= 1.5 && oldpeak <= 4.2
		then "Riesgoso"
		else if oldpeak >4.2
			then "Terrible"
			else "Valor no contemplado"

{-|
  Un examen de talio, el resultado, indica qué pasa contigo.
-}
talliumScan :: Integer -> String
talliumScan talio =
	if talio == 3
		then "Normal"
		else if talio == 6
			then "Defecto tratable"
			else if talio == 7
				then "Defecto irreversible"
				else "Valor fuera de rango"

{-|
  Indica el sexo del sujeto y se le asigna un valor
  Femenino = 0
  Masculino = 1
-}
sex :: String -> Integer
sex sexo =
	if sexo == "Femenino"
		then 0
		else if sexo == "Masculino"
			then 1
			else -1

{-|
  Clasifica al sujeto según su edad.
  [0,32] -> Joven
  [33,40] -> Mediana edad
  [41,58] -> Viejo
  [59) -> Muy viejo
-}
age :: Integer -> String
age edad =
	if edad < 33 && edad >= 0
		then "Joven"
		else if edad >= 33 && edad <= 40
			then "Mediana edad"
			else if edad > 40 && edad <= 58
				then "Viejo"
				else "Muy viejo"

{-|
  Es una preguntita, pensada para el dolor de pecho.
  ¿Tienes dolor de pecho?
  Si -> True
  No o cualquier otra cosa -> False
-}
questionPC :: String -> Bool
questionPC pregunta =
	if pregunta == "Si"
		then True
		else False


main = do
    print (painChest "Angina atípica")
    print (bloodPressure 140)
    print (cholesterol 160)
    print (bloodSugar 130)
    print (restingElectrocardiography 0.3)
    print (maximumHeartRate 130)
    print (exercise "Si")
    print (depressionST 1.5)
    print (talliumScan 3)
    print (sex "Femenino")
    print (age 34)
    print (questionPC "Si")