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

cholesterol :: Integer -> String
cholesterol ldl =
	if ldl < 100 && ldl > 50
		then "Bajo"
		else if ldl > 99 && ldl < 130
			then "Media"
			else if ldl > 129 && ldl < 160
				then "Alta"
				else if ldl > 159 && ldl < 190
					then "Muy alta"
					else "¿Estás muerto? :o"

bloodSugar :: Integer -> Bool
bloodSugar diabetes =
	if diabetes > 120
		then True
		else False

restingElectrocardiography :: Float -> String
restingElectrocardiography electroReposo = 
	if electroReposo >= -0.5 && electroReposo <= 0.4
		then "Normal"
		else if electroReposo >= 2.45 && electroReposo <1.8
			then  "ST-T anormal"
			else if electroReposo >= 1.4 && electroReposo<=2.5
				then "Hipertrofia"
				else "Valor no válido"


maximumHeartRate :: Integer -> String
maximumHeartRate frecuenciaCardiaca =
	if frecuenciaCardiaca < 111 && frecuenciaCardiaca > 50
		then "Baja"
		else if frecuenciaCardiaca > 110 && frecuenciaCardiaca < 153
			then "Media"
			else if frecuenciaCardiaca > 151
				then "Alta"
				else "Valor no válido"


exercise :: String -> Bool
exercise ejercicio =
	if ejercicio == "Si"
		then True
		else False

depressionST :: Float -> String
depressionST oldpeak =
	if oldpeak >= 1.5 && oldpeak <= 4.2
		then "Riesgoso"
		else if oldpeak >4.2
			then "Terrible"
			else "Valor no contemplado"

talliumScan :: Integer -> String
talliumScan talio =
	if talio == 3
		then "Normal"
		else if talio == 6
			then "Defecto tratable"
			else if talio == 7
				then "Defecto irreversible"
				else "Valor fuera de rango"


sex :: String -> Integer
sex sexo =
	if sexo == "Femenino"
		then 0
		else if sexo == "Masculino"
			then 1
			else -1

age :: Integer -> String
age edad =
	if edad < 33 && edad >= 0
		then "Joven"
		else if edad >= 33 && edad <= 40
			then "Mediana edad"
			else if edad > 40 && edad <= 58
				then "Viejo"
				else "Muy viejo"


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