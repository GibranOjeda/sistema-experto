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








main = do
       print (painChest "Angina atípica")
       print (bloodPressure 140)
       print (cholesterol 160)

