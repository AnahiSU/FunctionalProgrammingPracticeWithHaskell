sumarUno x = x+1
mayorDeDos a b = if a>b then a else b
--fechaMayor :: Int -> Int -> Int -> Int -> Int -> Int -> (Int, Int, Int)
mayorDeCuatrov1 a b c d = mayorDeDos a (mayorDeDos b (mayorDeDos c d))

--practica 3
mayorDeCuatrov2 a b c d
	| a > b && a > c && a > d = a
	| b > c && b > d = b
	| c > d = c
	| otherwise = d
estadoDeNota nota
	| nota > 50 = "Aprobado"
	| otherwise = "Reprobado"
calificacion nota
	| nota >= 90 = "Excelente"
	| nota >=70 && nota < 90 = "Bien"
	| nota >= 51 && nota <= 69 = "Regular"
	| otherwise = "Mal"
estadoSemestre pp sp ef si
	| (pp + sp)/2 > 50 = "Aprobado"
	| ef > 50 = "Aprobado"
	| si > 50 = "Aprobado"
	| otherwise = "Reprobado"
esMayorA1 num den 
	| num > den = True
	| otherwise = False
anhosTranscurridos d m a d1 m1 a1
	| a>a1 = a-a1
	| a<a1 = a1-a
	| otherwise = 0

--practica 4

sigVoc v = case v of
			'a' -> 'e'
			'e' -> 'i'
			'i' -> 'o'
			'o' -> 'u'
			'u' -> 'a'
			_ -> '?'
literal num = case num of
			1 -> "uno"
			2 -> "dos"
			3 -> "tres"
			4 -> "cuatro"
			5 -> "cinco"
			6 -> "seis"
			7 -> "siete"
			8 -> "ocho"
			9 -> "nueve"
			0 -> "cero"
miAnd val1 val2 = case val1 of
			True -> case val2 of
					True -> True
					False -> False
			_ -> False

miOr val1 val2 = case val1 of
			False -> val2
			True  -> True

miXOr val1 val2 = case val1 of
			True -> case val2 of	
					True -> False
					_ -> True
			False -> case val2 of
					False -> False
					_ -> True

operarEn val1 val2 op = case op of
			"and" -> miAnd val1 val2
			"or" -> miOr val1 val2
			"xor" -> miXOr val1 val2

--case con guardas "|" y variables locales
sumatoria a b c = case () of
    _ | suma < 10   -> "Sumatoria menor"
      | suma < 20   -> "Sumatoria mayor"
      | otherwise   -> "Vacio"
  where
    suma = a + b + c


