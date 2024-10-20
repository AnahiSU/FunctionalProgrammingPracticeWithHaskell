--Práctica Funciones estándar	
estaOrd f xs = and (zipWith f xs (tail xs))
sonIguales xs zs = and(zipWith (==) xs zs)
esMat xss = length xss == length (filter (== length xss) (map length xss))
miLength xs = sum(map f xs)
	where 
		f a =  1
miFilter f xs = concat (map g xs)
	where 
		g a = if(f a) then [a] else []

miZip xs zs = map g [0..max]
	where 
		 max = (min (length xs) (length zs))-1
		 g pos = (xs!!pos,zs!!pos)
trans xss = map f [0..max]
	where
		max = (length (head xss)) - 1
		f pos = map (!!pos) xss
