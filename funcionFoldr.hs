--Práctica de Foldr
prod xs = foldr (*) 1 xs
miMap f xs = foldr g [] xs
	where 
		g a ys = (f a) : ys
miFilterFo f xs = foldr g [] xs
	where 
		g a ys = if (f a) then a:ys else ys
miLengthF xs = sum(foldr g [] xs)
	where 
		g a ys = 1:ys
miUnion xs ys = foldr g ys xs
	where 
		g a zs = a:zs
miReverse xs = foldr g [] xs
	where 
		g a zs = zs++[a]
miConcat xss = foldr g [] xss
	where 
		g ys zs = foldr h zs ys
		h a js = a:js
miTakeWhile f xs = snd (foldr g (True,[]) xs)
	where 
		g a (b,ys) = if b && (f a) then (True,(a:ys)) else (False,[])
miNum xs = foldr g 0 (zip xs [0..])
	where 
		tam = (length xs)-1
		g (a,b) v = v + (a * (10^(tam-b)))

elMinimo xs = foldr (\x y -> min x y) 10000000 xs

reemplazarMin xs = fst (foldr g ([],elMinimo xs) xs)
	where 
		g a (ys,v) = (v:ys,v)
buscaYCuenta u v = foldr f a v
	where 
		f w (rs,tam) = if w == u then (tam:rs,tam+1) else (rs,tam+1)
		a = ([],0)
