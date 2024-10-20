--Listas de comprensión
miFilterLC f xs = [x|x<-xs, f x]
miMapLC f xs = [f x|x<-xs]
miConcatLC xss = [y|xs<-xss,y<-xs]
miLengthLC xs = sum [1|x<-xs]
encrip xs = [f x | x<-xs]
	where 
		f v = case v of
			'a'-> '1'
			'e'-> '2'
			'i'-> '3'
			'o'-> '4'
			'u'-> '5'
			_->v
prodCart xs ys = [(x,y)|x<-xs, y<-ys]
intersec xs ys = [x|x<-xs, estaEn ys x]
eliminar xs v = [x|x<-xs,x/=v]
estaEn xs v = length [x|x<-xs,x==v] >=1
union xs ys = xs ++ [y|y<-ys,not (estaEn xs y)]
repes xs = [x | (x, i) <- zip xs [0..], encontrar (x,i) xs]
  where 
    encontrar (v,pos) zs = length (filter (==v) (take pos zs)) == 0
diagP xss = [f x | x<-zip xss [0..]]
	where 
		f (rs,pos) = rs!!pos
miZipWith f xs ys = [f x y | (x,y) <-zip xs ys]
miZip3 xs ys zs = [(x,y,z) | pos<-[0..maxi-1],x<-xs!!pos, y<-ys!!pos, z<-xs!!pos]
		where
			maxi = min (length xs) (min (length ys) (length zs))
respetaOrd f xs = and [(f (xs!!pos) (xs!!(pos+1)))|pos<-[0..n-2] ]
	where 
		n = length xs
