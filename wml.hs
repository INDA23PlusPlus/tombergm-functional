first (x:xs) = x
next (x:xs) = xs

len x = len_ (0,x)
len_ (n,[]) = n
len_ (n,(x:xs)) = len_(n+1,xs)

sort [] = []
sort [x] = [x]
sort (x:xs) =
	if len x < len (first (sort xs)) then
		x : sort xs
	else
		first (sort xs) : sort (x : next (sort xs))

l (x:xs) = xs
r [a,b] = [a]
r (x:xs) = x : r xs

wml x = ml (sort x)
ml [] = 0
ml [x] = len x
ml [a,b] = (len a + len b) / 2
ml x = mr (l x)
mr [a,b] = len a
mr x = ml (r x)
