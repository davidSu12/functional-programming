let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1


let length'n'top n =
	let rec aux_func (acc, m) (n) =
		if n = 1 then (acc,m)
		else aux_func(1+acc, max (n) (m)) (f(n))
	in aux_func (1, n) (n)



