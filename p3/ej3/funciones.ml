let rec sumto n = if n = 0 then 0 else n + sumto(n-1);;
let rec exp2 n = if n = 0 then 1 else 2 * exp2 (n - 1);;
let rec num_cifras n = 
	if n = 0 then 1
	else if n / 10 = 0 then 1 
	else num_cifras( n / 10) + 1;;                 
let rec sum_cifras n = if n = 0 then 0 else n mod 10 + sum_cifras(n / 10);;