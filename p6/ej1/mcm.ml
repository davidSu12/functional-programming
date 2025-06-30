let rec mcd' x y = 
	if x = 0 then y
	else if y = 0 then x
	else 
		begin
			if x > y then mcd' (x mod y) (y)
			else mcd' (x)(y mod x)
		
		end 

let mcm x y = 
	(x * y) / mcd' (x) (y)

let mcm' x y = 
	if x <= 0 || y <= 0 then -1
	else mcm (x) (y)