let rec mcd' x y = 
	if x = 0 then y
	else if y = 0 then x
	else 
		begin
			if x > y then mcd (x mod y) (y)
			else mcd (x)(y mod x)
		
		end 

let mcd_pasos x y : int * int = 
	let rec aux_func (mcd, pasos) (x) (y) = 
			if x = 0 then (y, pasos + 1)
			else if y = 0 then (x, pasos + 1)
			else 
				begin
					if x > y then aux_func (mcd, pasos + 1) (x mod y) (y)
					else aux_func (mcd, pasos + 1) (x) (y mod x)
				
				end 
	in aux_func (0, 0) (x) (y)


