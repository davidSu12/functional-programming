let rec binstr_of_int (n : int) : string = 
	if n = 0 then ""
	else binstr_of_int(n / 2) ^ string_of_int(n mod 2) 

let int_of_binstr(cadena : string ) = 
	let rec aux_func (pivote: int) (cadena:string) (longitud_cadena: int) = 
		if pivote = longitud_cadena then
			if cadena.[pivote] = '1' then 1
			else 0
		else
			if cadena.[pivote] = '1' then
				int_of_float(2. ** float_of_int(longitud_cadena - pivote)) + aux_func(pivote + 1)(cadena)(longitud_cadena)
			else
				aux_func(pivote + 1)(cadena)(longitud_cadena)
	in aux_func(0)(cadena)(String.length(cadena)-1)           