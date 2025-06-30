let factoriza (n: int) : string = 
	if n = 0 then "0"
	else if n = 1 then "1"
	else if n = (-1) then "(-1)"
	else
		let rec func_aux (counter : int) (numero: int) (cadena: string) : string = 

				if numero mod counter = 0 then
					(*si nos encontramos en el final*)
					if numero / counter = 1 then
						cadena ^ string_of_int(counter)
					else if numero /counter = -1 then
						"(-1) * " ^ cadena ^ string_of_int(counter)
					else
						 func_aux(counter)(numero / counter)(cadena ^ string_of_int(counter) ^ " * ")
				else
					func_aux(counter + 1)(numero)(cadena)
			in func_aux(2)(n)("")

		