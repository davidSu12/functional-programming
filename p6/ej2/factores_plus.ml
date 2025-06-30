let factoriza (n: int) : string = 
	if n = 0 then "0"
	else if n = 1 then "1"
	else if n = (-1) then "(-1)"
	else
		let rec func_aux (counter : int) (numero: int) (cadena: string) (exponente: int): string = 

				if numero mod counter = 0 then
					(*
						Tenemos que ver si estamos en el final o no
					*)
					if numero / counter = 1 then
						if exponente = 1 then
							cadena ^ string_of_int(counter)
						else
							cadena ^ string_of_int(counter)^"^"^string_of_int(exponente)

					else if numero / counter = -1 then
						if exponente = 1 then
							"(-1) * "^cadena^string_of_int(counter)
						else
							"(-1) * "^cadena^string_of_int(counter)^"^"^string_of_int(exponente)
					else
						if (numero / counter) mod counter = 0 then 

							func_aux(counter)(numero / counter)(cadena)(exponente + 1)
						else 
							if exponente = 1 then

								func_aux(counter)(numero / counter)(cadena ^ string_of_int(counter)^" * ")(exponente) 

							else 
								func_aux(counter)(numero / counter)(cadena ^ string_of_int(counter)^"^"^string_of_int(exponente)^" * ")(exponente) 

				else
					func_aux(counter + 1)(numero)(cadena)(1)
			in func_aux(2)(n)("")(1)

		