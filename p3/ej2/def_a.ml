let pi = 3.14159365

let e = 2.717271

let max_int_f = float_of_int(max_int)

let per x =  2. *. pi *. x

let area x =  pi *. x *. x

let next_char car =  Char.chr(Char.code(car) + 1)

let absf num = if num >= 0. then num else -. num

let odd n = if n mod 2 = 0 then true else false

let next5mult n = 	if n mod 5 = 0 then n + 5
					else let m = n mod 5 in (5-m) + n

let is_letter car = if 'a' <= car && car <= 'z' then true else false

let string_of_bool n =  if n then "verdadero" else "falso"
