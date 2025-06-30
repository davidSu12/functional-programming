let rec fact n = 
	if n = 0 then 1 
	else n * fact(n - 1);;

let rec sum n = 
	if n = 0 then 1.
	else (1. /. float_of_int (fact(n))) +. sum(n-1);;

let () = print_endline(string_of_float (sum(10))) 