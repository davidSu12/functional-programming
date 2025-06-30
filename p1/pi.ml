let sign (n) = 
	if n mod 2 = 0 then 1. else -1.;;

let odd (n) = 
	(2.*.n) +. 1.;;


let rec sum (n) = 
	if n > 0. then 
		sign(int_of_float n) *. (1./.odd(n)) +. sum (n -. 1.)
	else 1.;;

let pi (n) = 
	4. *. sum n;;

let () = print_endline (string_of_float (pi(10000)))