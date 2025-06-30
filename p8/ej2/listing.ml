let from0to n = 
	if n < 0 then raise(Failure "n < 0")
	else
	let rec func counter n acc = 
		if counter = n then
			List.rev(counter :: acc)
		else func (counter + 1) (n) (counter :: acc)
	in func 0 n []

let to0from n = 
	if n < 0 then raise(Failure "n < 0")
	else
	let rec func counter n acc = 
		if counter = n then
			counter :: acc
		else func (counter + 1)(n)(counter :: acc)
	in func 0 n []

let pair x lst = 
	let rec func x lst acc = 
		match lst with
		|[] -> List.rev(acc)
		|h :: t -> func (x) (t) ((x,h) :: acc)
	in func x lst []

let remove_first n lst = 
	let rec aux_func n lst lst1 lst2 = 
		match lst2 with
		| [] -> raise(Failure "not")
		| h::t -> begin
					if h = n then
						List.rev_append (lst1) (t)
					else
						aux_func (n) (lst) (h :: lst1)  (t)
					end 
	in aux_func (n) (lst) ([]) (lst)

let remove_all n lst = 
	List.filter (fun x -> x <> n) lst
	

let ldif lst1 lst2 = 
	List.filter (fun x -> List.mem (x) (lst2) = false)(lst1)