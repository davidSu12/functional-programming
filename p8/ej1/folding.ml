

let i_prod lst = 
	List.fold_left ( * )(1)(lst)

let f_prod lst = 
	List.fold_left ( *. )(1)(lst)

let lmin  lst = 
	match lst with
	|[] -> raise (Failure "empty list")
	|h::t -> List.fold_left (min)(h)(t)


let min_max lst = 
	match lst with
	|[] -> raise (Failure "empty list")
	|h::t -> (List.fold_left (min)(h)(t) , List.fold_left (max)(h)(t))


let rev lst = 
	match lst with
	|[] -> raise (Failure "empty list")
	|h::t -> List.fold_left (fun x y -> y::x)([h])(t)

let rev_append lst1 lst2 = 
	match lst1 with
	|[] -> raise (Failure "empty list")
	|_ -> List.fold_left(fun x y -> y :: x)(lst2)(lst1)

let rev_map f lst = 
	match lst with
	|[] -> raise (Failure "empty list")
	|_ -> List.fold_left (fun x y -> f(y) :: x)([])(lst)

(* f init el*)

let concat lst = 
	match lst with
	|[] -> raise (Failure "empty list")
	|_ -> List.fold_left (fun init el -> init ^ el)("")(lst)