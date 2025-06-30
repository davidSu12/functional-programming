let hd = function
	|[] -> raise(Failure "list is empty") 
	|h::t -> h

let tl = function
	|[] -> raise(Failure "list is empty")
	|h::t -> t

let rec last = function
	|[] -> raise(Failure "list is empty")
	|h::[] -> h
	|h::t -> last(t)

let rec length = function
	|[] -> 0
	|h::t -> 1 + length(t)

let length' lst = 
	let rec aux_func(acc)(lst) = 
		match lst with
		|[] -> acc
		|h::t -> aux_func(acc + 1)(t)
	in aux_func(0)(lst)


let compare_lengths (lst1) (lst2) = 
	let rec aux_func (lst1) (lst2) = 
		match lst1, lst2 with
		|[],[] -> 0
		|h::_, [] -> 1
		|[], h::_ -> -1
		|h::t, m::n -> aux_func(t)(n)
	in aux_func(lst1)(lst2)

let append (lst1) (lst2) = 
	let rec aux_func (lst1) (lst2) = 
		match lst1 with
			|[] -> lst2
			|h::[] -> h::lst2
			|h::t -> h :: aux_func(t)(lst2)
	in aux_func(lst1)(lst2)	

let rev_append (lst1) (lst2) = 
	let rec aux_func (lst1)(lst2) = 
		match lst1 with
			|[] -> lst2
			|h::[] -> h::lst2
			|h::t -> aux_func(t)(h::lst2)
	in aux_func(lst1)(lst2) 

let rev (lst1) = 
	let rec aux_func(lst1)(pivot) = 
		match lst1 with
			|[] -> []
			|h::[] -> h ::pivot
			|h::t -> aux_func(t) (h :: pivot)

	in aux_func(lst1)([])


let concat (lst) =
	let rec func_aux(lst) = 
		match lst with
		|[] -> []
		| h::[] -> begin
					match h with 
					|[] -> []
					|m :: [] -> m :: []
					|m :: t -> m :: func_aux([t])
				end
		| h::m -> begin
					match h with
					|[] -> func_aux(m)
					|n ::[] -> n :: func_aux(m) 
					|n :: t -> n :: func_aux(t :: m)
				end
	in func_aux(lst)


let flatten (lst) =
	let rec func_aux(lst) = 
		match lst with
		|[] -> []
		| h::[] -> begin
					match h with 
					|[] -> []
					|m :: [] -> m :: []
					|m :: t -> m :: func_aux([t])
				end
		| h::m -> begin
					match h with
					|[] -> func_aux(m)
					|n ::[] -> n :: func_aux(m) 
					|n :: t -> n :: func_aux(t :: m)
				end
	in func_aux(lst)

let init n f = 
	let rec aux_func n f count = 
		if count = n - 1 then 
			f(n-1) :: []
		else 
			f(count) :: aux_func n f (count + 1)
	in aux_func n f 0


let nth lst n =
	if n < 0 then raise(Invalid_argument "n is negative")
	else
		let rec aux_func lst n count =
			match lst with
			|[] -> raise(Failure "list is to short")
			|h::t -> begin 
						if count = n then 
							h
						else
							aux_func(t)(n)(count + 1)
					end 		
		in aux_func(lst)(n)(0)

let rec map f lst = 
	match lst with
	|[] -> []
	|h :: t -> f(h) :: map(f)(t)

let rev_map f (lst) = 
	let rec aux_func f lst aux_lst= 
		match lst with 
		|[] -> aux_lst
		|h :: [] -> f(h) :: aux_lst
		|h :: t -> aux_func f t (f(h)::aux_lst)
	in aux_func f lst []


let rec map2 f lst1 lst2 = 
	match lst1,lst2 with 
	|[],[] -> []
	|_, [] -> raise(Invalid_argument "different length list")
	|[], _ -> raise(Invalid_argument "different length list")
	|h::t, p::q -> f (h)(p) :: map2 f (t)(q)

let rec combine lst1 lst2 = 
	match lst1, lst2 with
	|[],[] -> []
	|_,[] -> raise(Invalid_argument "different length list")
	|[],_ -> raise(Invalid_argument "different length list")
	|h::t, p::q -> (h,p) :: combine(t)(q)
 

let rec split lst =
  match lst with
  | [] -> ([], [])
  | (x,y)::rest ->
      let (xs, ys) = split rest in
      (x::xs, y::ys)

let rec find f lst = 
	match lst with
	|[] -> raise(Not_found)
	|h::t -> if f(h) then h else find f t

let rec filter f lst = 
	match lst with 
	|[] -> []
	|h::t -> if f(h) then h :: filter(f)(t) else filter(f)(t)

let filter' f lst = 
	let rec func_aux f lst pivot = 
		match lst with
		|[] -> List.rev pivot 
		|h::t -> if f(h) then func_aux (f) (t) (h::pivot) else func_aux (f) (t) (pivot)
	in func_aux f lst []

let rec partition f lst =
	match lst with
	|[] -> ([], [])
	|h::t -> 
		let (xs, ys) = partition f t
		in begin
			if f(h) then (h::xs, ys)
			else (xs, h::ys)
		end

let partition' f lst = 
	let rec aux_func f lst (x,y) = 
		match lst with
		|[] -> (x,y)
		|h::t -> begin
			if f(h) then aux_func f t (h::x, y)
			else aux_func f t (x, h::y) 
		end
	in aux_func f lst ([],[])

let for_all f lst = 
	let rec aux_func f lst (acc) = 
		match lst with
		|[] -> acc
		|h :: t -> begin
			if f(h) then aux_func f t (acc && true)
			else false
		end 
	in aux_func f lst (true)

let exists f lst = 
	let rec aux_func f lst (acc) = 
		match lst with
		|[] -> acc
		|h :: t -> begin 
			if f(h) then true else aux_func (f)(t)(acc || false)
		end
	in aux_func f lst (false)

let rec mem n lst = 
	match lst with
	|[] -> false
	|h::t -> begin 
		if h = n then true else mem n t
	end
let fold_left f n lst = 
	let rec aux_func f n lst acc = 
		match lst with 
		|[] -> raise (Invalid_argument "empty list")
		|h::[] -> f (acc) (h)
		|h::t ->  aux_func f n t (f(acc) h)
	in aux_func f n lst n

let rec fold_right f lst n =
	match lst with 
	|[] -> raise(Invalid_argument "empty list")
	| h::[] -> f(h)(n)
	|h :: t -> let el = fold_right f t n in
		begin
			f (h) (el)
		end 