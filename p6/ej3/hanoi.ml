

let otro (x: int ) (y: int) : int = 
    abs(x - y)

let move (ori,des) :string = 
    (*solamente tenemos que considerar los tres origenes*)
    if ori = 1 then
        if des = 2 then
            "1 -> 2    3\n"
        else
            "1 ---2--> 3\n"
    else if ori = 2 then
        if des = 1 then 
            "1 <- 2    3\n"
        else
            "1    2 -> 3\n"
    else 
        if des = 1 then 
            "1 <--2--- 3\n"
        else
            "1    2 <- 3\n"


let rec hanoi n ori des = 
    (* n número de discos, 1 <= ori <= 3, 1 <= dest <= 3, ori <> des *)
    if n = 0 then "" else
    let otro = otro ori des in
    hanoi (n-1) ori otro ^ move (ori, des) ^ hanoi (n-1) otro des
    
let hanoi n ori des =
    if n = 0 || ori = des then "\n"
    else hanoi n ori des    
       
let print_hanoi n ori des =
    if n < 0 || ori < 1 || ori > 3 || des < 1 || des > 3
       then print_endline  " ** ERROR ** \n"
       else print_endline (" =========== \n" ^ 
                           hanoi n ori des ^
                           " =========== \n")

let crono f x = 
    let t = Sys.time () in
    f x; Sys.time () -. t


let nth_hanoi_move (n:int) (nd:int) (ori: int) (des: int) = 

    let rec aux_func (n: int ) (nd: int) (ori: int) (des: int) : int * int =

        let pivot = (int_of_float(2. ** float_of_int(nd)) - 2) / 2 + 1
        in 
            if n < pivot then
                let mal = otro ori des in
                aux_func(n)(nd-1)(ori)(mal)
            else if pivot < n then 
                let mal = otro ori des in
                aux_func(n - pivot)(nd-1)(mal)(des)
            else
                (ori, des)

    in aux_func(n)(nd)(ori)(des)
