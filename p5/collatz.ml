let f a = 
	if a mod 2 = 0 then a / 2 
	else 3 * a + 1
;;

(*función orbit*)

let rec orbit x =
	if x=1 then print_endline "1" 
	else (print_string(string_of_int x ^", "); orbit (f x))
;;


(*función lenght recursiva*)

let rec length n = 
	if n = 1 then 0 
	else 1 + length (f n)
;;

(*función recursiva top*)
  
let rec top t =
	if t = 1 then 1 
	else max t (top (f t))
;;

(*función recursiva lenght'n'top*)

let rec length'n'top n =
	if n = 1 then (0, 1)
	else let (contar, maximo) = length'n'top (f n) 
		in (contar + 1, max n (maximo))
;;

