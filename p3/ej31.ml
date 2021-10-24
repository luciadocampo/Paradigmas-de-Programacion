
let e1 = 
	(function pi -> pi *. (pi +. 1.)) (2. *. asin 1.);;


let e2 = 
	(function lg2 -> function log2 -> log log2 /. lg2) (log 2.) (float (1024*1024));;


let e3 = 
	(function pi_2 -> function r -> pi_2 *. r) (4. *. asin 1.);;


let e4 = 
	(function sqr -> function pi -> function r -> pi *. sqr r)(function x-> x *. x)(2. *. asin 1.);;
	
	
let abs n =
	(function true -> n | false -> - n) (n >= 0);;


let par n = 
	(function true -> true | false -> false) (n mod 2 = 0);;


let saluda s = 
	(function true -> print_endline "Hola!" | false -> ()) (s = "Hola");;


let f n = 
	(function true -> "es par" | false -> "es impar") (n mod 2 = 0);;


let f n =
	(function true -> "múltiplo de 2" | false -> ((function true -> "múltiplo de 3" | false -> "impar") (n mod 3 = 0))) (n mod 2 = 0);;
