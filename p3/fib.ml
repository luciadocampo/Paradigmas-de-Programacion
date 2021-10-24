let rec fib n =
	if n <= 1 then n
	else fib (n-1) + fib (n-2);;


let rec f = function
	0 -> print_endline (string_of_int(fib 0))
|n -> let _ = f (n-1) in
		 print_endline (string_of_int(fib n));;


if Array.length Sys.argv = 2 then

	f(int_of_string Sys.argv.(1));;
