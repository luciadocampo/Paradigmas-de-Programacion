let rec fact = function
0 -> 1
| n -> n * fact (n - 1)
in print_endline (string_of_int (fact (int_of_string(Sys.argv.(1)))))
