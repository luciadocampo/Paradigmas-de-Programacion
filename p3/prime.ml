(*is_prime*)

let is_prime n =
  let rec check_from i =
	i >= n ||
	 (n mod i <> 0 && check_from (i+1))
  in check_from 2;;


(*next_prime*)

let rec next_prime n = 
	let n = succ n in
		if is_prime n then n else next_prime (n+1);;
		
(*last_prime_to*)

let rec last_prime_to n = 
	if is_prime n then n else last_prime_to (n-1);;
		
(*is_prime2*)

let is_prime2 n = 
	if (n mod 2 <> 0)
		then let n = abs n in
			let rec not_div_from d = 
				d * d > n || (n mod d <> 0 && not_div_from (d+2))
			in n > 1 && not_div_from 3
		else false;;
