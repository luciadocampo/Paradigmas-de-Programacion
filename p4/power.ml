let rec power (x:int) (y:int) =
 if  y = 1  then x
 else x * power x (y-1);; 
 

let power' x y =
if y mod 2 = 0 then int_of_float((float_of_int(x * x)) ** (float_of_int(y / 2)))
else
int_of_float(float_of_int(x*int_of_float((float_of_int(x*x))**float_of_int(y/2))));;



(*función x^n; x:float, n:int, n >= 0*)

let rec powerf (x:float) (y:int) =
 if  y = 1  then x
 else x *. powerf x (y-1);;
