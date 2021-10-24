(*Un valor u de tipo int a partir de una expresión que contenga, al menos, 4 operadores infijos*)
		let u = (6 / 2 + 3 - 1) * 3;;

(*Un valor v de tipo float a partir de una expresión que incluya una función predefinida*)
		let v= sin(0.9);;
		
(*Un valor w de tipo char a partir de una expresión que incluya una sub-expresión de tipo int*)	
		let w = Char.chr 98;;	
		
(*Un valor x de tipo bool a partir de una expresión que incluya una o más funciones u operadores*)
		let x = ('a' > 'b') || (abs 2 < 7);;
		
(*Un valor y de tipo string a partir de una expresión que contenga una frase if-then else*)		
		let y = if 8 > 1 then "Verdadero" else "Falso";;		
