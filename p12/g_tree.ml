type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t));;


let rec height = function (* la lista se llama "hijos" *)
    Gt (r,[]) -> 1
   |Gt (r, hijos) -> 1 + (List.fold_left (max) 1 (List.map height hijos));; 
   

let rec leaves = function
    Gt (r,[]) -> [r]
   |Gt (_,hijos) -> List.fold_left (@) [] (List.map leaves hijos);; 
   
   
   
let rec mirror = function
    Gt (r,[]) -> Gt(r,[])
   |Gt (r, hijos) -> Gt(r, List.map mirror (List.rev hijos));;
   
      
let rec preorder = function
    Gt (r,[]) -> [r]
   |Gt (r,hijos) -> List.fold_left (@) [r] (List.map preorder hijos);;
   
   
let rec postorder = function
    Gt (r,[]) -> [r]
   |Gt (r,hijos) -> List.fold_right (@) (List.map postorder hijos) [r];;   
   
   
   
   
   
   
   
   
   

