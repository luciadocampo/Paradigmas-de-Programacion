open List;;

let rec qsort1 ord = function
  [] -> []
  | h::t -> let after, before = List.partition (ord h) t in
            qsort1 ord before @ h :: qsort1 ord after;;
(* no es buena para los casos en los que la lista no está balanceada *)



let rec qsort2 ord =
  let append' l1 l2 = List.rev_append (List.rev l1) l2 in
  function
    [] -> []
    | h::t -> let after, before = List.partition (ord h) t in
              append' (qsort2 ord before) (h :: qsort2 ord after);;


(* la primera ventaja es que al no usar el @ puede utilizar funciones terminales como rev y rev_append.
   También, qsort2 es más rápida cuando la lista ya está inicialmente ordenada *)
   
(* qsort2 sí que podrían ordenar listas que qsort1 no; qsort2 es capaz de ordenar listas más grande sin dar StackOverflow *)


let l1 = List.init 50_000 (function l -> Random.int 10_000);;

   
(*qsort2 es más lento que qsort1 porque las funciones recursivas terminales se comportan como bucles de tamaño infinito. Por tanto, cuando la lista es muy larga salta Stack overflow y continua con la ejecución*)
(*qsort1 = 0.1537299*)   
(*qsort2 = 0.1553400*) 
(*Diferencia: 1.03%*)
   
   
   
   
   
   
   

