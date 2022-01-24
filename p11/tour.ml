(* tour : int -> int -> (int * int) -> (int * int) -> (int * int) list *)

open List;;

let possibleMoves =
    [(1,2); (1,-2); (2,1); (2,-1); (-1,2); (-1,-2); (-2,1); (-2,-1)];;


let addPos (a,b) (c,d) = (a+c,b+d);;


let getAllPos i j =
    let rec aux pos sol = if (pos = length possibleMoves)
        then sol
        else aux (pos+1) ((addPos (nth possibleMoves pos) (i,j))::sol)
    in aux 0 [];;

let filter_values n m l =
    filter (fun (i,j) -> 1 <= i && i <= n && 1 <= j && j <= m) l;;
    

let filter_visited visited l =
    let rec aux l sol = match l with
        [] -> sol
       |h::t -> if mem h visited
            then aux t sol
            else aux t (h::sol)
    in aux l [];;

let get_posibilities visited n m x y = (filter_visited visited (filter_values n m (getAllPos x y)));;


let tour n m (fi, ci) (ff, cf) =
    let rec aux visited (x,y) last_posibilities = if (x,y) = (ff,cf)
        then rev ((ff,cf)::visited)
        else
            let visited2 = ((x,y)::visited) in
            let posibilities = get_posibilities visited2 n m x y
            in if posibilities = []
                then if last_posibilities = []
                    then raise Not_found (*si no hubiera camino*)
                    else aux visited (hd last_posibilities) (tl last_posibilities)
                else aux visited2 (hd posibilities) (tl posibilities)
    in aux [] (fi,ci) [];;




