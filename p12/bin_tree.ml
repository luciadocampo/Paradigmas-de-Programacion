type 'a bin_tree =
    Empty
  | Node of 'a * 'a bin_tree * 'a bin_tree;;

let rec fold_tree f a = function
    Empty -> a
  | Node (x, l, r) -> f x (fold_tree f a l) (fold_tree f a r);;


let sum tree =
	let auxsum x l r = x+l+r in
	    fold_tree auxsum 0 tree;;


let prod tree = fold_tree (fun x l r -> x *. l *. r) 1.0 tree;;


let size tree = 
    let auxsum x l r = 1+l+r in
        fold_tree auxsum 0 tree;;


let inorder tree = fold_tree (fun x l r -> l@[x]@r) [] tree;;


let mirror tree = fold_tree (fun x y z -> Node(x,y,z)) Empty tree;;
