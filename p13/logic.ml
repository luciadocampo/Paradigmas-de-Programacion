type log_exp =
    Const of bool
  | Var of string
  | Neg of log_exp
  | Disj of log_exp * log_exp
  | Conj of log_exp * log_exp
  | Cond of log_exp * log_exp
  | BiCond of log_exp * log_exp;;
(* BiCond (Cond (Var "p", Var "q"), Disj (Neg (Var "p"), Var "q")) *)


let rec eval ctx = function
    Const b -> b
  | Var s -> List.assoc s ctx
  | Neg e -> not (eval ctx e)
  | Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
  | Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
  | Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
  | BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2);;
(* val eval : (string * bool) list -> log_exp -> bool = <fun> *)


type oper = Not;;


type biOper = Or | And | If | Iff;;


type prop =
    C of bool
  | V of string
  | Op of oper * prop
  | BiOp of biOper * prop * prop;;
  
  
  
(* A) *)


let rec prop_of_log_exp = function
    Const x -> C x
  | Var x -> V x
  | Neg x -> Op (Not,(prop_of_log_exp x))
  | Disj (x,y) -> BiOp (Or,(prop_of_log_exp x),(prop_of_log_exp y))
  | Conj (x,y) -> BiOp (And,(prop_of_log_exp x),(prop_of_log_exp y))
  | Cond (x,y) -> BiOp (If,(prop_of_log_exp x),(prop_of_log_exp y))
  | BiCond (x,y) -> BiOp (Iff,(prop_of_log_exp x),(prop_of_log_exp y));;

(* Comprobación: *)  
let e = BiCond (Cond (Var "p", Var "q"), Disj (Neg (Var "p"), Var "q"));;
prop_of_log_exp e;;



let rec log_exp_of_prop = function
    C x -> Const x
  | V x -> Var x
  | Op (Not,x) -> Neg (log_exp_of_prop x)
  | BiOp (Or,x,y) -> Disj (log_exp_of_prop x,log_exp_of_prop y)
  | BiOp (And,x,y) -> Conj (log_exp_of_prop x,log_exp_of_prop y)
  | BiOp (If,x,y) -> Cond (log_exp_of_prop x,log_exp_of_prop y)
  | BiOp (Iff,x,y) -> BiCond (log_exp_of_prop x,log_exp_of_prop y);;
  
(* Comprobación: *)  
let e2 = BiOp (Iff, BiOp (If, V "p", V "q"), BiOp (Or, Op (Not, V "p"), V "q"));;
log_exp_of_prop e2;;




(*  B) *)

let opval = function
    Not -> not;;

    
    
let biopval = function
    Or -> (||)
  | And -> (&&)
  | If -> fun p q -> (not p) || q
  | Iff -> (=);;

  
  

let rec peval ctx = function
    C b -> b
  | V s -> List.assoc s ctx
  | Op (op,p) -> (opval op) (peval ctx p)
  | BiOp (biop, p1, p2) -> (biopval biop) (peval ctx p1) (peval ctx p2);;
  
(* Comprobación *)
peval [("p",false); ("q",false)] e2;;
peval [("p",false); ("q",true)] e2;;
peval [("p",true); ("q",false)] e2;;
peval [("p",true); ("q",true)] e2;;
let e3 = BiOp (If, V "p", V "q");;
peval [("p",false); ("q",false)] e3;;
peval [("p",false); ("q",true)] e3;;
peval [("p",true); ("q",false)] e3;;
peval [("p",true); ("q",true)] e3;;




(*  C)  *)


let rec variables = function
    C _ -> []
  | V x -> [x]
  | Op (_, x) -> variables x
  | BiOp (_,x,y) -> variables x @ variables y ;;
 

let rec remove_repeated = function
    [] -> []
  | h::t -> if List.mem h t then remove_repeated t
            else h::(remove_repeated t);;
    
            

let rec asignar = function
    [] -> [ [] ]
   |h::t -> (List. map (function c -> (h,true)::c) (asignar t)) @ (List.map (function c -> (h,false)::c) (asignar t));;

asignar ["p";"q"];;
(* [[("p", true); ("q", true)]; [("p", true); ("q", false)];
 [("p", false); ("q", true)]; [("p", false); ("q", false)]] *)



let is_tau p =
    let valores = asignar (remove_repeated (variables p)) in List.for_all (function c -> peval c p) valores;;

is_tau e2;; (* es una Tautología*)
is_tau e3;; (* no es una Tautología*)

