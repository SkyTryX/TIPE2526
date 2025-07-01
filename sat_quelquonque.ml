type var = int;;
type literal = V of var | NV of var;;
type clause = literal list;;
type cnf = clause list;;

type proposition =
  | Var of var
  | Not of proposition
  | And of proposition * proposition
  | Or of proposition * proposition

let rec to_literal prop =
    match prop with
    | Var v -> V v
    | Not (Var v) -> NV v
    | _ -> failwith "Unsupported proposition type"
  
let rec distribute_or_over_and p1 p2 =
    match (p1, p2) with
    | (And (a1, a2), _) -> And (distribute_or_over_and a1 p2, distribute_or_over_and a2 p2)
    | (_, And (b1, b2)) -> And (distribute_or_over_and p1 b1, distribute_or_over_and p1 b2)
    | _ -> Or (p1, p2)
  
let rec to_cnf prop =
    match prop with
    | Not (Or (p1, p2)) -> And (to_cnf (Not p1), to_cnf (Not p2))  (* Loi de De Morgan *)
    | Not (And (p1, p2)) -> Or (to_cnf (Not p1), to_cnf (Not p2))  (* Loi de De Morgan *)
    | And (p1, p2) -> And (to_cnf p1, to_cnf p2)
    | Or (p1, p2) -> distribute_or_over_and (to_cnf p1) (to_cnf p2)
    | Var v -> Var v
    | Not v -> Not (to_cnf v)
  
let rec clause_from_proposition prop =
    match prop with
    | Var v -> [V v]
    | Not (Var v) -> [NV v]
    | _ -> failwith "Unsupported proposition for clause conversion"
  
let rec cnf_from_proposition prop =
    match to_cnf prop with
    | And (p1, p2) -> (cnf_from_proposition p1) @ (cnf_from_proposition p2)
    | Or (p1, p2) -> [clause_from_proposition p1 @ clause_from_proposition p2]
    | Var v -> [[V v]]
    | Not (Var v) -> [[NV v]]
    | _ -> failwith "Unsupported proposition for CNF conversion"
  

let litt_of_name_and_bool x b =
  if b then V x else NV x;;

let printclause c=
  Printf.printf "[";
  let rec aux = function
      [] ->  Printf.printf "]";
    | l::q -> match l with
      | NV x ->  Printf.printf "NV %d; " x; aux q;
      | V x -> Printf.printf "V %d; " x; aux q;
  in aux c
;;

let printprop p =  Printf.printf "prop = [";
  let rec aux = function
      [] ->  Printf.printf "]\n";
    | c::q -> printclause c; aux q;
  in aux p;;

let delete_lit (c:clause) (x:int) (b:bool) =
  List.filter
    ((<>) @@ litt_of_name_and_bool x (not b))
    c;;

let rec subst (f:cnf) (x:int) (b:bool) : cnf option  =
  match f with
    [] -> Some []
  | c::q -> let q' =  subst q x b in
            match q' with
            | None -> None
            | Some q2 -> let c = delete_lit c x b in
                         if c = [] then None else
                           if List.mem (litt_of_name_and_bool x b) c
                           then Some q2
                           else Some (c::q2);;

let get_var (f:cnf) : var = match f with 
  | (V x::_)::_ | (NV x::_)::_ -> x
  | _ -> failwith "pas de variable";;

let rec quine f = match f with
  | [] -> true
  | _ -> let x = get_var f in
    (*totest : substitution partielle*)
    let rec aux f totest =   match totest with
      | [] -> false (*on a testé x<-T et x<-F sans succès*)
      | b::q -> let sf = subst f x b (*x<-b*)
        in match sf with
        | Some f2 ->
           if quine f2
           then true (*un choix de substitution donne true*)
           else aux f q (*x<-b marche pas,on essaye le suivant*)
        | None -> aux f q
    in aux f [true;false];;

let sat (f:proposition) : bool = quine @@ cnf_from_proposition f;;

sat (And(Not(Var 1), Var 1));; (* false *)
sat (And(Not(Var 1), Var 2));; (* true *)
sat (And(Not(Var 1), Or(Not(Var 2),  Var 1)));; (* true *)
sat (And(Not(Var 1), Or(Var 1,  Var 1)));; (* false *)
sat (And(Var 1, Or(Var 1,  And(Not(Var 3), Var 3))));; (* true *)
sat (And(Var 1, Or(Not(Var 1),  And(Var 1, Var 2))));; (* true *)
sat (And(Not(Var 1), Or(Var 1,  And(Not(Var 2), Var 2))));; (* false *)
sat (And(Not(Var 1), Or(Var 1,  And(Var 1, Not(Var 1)))));; (* false *)