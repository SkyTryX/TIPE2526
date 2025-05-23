type formula = (* Definition d'un type simple de formule logique *)
  | Atom of string
  | Not of formula
  | And of formula * formula
  | Or of formula * formula

let rec print_formula (f:formula) = match f with
  | Atom(s) -> print_string s
  | Not f -> print_string "Not("; print_formula f;print_string ")"
  | And (f,g) -> print_formula f;print_string " And ";print_formula g
  | Or (f,g) -> print_formula f;print_string " Or ";print_formula g;;

let rec print_branches (b:formula list list) =
  print_string "[";
  List.iter (fun y -> print_string "[";List.iter (fun x -> print_formula x; print_string ", ";) y;print_string "]";) b;
  print_string "]";;

(* On utilise les règles de la méthode des tableaux *)
let expand formula = (* O(1) *)
  match formula with
  | Not (Not f) -> [[f]]
  | Not (And (f1, f2)) -> [[Not f1]; [Not f2]]
  | Not (Or (f1, f2)) -> [[Not f1; Not f2]]
  | And (f1, f2) -> [[f1; f2]]
  | Or (f1, f2) -> [[f1]; [f2]]
  | _ -> [];;

(* On regarde si il existe un cycle i.e une contradiction *)
let has_cycle branch =
  List.exists (fun f -> List.mem (Not f) branch) branch;;

let rec tableau branches =
  match branches with
  | [] -> false (* Toutes les branches sont fermés *)
  | branch :: rest -> 

  if has_cycle branch then (* Si la branche selectionné a un cycle *)
    tableau rest (* On vérifie le reste *)
  else 
    (* Sinon, on la developpe *)
    match branch with
    | [] -> true (* Une branche non fermé pas developpable, c'est gagné! *)
    | f :: fs ->
    
    let expansions = expand f in match expansions with
    | [] -> if List.exists (fun y -> match y with | Atom _ -> false | _ -> true) fs then
        tableau ((fs@[f]) :: rest)
        else 
          tableau (fs :: rest)
    | new_branches ->
      
    let expanded_branches = List.map (fun b -> b @ fs) new_branches in
    tableau (expanded_branches @ rest);; (* On appelle recursivement sur les versions developpés des branches*)
   

let is_satisfiable formula =
  let initial_branch = [formula] in tableau [initial_branch];;

let f = And(Atom "A", Or(Not (Atom "A"), Not(Atom "A")))in is_satisfiable f;;
let f = And(Atom "A", Or(Not (Atom "A"), And(Not(Atom "D"), Atom "D")))in is_satisfiable f;;
let f = And(Atom "D", Or(Not (Atom "A"), And(Not(Atom "D"), Atom "D")))in is_satisfiable f;;


let f = And(Atom "A", Or(Not (Atom "A"), Not(Atom "A"))) in is_satisfiable f;;
