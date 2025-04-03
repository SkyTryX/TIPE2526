type formula = (* Definition d'un type simple de formule logique *)
  | Atom of string
  | Not of formula
  | And of formula * formula
  | Or of formula * formula

(* On utilise les règles de la méthode des tableaux *)
let expand formula =
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
  else (* Sinon, on le developpe *)
    match branch with
    | [] -> true (* Une branche non fermé pas developpable, c'est gagné! *)
    | f :: fs ->

    let expansions = expand f in match expansions with
    | [] -> tableau (fs :: rest) (* rien n'a developpé, on continue *)
    | new_branches ->
      
    let expanded_branches = List.map (fun b -> b @ fs) new_branches in
    tableau (expanded_branches @ rest);; (* On appelle recursivement sur les versions developpés des branches*)

(* Petite fonction *)
let is_satisfiable formula =
  let initial_branch = [formula] in tableau [initial_branch];;
  
let f = And (And(Atom "P", Not(Atom "P")), Atom "Q") in
is_satisfiable f;;