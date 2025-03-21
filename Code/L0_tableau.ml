type formula = (* Type caractérisant une formule *)
  Literal of string | (* Un Litteral avec un nom qui le caractérise *)
  And of formula * formula | (* Conjonction de formule *)
  Or of formula * formula | (* Disjonction de formule *)
  Not of formula;; (* Negation de formule *)

type tableau = Nil of formula | Node of tableau * formula array * tableau;;

let implies (a:formula) (b:formula) = Or(Not(a), b);;
let equivalent (a:formula) (b:formula) = And(implies a b, implies b a);;
(* Pour permettre l'utilisation d'équivalence et d'implication dans les formules *)

(* Fonction qui a une formule avec des hypothèses associe son arbre de décision *)
let rec formula2tree_nohyp (f: formula) : tableau = match f with
  | Literal(name) -> Nil(Literal(name))
  | Not(Literal(name)) -> Nil(Not(Litteral(name)))
  | Or(a, b) -> Node(formula2tree a, )
  | And(a,b) -> ()
  | Not(Not(a)) -> ()
  | Not(Or(a, b)) -> ()
  | Not(And(a,b)) -> ()

(* Fonction qui renvoie si un arbre de décision d'une formule est fermé *)
let tree_has_cycle (t: tableau) : bool = 
  let rec aux (t:tableau) (acc:array formula) = match t with
in aux t [||];;

(* Fonction principale du proover *)
let proover (f : formula) (hyp: formula array) : bool = 
  let f_tree = formula2tree (Not(f)) hyp in tree_has_cycle f_tree;;