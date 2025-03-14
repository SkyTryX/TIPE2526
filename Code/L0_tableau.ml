type formula = 
  Literal | 
  And of formula * formula | 
  Or of formula * formula |
  Not of formula |
  Implies of formula * formula |
  Equivalent of formula * formula;;

type 'a tree = Nil | Node of 'a tree * 'a array * 'a tree;;

(* Fonction qui a une formule avec des hypothèses associe son arbre de décision *)
let rec formula2tree (f: formula) (hyp: formula array) : formula tree = match f with
  | Literal -> Nil  
  | Not(Literal) -> Nil
  | Or(a, b) -> Node(formula2tree a hyp, hyp, formula2tree b hyp)
  | And(a,b) -> Nil
  | Implies(a,b) -> Node(formula2tree (Not(a)) hyp, hyp, formula2tree b hyp)
  | Equivalent(a,b) -> Nil
  | Not(Not(a)) -> Node(formula2tree a hyp, hyp, Nil)
  | Not(Or(a, b)) -> Nil
  | Not(And(a,b)) -> Node(formula2tree (Not(a)) hyp, hyp, formula2tree (Not(b)) hyp)
  | Not(Implies(a,b)) -> Nil
  | Not(Equivalent(a,b)) -> Nil

(* Fonction qui renvoie si un arbre de décision d'une formule est fermé *)
let rec tree_is_closed (t: formula tree) : bool = true;;

(* Fonction principale du proover *)
let proover (f : formula) (hyp: formula array) : bool = 
  let f_tree = formula2tree (Not(f)) hyp in tree_is_closed f_tree;;