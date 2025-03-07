type boolean = True | False

type formula = 
  Literal of boolean | 
  And of formula * formula | 
  Or of formula * formula |
  Not of formula |
  Implies of formula * formula |
  Equivalent of formula * formula;;

type 'a tree = Nil | Node of 'a tree * 'a array * 'a tree;;

(* Fonction qui a une formule avec des hypothèses associe son arbre de décision *)
let formula2tree (f: formula) (hyp: formula array) : formula tree = match f with
  | Literal(_) -> Nil  
  | Not(Literal(_)) -> Nil
  | Or(a, b) -> Node(formula2tree a hyp, hyp, formula2tree b hyp)
  | And(a,b) -> Nil
  | Implies(a,b) -> Node(formula2tree Not(a) hyp, hyp, formula2tree b hyp)
  | Equivalent(a,b) -> Nil
  | Not(Not(a)) -> Node(formula2tree a hyp, hyp, Nil)
  | Not(Or(a, b)) -> Nil
  | Not(And(a,b)) -> Node(formula2tree Not(a) hyp, hyp, formula2tree Not(b) hyp)
  | Not(Implies(a,b)) -> Nil
  | Not(Equivalent(a,b)) -> Nil