type symbol = 
  Const of bool | 
  And of symbol * symbol | 
  Or of symbol * symbol |
  Not of symbol |
  Implies of symbol * symbol |
  Equivalent of symbol * symbol;;

type 'a tree = Nil | Node of 'a tree * 'a * 'a tree;;

