
type 'a boolFormula =
	  True
	| False
	| Varb of 'a
	| Not of 'a boolFormula
	| Ou of 'a boolFormula * 'a boolFormula
	| Et of 'a boolFormula * 'a boolFormula
	| Im of 'a boolFormula * 'a boolFormula
	| OuN of 'a boolFormula list
	| EtN of 'a boolFormula list

val nooptim: bool ref 

val select : (int * int) * Ast.pexpr * (Ast.ppat * Ast.pexpr)list -> Ast.pexpr 
val selectOpt : (int * int) * Ast.pexpr * (Ast.ppat * Ast.pexpr)list * Ast.pexpr boolFormula -> Ast.pexpr 
val ast_of_bool : Ast.pexpr boolFormula * (int * int ) -> Ast.pexpr 


