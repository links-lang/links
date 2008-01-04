open Sugartypes

let desugar_repeat _ : Regex.repeat -> phrasenode = function
  | Regex.Star      -> `ConstructorLit ("Star", None)
  | Regex.Plus      -> `ConstructorLit ("Plus", None)
  | Regex.Question  -> `ConstructorLit ("Question", None)
      
let desugar_regex pos : regex -> phrasenode = 
  (* Desugar a regex, making sure that only variables are embedded
     within.  Any expressions that are spliced into the regex must be
     let-bound beforehand.  *)
  let exprs = ref [] in
  let expr e = 
    let v = Utility.gensym ~prefix:"_regex_" () in
      begin
        exprs := (v, e) :: !exprs;
        `Var v, pos
      end in
  let rec aux : regex -> phrasenode = 
    function
      | `Range (f, t)    -> `ConstructorLit ("Range", Some (`TupleLit [`Constant (`Char f), pos; `Constant (`Char t), pos], pos))
      | `Simply s        -> `ConstructorLit ("Simply", Some (`Constant (`String s), pos))
      | `Quote s        -> `ConstructorLit ("Quote", Some (aux s, pos))
      | `Any             -> `ConstructorLit ("Any", None)
      | `StartAnchor   -> `ConstructorLit ("StartAnchor", None)
      | `EndAnchor     -> `ConstructorLit ("EndAnchor", None)
      | `Seq rs          -> `ConstructorLit ("Seq", Some (`ListLit (List.map (fun s -> aux s, pos) 
                                                                      rs), pos))
      | `Alternate (r1, r2)  -> `ConstructorLit ("Alternate",  Some (`TupleLit [aux r1, pos; aux r2, pos], pos))
      | `Group s          -> `ConstructorLit ("Group", Some (aux s, pos))
      | `Repeat (rep, r) -> `ConstructorLit ("Repeat", Some (`TupleLit [desugar_repeat pos rep, pos; 
                                                                        aux r, pos], pos))
      | `Splice e        -> `ConstructorLit ("Quote", Some(`ConstructorLit ("Simply", Some (expr e)), pos))
      | `Replace (re, (`Literal tmpl)) -> `ConstructorLit("Replace", Some(`TupleLit ([(aux re, pos); (`Constant (`String tmpl), pos)]), pos))
      | `Replace (re, (`Splice e)) -> `ConstructorLit("Replace", Some(`TupleLit ([(aux re, pos); expr e]), pos))
  in fun e ->
    let e = aux e in
      `Block (List.map (fun (v, e1) -> (`Val ((`Variable (v, None,pos), pos), e1, `Unknown, None), pos)) !exprs,
              (e, pos))

let appl pos name args = 
  (`FnAppl ((`Var name, pos), args), pos : phrase)

let desugar_regexes = 
object
  inherit SugarTraversals.map as super

  method phrase (p, pos) = match p with
    | `InfixAppl (`RegexMatch flags, e1, (`Regex((`Replace(_,_) as r)), _)) -> 
	let libfn = 
	  if List.exists ((=)`RegexNative) flags
          then "sntilde" 
          else "stilde" in
          appl pos libfn [e1; (desugar_regex pos r, pos)]
    | `InfixAppl (`RegexMatch flags, e1, (`Regex r, _)) -> 
	let nativep = List.exists ((=) `RegexNative) flags 
        and listp   = List.exists ((=) `RegexList)   flags in
	let libfn = match listp, nativep with
          | true, true   -> "lntilde"
          | true, false  -> "ltilde"
          | false, false -> "tilde"
          | false, true  -> "ntilde" in
          appl pos libfn [e1; (desugar_regex pos r, pos)]
    | `InfixAppl (`RegexMatch _, _, _) -> 
        raise (ConcreteSyntaxError ("Internal error: unexpected rhs of regex operator",
                                    pos))
    | p -> super#phrase (p, pos)
               

end

let has_no_regexes =
object (self)
  inherit SugarTraversals.predicate as super

  val no_regexes = true
  method satisfied = no_regexes
  method regex _ = {< no_regexes = false >}
end

