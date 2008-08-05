open Sugartypes

let desugar_repeat : Regex.repeat -> phrasenode = function
  | Regex.Star      -> `ConstructorLit ("Star"     , None, None)
  | Regex.Plus      -> `ConstructorLit ("Plus"     , None, None)
  | Regex.Question  -> `ConstructorLit ("Question" , None, None)
      
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
      | `Range (f, t)       -> `ConstructorLit ("Range", Some (`TupleLit [`Constant (`Char f), pos;
                                                                          `Constant (`Char t), pos], pos), None)
      | `Simply s           -> `ConstructorLit ("Simply", Some (`Constant (`String s), pos), None)
      | `Quote s            -> `ConstructorLit ("Quote", Some (aux s, pos), None)
      | `Any                -> `ConstructorLit ("Any", None, None)
      | `StartAnchor        -> `ConstructorLit ("StartAnchor", None, None)
      | `EndAnchor          -> `ConstructorLit ("EndAnchor", None, None)
      | `Seq rs             -> `ConstructorLit ("Seq", Some (`ListLit (List.map (fun s -> aux s, pos) rs, None),
                                                             pos), None)
      | `Alternate (r1, r2) -> `ConstructorLit ("Alternate",  Some (`TupleLit [aux r1, pos; aux r2, pos], pos), None)
      | `Group s            -> `ConstructorLit ("Group", Some (aux s, pos), None)
      | `Repeat (rep, r)    -> `ConstructorLit ("Repeat", Some (`TupleLit [desugar_repeat rep, pos; 
                                                                        aux r, pos], pos), None)
      | `Splice e           -> `ConstructorLit ("Quote", Some(`ConstructorLit ("Simply", Some (expr e), None), pos), None)
      | `Replace (re, (`Literal tmpl)) -> `ConstructorLit("Replace", Some (`TupleLit ([(aux re, pos); 
                                                                                      (`Constant (`String tmpl), pos)]),
                                                                          pos), None)
      | `Replace (re, (`Splice e)) -> `ConstructorLit("Replace", Some (`TupleLit ([(aux re, pos); expr e]), pos), None)
  in fun e ->
    let e = aux e in
      `Block (List.map (fun (v, e1) -> (`Val ([], (`Variable (v, None, pos), pos), e1, `Unknown, None), pos)) !exprs,
              (e, pos))

let appl pos name tyargs args = 
  (`FnAppl ((tappl (`Var name, tyargs), pos), args), pos : phrase)

let desugar_regexes = 
object(self)
  inherit SugarTraversals.map as super

  method phrase (p, pos) = match p with
    | `InfixAppl ((tyargs, `RegexMatch flags), e1, (`Regex((`Replace(_,_) as r)), _)) -> 
        let libfn = 
          if List.exists ((=)`RegexNative) flags
          then "sntilde" 
          else "stilde" in
          self#phrase (appl pos libfn tyargs [e1; (desugar_regex pos r, pos)])
    | `InfixAppl ((tyargs, `RegexMatch flags), e1, (`Regex r, _)) -> 
        let nativep = List.exists ((=) `RegexNative) flags 
        and listp   = List.exists ((=) `RegexList)   flags in
        let libfn = match listp, nativep with
          | true, true   -> "lntilde"
          | true, false  -> "ltilde"
          | false, false -> "tilde"
          | false, true  -> "ntilde" in
          self#phrase (appl pos libfn tyargs [e1; (desugar_regex pos r, pos)])
    | `InfixAppl ((_tyargs, `RegexMatch _), _, _) -> 
        raise (ConcreteSyntaxError ("Internal error: unexpected rhs of regex operator",
                                    pos))
    | p -> super#phrase (p, pos)
               

end

let has_no_regexes =
object
  inherit SugarTraversals.predicate

  val no_regexes = true
  method satisfied = no_regexes
  method regex _ = {< no_regexes = false >}
end

