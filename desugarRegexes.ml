open Sugartypes

let desugar_repeat regex_type : Regex.repeat -> phrasenode = function
  | Regex.Star      -> `ConstructorLit ("Star"     , None, Some regex_type)
  | Regex.Plus      -> `ConstructorLit ("Plus"     , None, Some regex_type)
  | Regex.Question  -> `ConstructorLit ("Question" , None, Some regex_type)

let desugar_regex phrase regex_type pos : regex -> phrasenode =
  (* Desugar a regex, making sure that only variables are embedded
     within.  Any expressions that are spliced into the regex must be
     let-bound beforehand.  *)
  let exprs = ref [] in
  let expr e =
    let (_, e, t) = phrase e in
    let v = Utility.gensym ~prefix:"_regex_" () in
      begin
        exprs := (v, e, t) :: !exprs;
        `Var v, pos
      end in
  let rec aux : regex -> phrasenode =
    function
      | `Range (f, t)       -> `ConstructorLit ("Range", Some (`TupleLit [`Constant (`Char f), pos;
                                                                          `Constant (`Char t), pos], pos), Some regex_type)
      | `Simply s           -> `ConstructorLit ("Simply", Some (`Constant (`String s), pos), Some regex_type)
      | `Quote s            -> `ConstructorLit ("Quote", Some (aux s, pos), Some regex_type)
      | `Any                -> `ConstructorLit ("Any", None, Some regex_type)
      | `StartAnchor        -> `ConstructorLit ("StartAnchor", None, Some regex_type)
      | `EndAnchor          -> `ConstructorLit ("EndAnchor", None, Some regex_type)
      | `Seq rs             -> `ConstructorLit ("Seq", Some (`ListLit (List.map (fun s -> aux s, pos) rs,
                                                                       Some (Types.make_list_type regex_type)),
                                                             pos), Some regex_type)
      | `Alternate (r1, r2) -> `ConstructorLit ("Alternate",  Some (`TupleLit [aux r1, pos; aux r2, pos], pos), Some regex_type)
      | `Group s            -> `ConstructorLit ("Group", Some (aux s, pos), Some regex_type)
      | `Repeat (rep, r)    -> `ConstructorLit ("Repeat", Some (`TupleLit [desugar_repeat regex_type rep, pos;
                                                                        aux r, pos], pos), Some regex_type)
      | `Splice e           -> `ConstructorLit ("Quote", Some(`ConstructorLit ("Simply", Some (expr e), Some regex_type), pos), Some regex_type)
      | `Replace (re, (`Literal tmpl)) -> `ConstructorLit("Replace", Some (`TupleLit ([(aux re, pos);
                                                                                      (`Constant (`String tmpl), pos)]),
                                                                          pos), Some regex_type)
      | `Replace (re, (`Splice e)) -> `ConstructorLit("Replace", Some (`TupleLit ([(aux re, pos); expr e]), pos), Some regex_type)
  in fun e ->
    let e = aux e in
      `Block (List.map (fun (v, e1, t) -> (`Val ([], (`Variable (v, Some t, pos), pos), e1, `Unknown, None), pos)) !exprs,
              (e, pos))

let appl pos name tyargs args =
  (`FnAppl ((tappl (`Var name, tyargs), pos), args), pos : phrase)

let desugar_regexes env =
object(self)
  (*  inherit SugarTraversals.map as super*)
  inherit (TransformSugar.transform env) as super

  val regex_type = Instantiate.alias "Regex" [] env.Types.tycon_env

  method! phrase (p, pos) = match p with
    | `InfixAppl ((tyargs, `RegexMatch flags), e1, (`Regex((`Replace(_,_) as r)), _)) ->
        let libfn =
          if List.exists ((=)`RegexNative) flags
          then "sntilde"
          else "stilde" in
          self#phrase (appl pos libfn tyargs [e1; (desugar_regex self#phrase regex_type pos r, pos)])
    | `InfixAppl ((tyargs, `RegexMatch flags), e1, (`Regex r, _)) ->
        let nativep = List.exists ((=) `RegexNative) flags
        and listp   = List.exists ((=) `RegexList)   flags in
        let libfn = match listp, nativep with
          | true, true   -> "lntilde"
          | true, false  -> "ltilde"
          | false, false -> "tilde"
          | false, true  -> "ntilde" in
          self#phrase (appl pos libfn tyargs [e1; (desugar_regex self#phrase regex_type pos r, pos)])
    | `InfixAppl ((_tyargs, `RegexMatch _), _, _) ->
        raise (Errors.SugarError (pos, "Internal error: unexpected rhs of regex operator"))
    | p -> super#phrase (p, pos)
end

let has_no_regexes =
object
  inherit SugarTraversals.predicate

  val no_regexes = true
  method satisfied = no_regexes
  method! regex _ = {< no_regexes = false >}
end

