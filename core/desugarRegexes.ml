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
        with_pos pos (`Var v)
      end in
  let rec aux : regex -> phrasenode =
    function
      | `Range (f, t)       -> `ConstructorLit ("Range",
                                 Some (with_pos pos (`TupleLit [with_pos pos (`Constant (`Char f));
                                                                with_pos pos (`Constant (`Char t))])),
                                 Some regex_type)
      | `Simply s           -> `ConstructorLit ("Simply", Some (with_pos pos (`Constant (`String s))), Some regex_type)
      | `Quote s            -> `ConstructorLit ("Quote", Some (with_pos pos (aux s)), Some regex_type)
      | `Any                -> `ConstructorLit ("Any", None, Some regex_type)
      | `StartAnchor        -> `ConstructorLit ("StartAnchor", None, Some regex_type)
      | `EndAnchor          -> `ConstructorLit ("EndAnchor", None, Some regex_type)
      | `Seq rs             -> `ConstructorLit ("Seq", Some (with_pos pos (`ListLit (List.map (fun s -> with_pos pos (aux s)) rs,
                                                                                     Some (Types.make_list_type regex_type)))),
                                                Some regex_type)
      | `Alternate (r1, r2) -> `ConstructorLit ("Alternate",
                                 Some (with_pos pos (`TupleLit [ with_pos pos (aux r1)
                                                               ; with_pos pos (aux r2)])),
                                 Some regex_type)
      | `Group s            -> `ConstructorLit ("Group", Some (with_pos pos (aux s)), Some regex_type)
      | `Repeat (rep, r)    -> `ConstructorLit ("Repeat",
                                 Some (with_pos pos (`TupleLit [with_pos pos (desugar_repeat regex_type rep);
                                                                with_pos pos (aux r)])),
                                 Some regex_type)
      | `Splice e           -> `ConstructorLit ("Quote",
                                 Some (with_pos pos (`ConstructorLit ("Simply", Some (expr e), Some regex_type))),
                                 Some regex_type)
      | `Replace (re, (`Literal tmpl)) -> `ConstructorLit("Replace",
                                 Some (with_pos pos (`TupleLit ([with_pos pos (aux re);
                                                                 with_pos pos (`Constant (`String tmpl))]))),
                                 Some regex_type)
      | `Replace (re, (`Splice e)) -> `ConstructorLit("Replace",
                                 Some (with_pos pos (`TupleLit ([with_pos pos (aux re); expr e]))),
                                 Some regex_type)
  in fun e ->
     let e = aux e in
     `Block (List.map (fun (v, e1, t) ->
                 (with_pos pos (`Val (with_pos pos (`Variable (make_binder v t pos)), ([], e1), `Unknown, None))))
               !exprs,
             with_pos pos e)

let appl pos name tyargs args =
  with_pos pos (`FnAppl (with_pos pos (tappl (`Var name, tyargs)), args))

let desugar_regexes env =
object(self)
  (*  inherit SugarTraversals.map as super*)
  inherit (TransformSugar.transform env) as super

  val regex_type = Instantiate.alias "Regex" [] env.Types.tycon_env

  method! phrase ({node=p; pos} as ph) = match p with
    | `InfixAppl ((tyargs, `RegexMatch flags), e1, {node=`Regex((`Replace(_,_) as r)); _}) ->
        let libfn =
          if List.exists ((=)`RegexNative) flags
          then "sntilde"
          else "stilde" in
          self#phrase (appl pos libfn tyargs
                            [e1; {node=desugar_regex self#phrase regex_type pos r; pos}])
    | `InfixAppl ((tyargs, `RegexMatch flags), e1, {node=`Regex r; _}) ->
        let nativep = List.exists ((=) `RegexNative) flags
        and listp   = List.exists ((=) `RegexList)   flags in
        let libfn = match listp, nativep with
          | true, true   -> "lntilde"
          | true, false  -> "ltilde"
          | false, false -> "tilde"
          | false, true  -> "ntilde" in
          self#phrase (appl pos libfn tyargs
                            [e1; {node=desugar_regex self#phrase regex_type pos r; pos}])
    | `InfixAppl ((_tyargs, `RegexMatch _), _, _) ->
        raise (Errors.SugarError (pos, "Internal error: unexpected rhs of regex operator"))
    | _ -> super#phrase ph
end

let has_no_regexes =
object
  inherit SugarTraversals.predicate

  val no_regexes = true
  method satisfied = no_regexes
  method! regex _ = {< no_regexes = false >}
end

