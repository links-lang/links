open Utility
open Operators
open SourceCode.WithPos
open Sugartypes
open SugarConstructors.DummyPositions

(* String constants used in regular expressions.  These should probably be
   reused in linksregexes.ml *)
let range_str        = "Range"
let simply_str       = "Simply"
let quote_str        = "Quote"
let any_str          = "Any"
let start_anchor_str = "StartAnchor"
let end_anchor_str   = "EndAnchor"
let seq_str          = "Seq"
let alternative_str  = "Alternate"
let group_str        = "Group"
let repeat_str       = "Repeat"
let replace_str      = "Replace"
let star_str         = "Star"
let plus_str         = "Plus"
let question_str     = "Question"

let desugar_regex phrase regex_type repeat_type regex : phrase =
  (* Desugar a regex, making sure that only variables are embedded
     within.  Any expressions that are spliced into the regex must be
     let-bound beforehand.  *)
  let constructor' ?body name = constructor name ?body ~ty:regex_type in
  let desugar_repeat : Regex.repeat -> phrase = function
    | Regex.Star      -> constructor star_str ~ty:repeat_type
    | Regex.Plus      -> constructor plus_str ~ty:repeat_type
    | Regex.Question  -> constructor question_str ~ty:repeat_type in
  let exprs = ref [] in
  let expr e =
    let (_, e, t) = phrase e in
    let v = Utility.gensym ~prefix:"_regex_" () in
      begin
        exprs := (v, e, t) :: !exprs;
        var v
      end in
  let rec aux : regex -> phrase =
    function
      | Range (f, t) ->
        constructor' ~body:(tuple [constant_char f; constant_char t]) range_str
      | Simply s           -> constructor' simply_str ~body:(constant_str s)
      | Quote s            -> constructor' quote_str  ~body:(aux s)
      | Any                -> constructor' any_str
      | StartAnchor        -> constructor' start_anchor_str
      | EndAnchor          -> constructor' end_anchor_str
      | Seq rs             ->
        constructor' seq_str ~body:(list ~ty:regex_type
                                         (List.map (fun s -> aux s) rs))
      | Alternate (r1, r2) ->
        constructor' alternative_str ~body:(tuple [aux r1; aux r2])
      | Group s ->
        constructor' group_str ~body:(aux s)
      | Repeat (rep, r) ->
        constructor' repeat_str ~body:(tuple [desugar_repeat rep; aux r])
      | Splice e ->
        constructor' quote_str ~body:(constructor' ~body:(expr e) simply_str)
      | Replace (re, (Literal tmpl)) ->
        constructor' replace_str ~body:(tuple [aux re; constant_str tmpl])
      | Replace (re, (SpliceExpr e)) ->
         constructor' replace_str ~body:(tuple [aux re; expr e])
  in block (List.map (fun (v, e1, t) ->
                val_binding (variable_pat ~ty:t v) e1) !exprs,
            aux regex)

let desugar_regexes env =
object(self)
  inherit (TransformSugar.transform env) as super

  val regex_type = Instantiate.alias "Regex" [] env.Types.tycon_env
  val repeat_type = Instantiate.alias "Repeat" [] env.Types.tycon_env

  method! phrase ({node=p; pos} as ph) = match p with
    | InfixAppl ((tyargs, BinaryOp.RegexMatch flags), e1, {node=Regex((Replace(_,_) as r)); _}) ->
        let libfn =
          if List.exists ((=)RegexNative) flags
          then "sntilde"
          else "stilde" in
          self#phrase (fn_appl libfn tyargs
                            [e1; desugar_regex self#phrase regex_type repeat_type r])
    | InfixAppl ((tyargs, BinaryOp.RegexMatch flags), e1, {node=Regex r; _}) ->
        let nativep = List.exists ((=) RegexNative) flags
        and listp   = List.exists ((=) RegexList)   flags in
        let libfn = match listp, nativep with
          | true, true   -> "lntilde"
          | true, false  -> "ltilde"
          | false, false -> "tilde"
          | false, true  -> "ntilde" in
          self#phrase (fn_appl libfn tyargs
                            [e1; desugar_regex self#phrase regex_type repeat_type r])
    | InfixAppl ((_tyargs, BinaryOp.RegexMatch _), _, _) ->
        let (_, expr) = SourceCode.Position.resolve_start_expr pos in
        let message = "Unexpected RHS of regex operator: " ^ expr in
        raise (Errors.internal_error ~filename:"desugarRegexes.ml" ~message)
    | _ -> super#phrase ph
end

let has_no_regexes =
object
  inherit SugarTraversals.predicate

  val no_regexes = true
  method satisfied = no_regexes
  method! regex _ = {< no_regexes = false >}
end

module Typeable
  = Transform.Typeable.Make(struct
        let name = "regexes"
        let obj env = (desugar_regexes env : TransformSugar.transform :> Transform.Typeable.sugar_transformer)
      end)
