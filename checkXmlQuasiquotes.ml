(* check that:
    - XML and page quasiquotes don't contain formlet bindings
    - XML and formlet quasiquotes don't contain formlet or page placements *)

(* check an individual quasiquote *)
let check mode pos e =
  let checker =
    object (o)
      inherit SugarTraversals.predicate as super

      val error = None

      method get_error = error

      method satisfied =
        match error with
        | None   -> true
        | Some _ -> false
      
      method phrase = fun ((e, pos) as phrase) ->
        match e with
        | `FormBinding _ ->
          if mode <> `Formlet then
            {< error = Some (`FormletBinding, pos) >}
          else
            super#phrase phrase
        | `FormletPlacement _ ->
          if mode <> `Page then
            {< error = Some (`FormletPlacement, pos) >}
          else
            super#phrase phrase
        | `PagePlacement _ ->
          if mode <> `Page then
            {< error = Some (`PagePlacement, pos) >}
          else
            super#phrase phrase
        | `Xml _ ->
          super#phrase phrase
        | e -> o
    end
  in
  let o = checker#phrase (e, pos) in
  let kind =
    match mode with
    | `Xml -> "XML"
    | `Formlet -> "formlet"
    | `Page -> "page"
  in
    match o#get_error with
    | None -> ()
    | Some (`FormletBinding, pos') ->
      let (_, _, expr) = SourceCode.resolve_pos pos' in
        raise (Errors.SugarError (pos, "Formlet binding " ^ expr ^ " in " ^ kind ^ " quasiquote"))
    | Some (`FormletPlacement, pos') ->
      let (_, _, expr) = SourceCode.resolve_pos pos' in
        raise (Errors.SugarError (pos, "Formlet placement " ^ expr ^ " in " ^ kind ^ " quasiquote"))
    | Some (`PagePlacement, pos') ->
      let (_, _, expr) = SourceCode.resolve_pos pos' in
        raise (Errors.SugarError (pos, "Page placement " ^ expr ^ " in " ^ kind ^ " quasiquote"))

let checker =
object (o : 'self)
  inherit SugarTraversals.fold as super

  val mode = `Exp

  method phrase = fun ((e, pos) as phrase) ->
    match e with
    | `Xml _ when mode = `Quasi ->
      super#phrase phrase
    | `Xml _ when mode = `Exp ->
      check `Xml pos e;
      {< mode = `Quasi >}#phrase phrase
    | `Formlet ((body, _) as body', yields) when mode = `Exp->
      check `Formlet pos body;
      let _ = {< mode = `Quasi >}#phrase body' in
        super#phrase yields
    | `Page ((body, _) as body') when mode = `Exp ->
      check `Page pos body;
      {< mode = `Quasi >}#phrase body'
    | (`Formlet _ | `Page _) when mode = `Quasi ->
      raise (Errors.SugarError (pos, "Malformed quasiquote (internal error)"))
    | _ when mode = `Quasi ->
      {< mode = `Exp >}#phrase phrase
    | _ when mode = `Exp ->
      super#phrase phrase
end
