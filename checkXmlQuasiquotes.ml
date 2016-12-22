(* check that:
    - XML and page quasiquotes don't contain formlet bindings
    - XML and formlet quasiquotes don't contain formlet or page placements *)

(* check an individual quasiquote *)
let check mode pos e =
  let checker =
    object (o)
      inherit SugarTraversals.fold as super

      val error = None

      method get_error = error

      method! phrase = fun ((e, pos) as phrase) ->
        match e with
        | `Xml (_, _, _, children) ->
          o#list (fun o -> o#phrase) children
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
        | _ -> o
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

(* traverse a whole tree searching for and then checking quasiquotes *)
let checker =
object (o)
  inherit SugarTraversals.fold as super

  (* In expression mode we're looking for a quasiquote.
     In quasiquote mode we're traversing a quasiquote. *)

  (* initially we're in expression mode *)
  val mode : [ `Exp | `Quasi ] = `Exp

  method set_mode new_mode = {< mode = new_mode >}
  method private phrase_with new_mode phrase =
    ((o#set_mode new_mode)#phrase phrase)#set_mode mode

  method! phrase = fun ((e, pos) as phrase) ->
    match e with
    | `Xml _ when mode = `Quasi ->
      super#phrase phrase
    | `Xml _ when mode = `Exp ->
      check `Xml pos e;
      o#phrase_with `Quasi phrase
    | `Formlet ((body, _) as body', yields) when mode = `Exp ->
      check `Formlet pos body;
      (o#phrase_with `Quasi body')#phrase yields
    | `Page ((body, _) as body') when mode = `Exp ->
      check `Page pos body;
      o#phrase_with `Quasi body'
    | (`Formlet _ | `Page _) when mode = `Quasi ->
      (* The parser should prevent this from ever happening *)
      raise (Errors.SugarError (pos, "Malformed quasiquote (internal error)"))
    | _ when mode = `Quasi ->
      o#phrase_with `Exp phrase
    | _ when mode = `Exp ->
      super#phrase phrase
    | _ -> assert false
end
