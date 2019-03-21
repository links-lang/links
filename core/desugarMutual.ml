open Sugartypes
open Utility
open SourceCode

(* Desugars Mutual. *)
(*
 * mutual {
 *   typename Foo = ...;
 *   typename Bar = ...;
 *   fun foo() {
 *       ...
 *   }
 *
 *  fun bar() {
 *       ...
 *   }
 * } -->
 * Typenames [Foo; Bar]
 * Funs [foo; bar]
 *
 * Any other binding occurring in a mutual block is forbidden.
 *)

(* Simple check to see whether a function is recursive: checks whether the
 * binder occurs within the free variables of the function body. *)
let is_recursive bnd fnlit =
  StringSet.mem (Binder.to_name bnd) (Freevars.funlit fnlit)


(* Fun bindings must be lifted into `Funs if they are recursive. This is
 * performed after the first pass. *)
let lift_funs =
object ((self : 'self_type))
    inherit SugarTraversals.map as super
    method! binding = fun b ->
      let position = WithPos.pos b in
      match WithPos.node b with
      |  Fun (bndr, lin, (tvs, fnlit), location, dt) ->
          let fnlit = self#funlit fnlit in
          if is_recursive bndr fnlit then
            WithPos.make ~pos:position (Funs [(bndr, lin, ((tvs, None), fnlit), location, dt, position)])
          else
            WithPos.make ~pos:position (Fun (bndr, lin, (tvs, fnlit), location, dt))
      | _ -> super#binding b
end

(*
let check_dups funs tys =
  (* Check to see whether there are any duplicate names, and report
   * an error if so. *)
  let assoc_to_list_map get_name xs =
    List.fold_left (fun acc (x, pos) ->
      let name = get_name x in
      StringMap.update name (fun x_opt ->
        OptionUtils.opt_app
          (fun positions -> Some (pos :: positions))
          (Some [pos]) x_opt) acc) StringMap.empty xs in

  let funs_map =
    assoc_to_list_map
      (fun (bndr, _, _, _, _) -> Binder.to_name bndr) funs in
  let tys_map = assoc_to_list_map (fst3) tys in
  let check map =
    let dups =
      StringMap.filter (fun _ poss -> List.length poss > 1) map in
    if StringMap.cardinal dups > 0 then
      raise (MultiplyDefinedMutualNames dups)
    else () in
  check funs_map; check tys_map

  let rec flatten_simple = fun () ->
  object(self)
    inherit SugarTraversals.map as super

    method flatten_bindings bs =
      ListUtils.concat_map
        (fun b -> ((flatten_bindings ())#binding b)#get_bindings) bs

    method flatten_block (bs, p) =
      let bs = self#flatten_bindings bs in
      let p = self#phrase p in
      (bs, p)

    method! phrasenode : phrasenode -> phrasenode = function
      | Block (bs, phr) ->
          let bs = self#flatten_bindings bs in
          let phr = self#phrase phr in
          Block (bs, phr)
      | x -> super#phrasenode x

    method! program : program -> program = fun (bs, phr) ->
      let bs = self#flatten_bindings bs in
      let phr = OptionUtils.opt_map (self#phrase) phr in
      (bs, phr)
  end
  and flatten_bindings = fun () ->
  let open WithPos in
  object (o: 'self_type')
    inherit SugarTraversals.fold
      val bindings = []
      method add_binding x = {< bindings = x :: bindings>}
      method get_bindings = List.rev bindings

      method! binding b =
      let mutual_pos = WithPos.pos b in
      match WithPos.node b with
        | Mutual [] -> assert false
        | Mutual bs ->
            (* Split into Funs and Types *)
            (* These come out of fold_left reversed: it doesn't really
             * matter, but they're reversed to save us confusion if we
             * end up debugging *)
            let (funs_rev, tys_rev) =
              List.fold_left (fun (funs, tys) x ->
                begin
                  match x with
                    | { node = (Fun (f1, f2, f3, f4, f5)); pos } ->
                        (((f1, f2, f3, f4, f5), pos) :: funs, tys)
                    | { node = Typenames [t]; pos } -> (funs, (t, pos) :: tys)
                    | { node = _; pos } -> raise (InvalidMutualBinding pos)
                end
              ) ([], []) bs in
            let (funs, tys) = (List.rev funs_rev, List.rev tys_rev) in
            (* Check for duplicates, and report an error if so. *)
            check_dups funs tys;
            (* Order matters here: the types must be in scope before the functions, otherwise
             * things get weird with alias preservation. *)
            let o =
              if not (ListUtils.empty tys) then
                o#add_binding (WithPos.make ~pos:mutual_pos (Typenames (List.map fst tys)))
              else o in
            (* We must take care to preserve non-recursive functions *)
            begin
            match funs with
              | [] -> o
              | [f] ->
                  let ((bnd, lin, (tvs, fl), loc, dt), pos) = f in
                  let fl = (flatten_simple())#funlit fl in
                  let f = Fun (bnd, lin, (tvs, fl), loc, dt) in
                  o#add_binding (WithPos.make ~pos f)
              | fs ->
                  let fs =
                    List.map (fun ((bnd, lin, (tvs, fl), loc, dt), pos) ->
                      let fl = (flatten_simple())#funlit fl in
                      (bnd, lin, ((tvs, None), fl), loc, dt, pos)
                    ) fs in
                  o#add_binding (WithPos.make ~pos:mutual_pos (Funs fs))
            end
        | _ -> o#add_binding ((flatten_simple ())#binding b)
  end
  let desugar_mutual = (flatten_simple() :> SugarTraversals.map)
  *)
