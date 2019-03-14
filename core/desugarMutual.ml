open Sugartypes
open Utility
open SourceCode
open Errors

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
          if is_recursive bndr fnlit then
            let fnlit = self#funlit fnlit in
            WithPos.make ~pos:position (Funs [(bndr, lin, ((tvs, None), fnlit), location, dt, position)])
          else
            let fnlit = self#funlit fnlit in
            WithPos.make ~pos:position (Fun (bndr, lin, (tvs, fnlit), location, dt))
      | _ -> super#binding b
end

(* Desugars Mutual blocks *)
(*
let desugar_sugarfuns =
object ((o : 'self_type))
  inherit SugarTraversals.map as super
  method! binding = fun b ->
    match WithPos.node b with
    (* Empty or singleton Mutual should not have been constructed. *)
    | Mutual [] -> assert false
    | Mutual [_] -> assert false
    | Mutual bs ->
        (* Recursively apply *)
        let bs = o#list (fun o -> o#binding) bs in
        (* All contained bindings must be of the form `Fun x. Extract these,
         * and turn them into the right form for Funs blocks. *)
        let fs =
          List.map (fun f ->
            match WithPos.node f with
            | Fun (bnd, lin, (tvs, fnlit), loc, dt) ->
                (bnd, lin, ((tvs, None), fnlit), loc, dt, WithPos.pos f)
            (* Due to parser construction and the fact we've desugared handlers
             * into `Funs already, something must have gone seriously wrong if
             * the block contains anything but `Funs. *)
            | _ -> assert false) bs in
        WithPos.(make ~pos:(pos b) (Funs fs))
    | _ -> super#binding b
end
*)

let check_dups funs tys =
  (* Check to see whether there are any duplicate names, and report
   * an error if so. *)
  let assoc_to_list_map get_name xs =
    List.fold_left (fun acc (x, pos)) ->
      let name = get_name x in
      StringMap.update (fun x_opt ->
        OptionUtils.opt_app
          (fun positions -> Some (pos :: positions))
          (Some [pos])) acc in
  let funs_map =
    assoc_to_list_map
      (fun (bndr, _, _, _, _) -> Binder.to_name bndr) funs in
  let tys_map = assoc_to_list_map (fst3) tys in
  let dups =
    StringMap.filter (fun _ poss -> List.length poss > 1) map in
  if StringMap.cardinal dups > 0 then
    raise MultiplyDefinedMutualNames dups
  else ()

let rec desugar_mutual () =
let open WithPos in
object (o: 'self_type')
  inherit SugarTraversals.fold as super
  val ty_bindings = [];
  val fun_bindings = [];
  method get_ty_bindings = ty_bindings
  method get_fun_bindings = fun_bindings

  method! binding b =
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
                  | { node = Fun f; pos } -> ((f, pos) :: funs, tys)
                  | { node = Typenames [t]; pos } -> (funs, (t, pos) :: tys)
                  | b -> raise InvalidMutualBinding (b, pos)
              end
            ) ([], []) bs in
          let (funs, tys) = (List.rev funs_rev, List.rev tys_rev) in
          (* Check for duplicates, and report an error if so. *)
          check_dups funs tys;

