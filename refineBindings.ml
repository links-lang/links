open Utility
open Sugartypes
open Printf


(* Helper function: add a group to a list of groups *)
let add group groups = match group with
  | [] -> groups
  | _  -> List.rev group::groups

(** [refine_bindings] locates mutually-recursive sccs in sequences of
    bindings.  (As a side effect we also dispense with [`Infix]
    declarations, which are only used during the parsing stage.)
*)
let refine_bindings : binding list -> binding list =
  fun bindings ->
    (* Group sequences of functions together *)
    let initial_groups =

      (* Technically it shouldn't be necessary to ensure that the
         order of functions defined within a group is preserved (the
         List.rev above), but it helps with debugging, and it turns
         out to be necessary in order for desugaring of for
         comprehensions to work properly in the prelude - which
         defines concatMap. *)
      (* group: the group we're currently working on, groups = the groups we've processed *)
      let group, groups =
        List.fold_right
          (fun (binding,_ as bind) (thisgroup, othergroups) ->
            match binding with
              (* Modules and funs will have been eliminated by now *)
              | `Module _ -> assert false
              | `Funs _ -> assert false
              | `Exp _
              | `Foreign _
              | `Import _
              | `Type _
              | `Val _ ->
                  (* collapse the group we're collecting, then start a
                     new empty group *)
                  ([], add [bind] (add thisgroup othergroups))
                | `Fun _ ->
                     (* Add binding to group *)
                  (bind::thisgroup, othergroups)
                | `Infix ->
                     (* discard binding *)
                  (thisgroup, othergroups))
            bindings ([], [])
      in
        add group groups
    in
      (* build a callgraph *)
    let callgraph : _ -> (string * (string list)) list
      = fun defs ->
        let defs = List.map
          (function
            | `Fun ((name,_,_), _, (_, funlit), _, _), _ -> (name, funlit)
            | _ -> assert false) defs in
        let names = StringSet.from_list (List.map fst defs) in
          List.map
            (fun (name, body) -> name,
               StringSet.elements
                 (StringSet.inter (Freevars.funlit body) names))
            defs in
      (* refine a group of function bindings *)
    let groupFuns pos (funs : binding list) : binding list =
      (* Unwrap from the bindingnode type *)
      let unFun = function
        | `Fun (b, lin, (_, funlit), location, dt), pos -> (b, lin, (([], None), funlit), location, dt, pos)
        | _ -> assert false in
      let find_fun name =
        List.find (function
                     | `Fun ((n,_,_), _, _, _, _), _ when name = n -> true
                     | _ -> false)
          funs in
      let graph = callgraph funs in
      let sccs = Graph.topo_sort_sccs graph in
        List.map
          (fun scc ->
             let funs = List.map (find_fun ->- unFun) scc in
               match funs with
                 | [(((n, _, _) as b), lin, ((tyvars, _), body), location, dt, pos)]
                     when not (StringSet.mem n (Freevars.funlit body)) -> `Fun (b, lin, (tyvars, body), location, dt), pos
                 | _ -> `Funs (funs), pos)

          sccs
    in
      (* refine a group of bindings *)
    let groupBindings = function
        (* TODO:

           Compute the position corresponding to the whole collection
           of functions.
        *)
      | (`Fun _, _)::_ as funs -> groupFuns (Lexing.dummy_pos, Lexing.dummy_pos, None) funs
      | binds -> binds in
    concat_map groupBindings initial_groups

(*
  * We need three traversals:
  * 1) Fold: find all type references within a type.
  * 2) Map: Replace all type applications of one name with another name
  * 3) Map: Inline a type application with a type.
*)
let find_type_references =
object (self)
  inherit SugarTraversals.fold as super

  val references : string list = []
  method add x = {< references = x :: references >}

  method references =
    StringSet.elements (StringSet.from_list (List.rev references))

  method datatype = function
    | `TypeApplication (tyAppName, argList) as tyApp ->
          let o =
            List.fold_left (fun acc ta -> acc#type_arg ta) self argList
          in
            o#add tyAppName
    | x -> super#datatype x

  method row_var = function
    | `Open (x, _, _) -> self#add x
    | `Recursive (x, row) as rrv ->
        let o = self#add x in o#row row
    | x -> super#row_var x

end

let findTyRefs ty =
  (find_type_references#datatype ty)#references


(* Type application substitution *)
let subst_ty_app refFrom refTo =
object(self)
  inherit SugarTraversals.map as super

  method datatype : datatype -> datatype = function
    | `TypeApplication (tyAppName, argList) as tyApp ->
        if tyAppName = refFrom then `TypeVar (refTo, Some default_subkind, `Rigid)
        else super#datatype tyApp
    | dt -> super#datatype dt
end

let substTyApp ty refFrom refTo =
  (subst_ty_app refFrom refTo)#datatype ty


(* Type variable substitution *)
let subst_ty_var varFrom (taTo : type_arg) =
object(self)
  inherit SugarTraversals.map as super

  (* varFrom: Type variable to substitute from.
   *  - This is the one in the tyTy
   * taTo: Type arg to replace with.
   *  - This is the one found in the application
   *)

  method datatype : datatype -> datatype =
    fun dt ->
      match dt with
        | `TypeVar (n, _, _) when n = varFrom ->
            (match taTo with
               | `Type dtTo -> dtTo
               | _ -> super#datatype dt)
        | `Forall (qs, quantDt) ->
            (match taTo with
              | `Type (`TypeVar (n, _, _)) ->
                  let qs' =
                    List.map (fun (tv, k, f as q) ->
                      if tv = varFrom then
                        (n, k, f)
                      else q) qs in `Forall (qs', self#datatype quantDt)
              | _ -> super#datatype dt)
        | _ -> super#datatype dt

  method fieldspec : fieldspec -> fieldspec =
    fun fs ->
      match fs with
        | `Var (n, _, _) when n = varFrom ->
            (match taTo with
              | `Presence (`Var _ as fsTo) -> fsTo
              | _ -> super#fieldspec fs)
        | _ -> super#fieldspec fs

  method row_var : row_var -> row_var = function
    | `Open (n, _, _) as rv when n = varFrom ->
        (match taTo with
          | `Row (_, (`Open _ as rv2)) -> rv2
          | _ -> super#row_var rv)
    | rv -> super#row_var rv

end

let substTyArg varFrom taTo ty =
  (subst_ty_var varFrom taTo)#datatype ty

(* Type inlining *)
let inline_ty toFind inlineArgs toInline =
object(self)
  inherit SugarTraversals.map as super

  method datatype : datatype -> datatype =
    fun dt ->
      match dt with
        | `TypeApplication (tyAppName, argList) as tyApp ->
            if tyAppName = toFind then (* && List.length argList = 0 then *)
              (* Ok, so what we need to do:
                * We have a list of the type arguments of the type to inline,
                * and also have a list of type arguments within the type app.
                * What we need to do is for every ty arg in the type,
                * substitute it for the corresponding arg in the arg list.
                * Bit like a fold / zip. There's probably some funky
                * category theory name for it, but blah.
                *)
              if (List.length inlineArgs = List.length argList) then
                List.fold_right (fun ((from_arg, _, _), to_arg) ty ->
                  (* We only want to work with type / row / presence *variables* here *)
                    substTyArg from_arg to_arg ty
                  ) (List.combine inlineArgs argList) toInline
              else
                (* Arity error, let something else pick it up *)
                 tyApp
            else
              super#datatype dt
        | x -> super#datatype x

end

let inlineTy ty tyRef inlineArgs refinedTy =
  (inline_ty tyRef inlineArgs refinedTy)#datatype ty

(* Similar to refine_bindings, RefineTypeBindings.refineTypeBindings finds
 * sequences of mutually recursive types, and rewrites them as explicit mus. *)
module RefineTypeBindings = struct

  (* Type synonyms *)
  type type_name = string
  type type_ty = name * (quantifier * tyvar option) list * datatype'
  type mu_alias = string
  type reference_info = (type_name, (type_name list * bool * position)) Hashtbl.t
  type type_hashtable = (type_name, type_ty) Hashtbl.t

  (* Type synonyms for substitution environments *)
  type alias_env = (type_name * mu_alias) list
  type type_env = (type_variable * type_arg) list

  (*
   * Split binding list into groups for the purposes of type refinement.
   * A "group" is defined as a block of type bindings uninterrupted by any
   * other bindings.
  *)
  let initialGroups : binding list -> binding list list =
    fun bindings ->
      let group, groups =
        List.fold_right (fun (binding, _ as bind) (currentGroup, otherGroups) ->
          match binding with
          | `Module _ -> assert false
          | `Funs _
          | `Fun _
          | `Foreign _
          | `Import _
          | `Val _
          | `Exp _
          | `Infix ->
              (* Collapse and start a new group *)
              ([], (currentGroup :: [bind] :: otherGroups))
          | `Type _ ->
              (* Add to this group *)
              (bind :: currentGroup, otherGroups)
        ) bindings ([], [])
      in add group groups

  (* typeReferences gets us a list of type names referenced by a given type. *)
  let typeReferences : type_ty -> type_hashtable -> type_name list =
    fun (_, _, (sugaredDT, _)) ht ->
      List.filter (fun x -> Hashtbl.mem ht x)
        (findTyRefs sugaredDT)

  (* Does a type refer to itself? *)
  let refersToSelf : type_ty -> type_name list -> bool =
    fun (name, tyVars, (sugaredDT, _)) refs ->
      let qExists =
        List.exists (fun (quant, _) ->
            let (qName, _, _) = quant in name = qName
          ) tyVars in
      let selfInDT = List.exists (fun x -> x = name) refs in
        qExists || selfInDT

  (* Gets the name of a type. *)
  let getName : type_ty -> type_name =
    fun (name, _, _) -> name

  (* Gets the sugared datatype from a type binding. *)
  let getDT : type_ty -> datatype =
    fun (_, _, (dt, _)) -> dt

  (* Updates the datatype in a type binding. *)
  let updateDT : type_ty -> datatype -> type_ty =
    fun (name, tyArgs, (_, unsugaredDT)) newDT ->
      (name, tyArgs, (newDT, unsugaredDT))

  let referenceInfo : binding list -> type_hashtable -> reference_info =
    fun binds typeHt ->
      let ht = Hashtbl.create 30 in
      List.iter (fun (bind, pos) ->
        match bind with
          | `Type (name, _, _ as tyTy) ->
              let refs = typeReferences tyTy typeHt in
              let referencesSelf = refersToSelf tyTy refs in
              Hashtbl.add ht name (refs, referencesSelf, pos)
          | _ -> assert false;
      ) binds;
      ht

  let refGraph : reference_info -> (type_name * type_name list) list =
    fun riTable ->
      (* Massively irritating, there's no toList function... *)
      List.rev (Hashtbl.fold (fun k (adj, _, _) acc ->
        (k, adj) :: acc
      ) riTable [])

  let isSelfReferential : type_name -> reference_info -> bool =
    fun name riTable ->
      snd3 (Hashtbl.find riTable name)


  (* Performs the inlining transformation on a given type. *)
  let rec refineType :
      type_ty ->
      alias_env ->
      type_hashtable ->
      type_name list -> (* Other components in the SCC list *)
      reference_info ->
      type_ty =
    fun ty env ht sccs ri ->
      let tyName = getName ty in
      let rts = isSelfReferential tyName ri in
      let sugaredDT = getDT ty in
      (* If we're self-referential, then add in a top-level mu *)
      let (env', dt) =
        if List.mem_assoc tyName env then assert false else
        if rts || List.length sccs > 1 then
          let muName = gensym ~prefix:"refined_mu" () in
            ((tyName, muName) :: env, `Mu (muName, sugaredDT))
        else (env, sugaredDT) in
      (* Now, we go through the list of type references.
       * If the reference is in the substitution environment, we replace it
       * with the mu variable we've created.
       * If not, then we'll need to refine that type, and inline it.
       *)
      let refinedTy = List.fold_right (fun tyRef curDataTy ->
        (* Only perform this transformation on other types in the group, and to self if self-referential. *)
        let shouldApply = Hashtbl.mem ht tyRef && (tyName <> tyRef || rts) in
        if shouldApply then
          (if List.mem_assoc tyRef env' then
            (* Simple tyApp substitution *)
            let muName = List.assoc tyRef env' in
            substTyApp curDataTy tyRef muName
           else
            (* Otherwise, we'll need to refine and inline *)
               let to_refine = Hashtbl.find ht tyRef in
               let (_, arg_list, _) = to_refine in
               let to_refine_args = List.map fst arg_list in
               let (_, _, (refinedRef, _)) = refineType to_refine env' ht sccs ri in
               inlineTy curDataTy tyRef to_refine_args refinedRef)
        else
          curDataTy
      ) sccs dt in

    updateDT ty refinedTy

  let refineSCCGroup :
      reference_info ->
      (type_name, type_ty) Hashtbl.t ->
      type_name list ->
      binding list =
    fun ri ht sccs ->
      let getPos name =
        thd3 (Hashtbl.find ri name) in
      List.map (fun name ->
        let rts = isSelfReferential name ri in
        let res = refineType (Hashtbl.find ht name) [] ht sccs ri in
        let (_, _, (res_dt, _)) = res in
        (* printf "Refined type %s: \n %s \n\n" name (Sugartypes.Show_datatype.show res_dt); *)
        (`Type res, getPos name)
      ) sccs

  let isTypeGroup : binding list -> bool = function
    | (`Type _, _) :: xs -> true
    | _ -> false

  (* Performs type refinement on a binding group. *)
  let refineGroup : binding list -> binding list = function
    | binds when isTypeGroup binds ->
      (* Create a hashtable mapping names to type bindings. *)
      let ht = Hashtbl.create 30 in
      List.iter (fun (x, _) ->
        match x with
          | `Type (name, _, _ as tyTy) as ty ->
            Hashtbl.add ht name tyTy;
          | _ -> assert false;
      ) binds;
      let refInfoTable = referenceInfo binds ht in
      let graph = refGraph refInfoTable in
      let sccList = Graph.topo_sort_sccs graph in
      (* Irritatingly we need to reattach the bindings with a position *)
      List.concat (
        List.map
          (fun sccGroup ->
            refineSCCGroup refInfoTable ht sccGroup
          ) sccList
      )
    | xs -> xs
  (* Refines a list of bindings. *)
  let refineTypeBindings : binding list -> binding list =
    fun binds ->
      List.concat (List.map refineGroup (initialGroups binds))
end

let refine_bindings =
object (self)
  inherit SugarTraversals.map as super
  method phrasenode : phrasenode -> phrasenode = function
    |`Block (bindings, body) ->
       let bindings = self#list (fun o -> o#binding) bindings in
       let body = self#phrase body in
       let refined_bindings =
         (RefineTypeBindings.refineTypeBindings ->-
         refine_bindings) bindings in
       `Block (refined_bindings, body)
    | p -> super#phrasenode p

  method program : program -> program =
    fun (bindings, body) ->
      let bindings = self#list (fun o -> o#binding) bindings in
      let body = self#option (fun o -> o#phrase) body in
      let refined_bindings =
        (RefineTypeBindings.refineTypeBindings ->-
        refine_bindings) bindings in
      refined_bindings, body

  method sentence : sentence -> sentence = function
    |`Definitions defs ->
       let defs = self#list (fun o -> o#binding) defs in
       let refined_bindings =
         (RefineTypeBindings.refineTypeBindings ->-
         refine_bindings) defs in
       `Definitions (refined_bindings)
    | d -> super#sentence d

end
