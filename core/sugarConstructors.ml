open Sugartypes
open Utility

let pos (start_pos, end_pos) : Sugartypes.position = (start_pos, end_pos, None)

let with_pos p = Sugartypes.with_pos (pos p)

let annotate sigpos (signame, datatype) dpos : _ -> binding =
  let checksig {node=signame; _} name =
    if signame <> name then
      raise (ConcreteSyntaxError
               ("Signature for `" ^ signame ^ "' should precede definition of `"
                ^ signame ^ "', not `"^ name ^"'.", pos sigpos)) in
    function
      | `Fun (name, lin, phrase, location) ->
          let _ = checksig signame name.node in
          with_pos dpos (`Fun ( make_untyped_binder name, lin, ([], phrase)
                              , location, Some datatype))
      | `Var (name, phrase, location) ->
          let _ = checksig signame name.node in
          with_pos dpos
            (`Val ( with_pos dpos (`Variable (make_untyped_binder name))
                  , ([], phrase), location, Some datatype))
      | `Handler (bndr, hnlit, _) ->
         let _ = checksig signame (name_of_binder bndr) in
         with_pos dpos (`Handler (bndr, hnlit, Some datatype))

let make_db_insert pos ins_exp lbls_pos lbls var_pos var =
  with_pos pos (`DBInsert
    (ins_exp, List.map fst lbls,
     with_pos lbls_pos (`ListLit ([with_pos lbls_pos
                                            (`RecordLit (lbls, None))]
                                 , None)),
     OptionUtils.opt_map (fun v -> with_pos var_pos (`Constant (`String v)))
                         var))
