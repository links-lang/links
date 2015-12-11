(* module TyEnv = Env.String *)

(* let rec listu : *)
(*           'self_type -> *)
(*           ('self_type -> 'a -> 'self_type * 'a) -> *)
(*           'a list -> 'self_type * 'a list = *)
(*   fun o f -> *)
(*   function *)
(*   | [] -> (o, []) *)
(*   | x :: xs -> *)
(*      let (o, x) = f o x in *)
(*      let (o, xs) = listu o f xs in (o, x::xs) *)

(* let option : *)
(*       'self_type -> *)
(*       ('self_type -> 'a -> ('self_type * 'a * Types.datatype)) -> *)
(*       'a option -> ('self_type * ('a option) * (Types.datatype option)) *)
(*   = *)
(*   fun o f -> *)
(*   function *)
(*   | None -> (o, None, None) *)
(*   | Some x -> let (o, x, t) = f o x in (o, Some x, Some t) *)

class where_prov_rewriting env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  val dp = Sugartypes.dummy_position



  (* method rewriteIterpatt : Sugartypes.iterpatt -> Sugartypes.iterpatt = *)
  (*   (\* Strips Prov constructor from record fields *\) *)
  (*   let rec strip_prov : Types.datatype -> Types.datatype = function *)
  (*     | `MetaTypeVar point -> *)
  (*        begin *)
  (*          match Unionfind.find point with *)
  (*          | `Body t -> strip_prov t *)
  (*          (\* Do I need all of the recursive stuff? *\) *)
  (*          (\* | `Var _ -> t *\) *)
  (*          (\* | `Recursive (var, _) when IntSet.mem var rec_points -> t *\) *)
  (*          (\* | `Recursive (var, body) -> *\) *)
  (*          (\*    let body = o#desugar (IntSet.add var rec_points) body in *\) *)
  (*          (\*    Unionfind.change point (`Recursive (var, body)); *\) *)
  (*          (\*    `MetaTypeVar point *\) *)
  (*        end *)
  (*     | `Record (fieldmap, row_var, dual) -> *)
  (*        let deprov = function *)
  (*          | `Application (abstype, [`Type arg]) when abstype == Types.prov -> arg *)
  (*          | x -> x in *)
  (*        let f : Types.field_spec -> Types.field_spec = function *)
  (*          | `Present t -> `Present (deprov t) *)
  (*          | `Absent -> assert false *)
  (*          | `Var meta_presence_var -> assert false in *)
  (*        `Record (StringMap.map f fieldmap, row_var, dual) *)
  (*     | t -> Debug.print ("tried to strip prov off of nonrecord-nonmetatypevar: "^Types.Show_datatype.show t); t in *)
  (*   function *)
  (*   | `Table ((`Variable (name, (Some datatype), posv) as v, pos), ((`Var n, pos2) as expression)) as t -> *)
  (*      (\* Is the datatype ever None? *\) *)
  (*      Debug.print ("iterpatt: "^Sugartypes.Show_iterpatt.show t); *)
  (*      Debug.print ("variable: "^Sugartypes.Show_patternnode.show v); *)
  (*      Debug.print ("expression: "^Sugartypes.Show_phrase.show expression); *)
  (*      Debug.print ("datatype: "^Types.Show_datatype.show datatype); *)
  (*      let datatype' = strip_prov datatype in *)
  (*      Debug.print ("deproved datatype: "^Types.Show_datatype.show datatype'); *)
  (*      let iterpatt' = `Table ((`Variable (name, Some datatype', dp), dp), expression) in *)
  (*      let prov_app = (`Var name, dp) in *)
  (*      let expression' : Sugartypes.phrase = (`Iteration ([iterpatt'], prov_app, None, None)), dp in *)
  (*      let outer_pattern : Sugartypes.pattern = `Variable (name, Some datatype, Sugartypes.dummy_position), Sugartypes.dummy_position in *)
  (*      let res = `Table (outer_pattern, expression') in *)
  (*      Debug.print ("desugared to: "^Sugartypes.Show_iterpatt.show res); *)
  (*      t *)
  (*   | `Table (_, _) -> assert false (\* TODO other patterns where we need to do things? *\) *)
  (*   | `List (_, _) as l -> l *)

  method! iterpatt : Sugartypes.iterpatt -> ('self_type * Sugartypes.iterpatt) = function
    | `Table (pattern, phrase) ->
       let (o, pattern) = o#pattern pattern in
       let (o, phrase, _t) = o#phrase phrase in
       let phrase' : Sugartypes.phrase = (`Projection (phrase, "2"), dp) in
       Debug.print ("Before: "^Sugartypes.Show_phrase.show phrase^"\n after: "^Sugartypes.Show_phrase.show phrase');
       (* We don't actually *change* the type (apparently, everywhere, something...), so don't try to extract *)
       (* let t = TypeUtils.project_type "1" t in *)
       (o, `List (pattern, phrase'))
    | `List _ as i -> super#iterpatt i

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | `TableLit (name, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db) as _dbg ->
       Debug.print ("TableLit: "^Sugartypes.Show_phrasenode.show _dbg);
       let (o, name, _) = o#phrase name in
       let (o, keys, _) = o#phrase keys in
       let (o, db, _) = o#phrase db in
       let (o, dtype) = o#sugar_datatype dtype in
       let (o, read_row) = o#datatype read_row in
       let (o, write_row) = o#datatype write_row in
       let (o, needed_row) = o#datatype needed_row in
       let tablelit : Sugartypes.phrasenode =
         `TableLit (name, (dtype, Some (read_row, write_row, needed_row)), constraints, keys, db) in
       let tablelit_type = `Table (read_row, write_row, needed_row) in

       let pattern : Sugartypes.pattern = (`Variable ("p", Some read_row, dp), dp) in
       (* Not sure what the `datatype option` in a ListLit is.
          - None fails pattern matching in transformSugar
          - Could be the element type. Could be the list type. *)
       let prov_calc_expr : Sugartypes.phrasenode = `ListLit ([`Var "p", dp], Some read_row) in
       let iter : Sugartypes.phrasenode = `Iteration ([`Table (pattern, (tablelit, dp))], (prov_calc_expr, dp), None, None) in

       let prov_type = Types.make_list_type read_row in
       let pair : Sugartypes.phrasenode = `TupleLit [(tablelit, dp); (iter, dp)] in
       let pair_type = Types.make_tuple_type [tablelit_type; prov_type] in
       Debug.print ("TableLit desugared:\n"^Sugartypes.Show_phrasenode.show pair);
       (o, pair, pair_type)
       
    | `Iteration (gens, body, cond, orderby) as _dbg ->
       Debug.print ("Iteration: \n" ^ Sugartypes.Show_phrasenode.show _dbg);
       let (o, e, t) = super#phrasenode _dbg in
       Debug.print ("Desugared iteration:\n" ^ Sugartypes.Show_phrasenode.show e);
       (o, e, t)

    (* | `Query (option_something, body, option_datatype) as _dbg -> *)
    (*    Debug.print ("Desugar query: " ^ Sugartypes.Show_phrasenode.show _dbg); *)
    (*    super#phrasenode _dbg *)
    (* | `Iteration (gens, body, cond, orderby) as _dbg -> *)
    (*    Debug.print ("Desugar iteration: \n" ^ Sugartypes.Show_phrasenode.show _dbg); *)
    (*    let (o, gens) = listu o (fun o -> o#iterpatt) gens in *)
    (*    (\* let gens' = List.map o#rewriteIterpatt gens in *\) *)
    (*    let (o, body, t) = o#phrase body in *)
    (*    let (o, cond, _) = option o (fun o -> o#phrase) cond in *)
    (*    let (o, orderby, _) = option o (fun o -> o#phrase) orderby in *)
    (*    let res = `Iteration (gens, body, cond, orderby) in *)
    (*    Debug.print ("Desugared iteration to: \n" ^ Sugartypes.Show_phrasenode.show res); *)
    (*    (o, res, t) *)
    | e -> super#phrasenode e
end

let where_prov_rewriting env = ((new where_prov_rewriting env) : where_prov_rewriting :> TransformSugar.transform)
