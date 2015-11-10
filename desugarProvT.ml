open Utility

(*
  Desugaring provenance types
  ---------------------------

*)

let dp = Sugartypes.dummy_position

let dummy_type = `Primitive `Int

module TypeVarMap = Utility.IntMap
type var_map = (bool * Types.meta_type_var) TypeVarMap.t 
                          
class desugar_prov_t env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  (* method! lookup_type (name : Sugartypes.name) : Types.datatype = *)
  (*   try let res = super#lookup_type name in res *)
  (*   with | Notfound.NotFound _ -> Debug.print ("Can't find `"^name^"` in env:"^ *)
  (*                                                "" (\* (Types.Show_typing_environment.show env) *\) *)
  (*                                                ^" What now?"); assert false  *)

  method! datatype : Types.datatype -> ('self_type * Types.datatype) =
    fun e ->
    (* let (o, e) = super#datatype e in *)
    let e' = o#desugar TypeVarMap.empty e in
    Debug.print ("Desugared dataype " ^ Types.Show_datatype.show e ^ " to " ^ Types.Show_datatype.show e');
    (o, e')

  method desugar_field_spec : var_map -> Types.field_spec -> Types.field_spec =
    fun rec_points ->
    function
    | `Absent -> `Absent
    | `Present t -> `Present (o#desugar rec_points t)
    | `Var _ -> (* TODO: what should happen here? *) assert false

  (* Copied from types.ml. Not sure this is the correct thing to do here... *)
  method desugar_row = fun rec_points row ->
    let (fields, row_var, dual) = fst (Types.unwrap_row row) in
    let fields' = StringMap.map (o#desugar_field_spec rec_points) fields in
    (fields', row_var, dual)

  method desugar_type_arg : var_map -> Types.type_arg -> Types.type_arg =
    fun rec_points ->
    function
    | `Type t -> `Type (o#desugar rec_points t)
    | `Row row -> `Row (o#desugar_row rec_points row)
    | `Presence f -> `Presence (o#desugar_field_spec rec_points f)
               
  method desugar : var_map -> Types.datatype -> Types.datatype =
    fun rec_points ->
    let desugar (t : Types.datatype) : Types.datatype = o#desugar rec_points t in
    let desugar_row (r : Types.row) : Types.row = o#desugar_row rec_points r in
    fun t ->
    match t with
    | `Not_typed
    | `Primitive _ -> t
    | `Function (f, m, t) -> `Function (desugar f, desugar_row m, desugar t)
    | `Lolli (f, m, t) -> `Lolli (desugar f, desugar_row m, desugar t)
    | `Record row -> `Record (desugar_row row)
    | `Variant row -> `Variant (desugar_row row)
    (* TODO we probably need to modify this *)
    | `Table (r, w, n) -> `Table (desugar r, desugar w, desugar n)
    | `Alias (_, t) -> desugar t
    (* This is where interesting this happen! *)
    | `Application (abstype, [`Type data_t]) when abstype = Types.prov ->
       Debug.print "Desugar prov type application";
       let prov_t = Types.prov_triple_type in
       (* We don't think we need the recursive call currently (since data_t has to be of base type),
          but we might in the future. *)
       let data_t = desugar data_t in
       Types.make_record_type (StringMap.from_alist [("prov", prov_t); ("data", data_t)])
    | `Application (abs, ts) -> `Application (abs, List.map (o#desugar_type_arg rec_points) ts)
    | `ForAll (qs, body) -> `ForAll (qs, desugar body)
    (* TODO What is this metatypevar thing about again? what do I do here? *)
    | `MetaTypeVar point as e -> e
       (* begin *)
       (*   match Unionfind.find point with *)
       (*   | `Var _ -> `MetaTypeVar point *)
       (*   | `Recursive (var, t) -> *)
       (*      if TypeVarMap.mem var rec_points then *)
       (*        let (dual, point) = TypeVarMap.find var rec_points in *)
       (*        if dual then `Dual (`MetaTypeVar point) *)
       (*        else `MetaTypeVar point *)
       (*      else *)
       (*        let var' = Types.fresh_raw_variable () in *)
       (*        let point = Unionfind.fresh (`Recursive (var', dummy_type)) in *)
       (*        Unionfind.change point (`Recursive (var', desugar (TypeVarMap.add var (false, point) rec_points) t)); *)
       (*        `MetaTypeVar point *)
       (*   | `Body s -> desugar s *)
       (* end *)
    | `Input (t, s) -> `Input (desugar t, desugar s)
    | `Output (t, s) -> `Output (desugar t, desugar s)
    | `Select row -> `Select (desugar_row row)
    | `Choice row -> `Choice (desugar_row row)
    | `Dual s -> `Dual (desugar s)
    | `End -> `End
end

let mydesugaring : Types.typing_environment -> Types.datatype -> Types.datatype =
  fun env ->
  function
  | x ->
     snd (((new desugar_prov_t env)
           : desugar_prov_t :> TransformSugar.transform)#datatype x)

let program : Types.typing_environment -> Sugartypes.program -> Sugartypes.program =
  fun env ->
  function
  | p -> snd3 (((new desugar_prov_t env)
                : desugar_prov_t :> TransformSugar.transform)#program p)
         
let environment : Types.environment -> Types.environment = function
  | env -> Env.String.map (mydesugaring {Types.var_env = Env.String.empty;
                                         Types.tycon_env = Env.String.empty;
                                         Types.effect_row = Types.make_empty_closed_row ()}) env

let typing_environment: Types.typing_environment -> Types.typing_environment = function
  | {Types.var_env;
     Types.tycon_env;
     Types.effect_row} ->
     {Types.var_env =
        (let var_env' = environment var_env in
         (* Debug.print ("env before: " ^ Types.Show_environment.show var_env) ; *)
         (* Debug.print ("env after:  " ^ Types.Show_environment.show var_env') ; *)
         var_env');
      Types.tycon_env = tycon_env; (* TODO do we need to transform in there? *)
      Types.effect_row = effect_row} (* TODO do we need to transform in there? *)

let desugar
      (tyenv : Types.typing_environment)
      ((p, t, e) : (Sugartypes.program * Types.datatype * Types.typing_environment))
    : (Sugartypes.program * Types.datatype * Types.typing_environment) =
  (* Debug.print ("tyenv: "^ Types.Show_typing_environment.show tyenv); *)
  (* Debug.print ("env before: "^ Types.Show_typing_environment.show e); *)
  (* Debug.print (Sugartypes.Show_program.show p); *)
  let e = typing_environment e in
  Debug.print ("env after: "^ Types.Show_typing_environment.show e);
  let e = (Types.extend_typing_environment e tyenv) in
  (* Debug.print ("extended env after: "^ Types.Show_typing_environment.show e); *)
  Debug.print ("Program before: "^Sugartypes.Show_program.show p);
  let p = program e p in
  Debug.print ("Program after: "^Sugartypes.Show_program.show p);
  let t = mydesugaring e t in
  (p, t, e)
