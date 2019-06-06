open CommonTypes
open Operators
open Utility
open SourceCode.WithPos
open Sugartypes
open SugarConstructors.DummyPositions

(* This module desugars multi-argument functions, anonymous functions, and
   operator sections.

  1. Multi-argument functions are turned into a series of nested one-argument
     functions:

    fun f[qs](xs1)...(xsk) {e}
  -->
    fun f[qs](xs1) {
      fun f2(xs2) {
        ...
        fun fk(xsk) {
          e
        }
        fk
        ...
      }
      f2
    }

  2. Anonymous functions are named and then turned into a series of nested
     one-argument functions:


    fun [qs](xs1)...(xsk) {e}
  -->
    {fun f[qs](xs1) {
       fun f1[](xs2) {
       ...
         fun fk[](xsk) {
           e
         }; fk
       ...
       }; f1
    }; f}


  3. Operator sections are turned into named functions:

    (.l)
  -->
    { fun f(x:r) {x.l}; f }

*)


(* unwrap a curried function definition as
   a collection of nested functions
*)
let unwrap_def (bndr, linearity, (tyvars, lam), location, t) =
  let f = Binder.to_name bndr in
  let ft = Binder.to_type bndr in
  let rt = TypeUtils.return_type ft in
  let lam =
    let rec make_lam t : funlit -> funlit =
      function
        | ([_ps], _body) as lam -> lam
        | (ps::pss, body) ->
            let g = gensym ~prefix:"_fun_" () in
            let rt = TypeUtils.return_type t in
              ([ps], block
                  ([fun_binding' ~linearity ~location (binder ~ty:t g)
                                 (make_lam rt (pss, body))],
                   var g))
        | _, _ -> assert false
    in make_lam rt lam
  in (binder ~ty:ft f, linearity, (tyvars, lam), location, t)

(*
  unwrap a curried function definition
  with a position attached
  (for recursive functions)
*)
let unwrap_def_dp { rec_binder = fb; rec_linearity = lin; rec_definition = tlam;
                    rec_location = location; rec_signature = t; rec_pos } =
  let (fb, lin, tlam, location, t) = unwrap_def (fb, lin, tlam, location, t) in
  { rec_binder = fb; rec_linearity = lin; rec_definition = tlam;
    rec_location = location; rec_signature = t; rec_pos }

class desugar_funs env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method private desugarFunLit argss lin lam location tvs =
    let inner_mb     = snd (last argss) in
    let (o, lam, rt) = o#funlit inner_mb lam in
    let ft = List.fold_right (fun (args, mb) rt -> `Function (args, mb, rt))
                             argss rt in
    let f = gensym ~prefix:"_fun_" () in
    let body, tvs, ft = match tvs with
      | [] -> Var f, [], ft
      | _  ->
         (* If a FunLit is surrounded by a type abstraction we need to generate
            a corresponding type application.  Type abstractions are inserted
            during type checking.  This really shouldn't happen since it couples
            typechecking with this desugaring stage.  If we didn't insert type
            application here the resulting AST would be ill-typed. *)
         let (tvs, tyargs), ft = Generalise.generalise env.Types.var_env ft in
         let ft = Instantiate.freshen_quantifiers ft in
         tappl (Var f, tyargs), tvs, ft in
    let (bndr, lin, tvs, loc, ty) =
      unwrap_def (binder ~ty:ft f, lin, (tvs, lam), location, None) in
    let e = block_node ([with_dummy_pos (Fun { fun_binder = bndr; fun_linearity = lin;
                                               fun_definition = tvs; fun_location = loc;
                                               fun_signature = ty })],
                         with_dummy_pos body)
    in (o, e, ft)

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | TAbstr (tvs', {node = TAppl ({node = TAbstr (tvs, {node =
         FunLit (Some argss, lin, lam, location); _ } ); _}, tyargs); _})
      when Settings.get_value Instantiate.quantified_instantiation ->
       let (o, e, ft) = o#desugarFunLit argss lin lam location tvs in
       (o, TAbstr (tvs', with_dummy_pos (TAppl (with_dummy_pos (
            TAbstr (tvs, with_dummy_pos e)), tyargs))), ft)
    | FunLit (Some argss, lin, lam, location) ->
       o#desugarFunLit argss lin lam location []
    | Section (Section.Project name) ->
        let ab, a = Types.fresh_type_quantifier (lin_any, res_any) in
        let rhob, (fields, rho, _) = Types.fresh_row_quantifier (lin_any, res_any) in
        let effb, eff = Types.fresh_row_quantifier (lin_any, res_any) in

        let r = `Record (StringMap.add name (`Present a) fields, rho, false) in

        let f = gensym ~prefix:"_fun_" () in
        let x = gensym ~prefix:"_fun_" () in
        let ft : Types.datatype = `ForAll (Types.box_quantifiers [ab; rhob;  effb],
                                           `Function (Types.make_tuple_type [r], eff, a)) in

        let pss = [[variable_pat ~ty:r x]] in
        let body = with_dummy_pos (Projection (var x, name)) in
        let e : phrasenode =
          block_node
            ([fun_binding' ~tyvars:[ab; rhob; effb] (binder ~ty:ft f) (pss, body)],
             var f)
        in (o, e, ft)
    | e -> super#phrasenode e

  method! bindingnode = function
    | Fun _ as b ->
        let (o, b) = super#bindingnode b in
          begin
            match b with
              | Fun { fun_binder = bndr; fun_linearity = lin;
                      fun_definition = tvs; fun_location = loc;
                      fun_signature = ty } ->
                 let (bndr', lin', tvs', loc', ty') =
                   unwrap_def (bndr, lin, tvs, loc, ty) in
                 (o, Fun { fun_binder = bndr'; fun_linearity = lin';
                           fun_definition = tvs'; fun_location = loc';
                           fun_signature = ty' })
              | _ -> assert false
          end
    | Funs _ as b ->
        let (o, b) = super#bindingnode b in
          begin
            match b with
              | Funs defs -> (o, Funs (List.map unwrap_def_dp defs))
              | _ -> assert false
          end
    | b -> super#bindingnode b
end

let desugar_funs env = ((new desugar_funs env) : desugar_funs :> TransformSugar.transform)

let desugar_program : TransformSugar.program_transformer =
  fun env program -> snd3 ((desugar_funs env)#program program)

let desugar_sentence : TransformSugar.sentence_transformer =
  fun env sentence -> snd ((desugar_funs env)#sentence sentence)

let has_no_funs =
object
  inherit SugarTraversals.predicate as super

  val has_no_funs = true
  method satisfied = has_no_funs

  method! phrasenode = function
    | FunLit _ -> {< has_no_funs = false >}
    | e -> super#phrasenode e

  method! bindingnode = function
    | Fun { fun_definition = (_, ([_], _)); _ } as b -> super#bindingnode b
    | Fun _ -> {< has_no_funs = false >}
    | Funs defs as b ->
        if
          List.exists
            (function
               | { rec_definition = (_, ([_], _)); _ } -> false
               | _ -> true) defs
        then
          {< has_no_funs = false >}
        else
          super#bindingnode b
    | b -> super#bindingnode b
end
