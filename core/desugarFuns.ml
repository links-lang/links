open CommonTypes
open Operators
open Utility
open Sugartypes
open SugarConstructors.DummyPositions

(*
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


    fun [qs](xs1)...(xsk) {e}
  -->
    {fun f[qs](xs1)...(xsk) {e};
     f}


    (.l)
  -->
    fun (r) {r.l}
*)


(* unwrap a curried function definition as
   a collection of nested functions
*)
let unwrap_def ({node=f, ft; _}, linearity, (tyvars, lam), location, t) =
  let ft = val_of ft in
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
let unwrap_def_dp (fb, lin, tlam, location, t, pos) =
  let (fb, lin, tlam, location, t) = unwrap_def (fb, lin, tlam, location, t) in
    (fb, lin, tlam, location, t, pos)

class desugar_funs env =
object (o : 'self_type)
  inherit (TransformSugar.transform env) as super

  method! phrasenode : Sugartypes.phrasenode -> ('self_type * Sugartypes.phrasenode * Types.datatype) = function
    | FunLit (Some argss, lin, lam, location) ->
        let inner_mb = snd (try last argss with Invalid_argument s -> raise (Invalid_argument ("!"^s))) in
        let (o, lam, rt) = o#funlit inner_mb lam in
        let ft =
          List.fold_right
            (fun (args, mb) rt ->
               `Function (args, mb, rt))
            argss
            rt in
        let f = gensym ~prefix:"_fun_" () in
        let e =
          block_node
            ([with_dummy_pos (Fun (unwrap_def ( binder ~ty:ft f, lin, ([], lam)
                                              , location, None)))],
             var f)
        in
          (o, e, ft)
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
              | Fun r -> (o, Fun (unwrap_def r))
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

let has_no_funs =
object
  inherit SugarTraversals.predicate as super

  val has_no_funs = true
  method satisfied = has_no_funs

  method! phrasenode = function
    | FunLit _ -> {< has_no_funs = false >}
    | e -> super#phrasenode e

  method! bindingnode = function
    | Fun (_f, _lin, (_tyvars, ([_ps], _body)), _location, _t) as b ->
        super#bindingnode b
    | Fun _ -> {< has_no_funs = false >}
    | Funs defs as b ->
        if
          List.exists
            (function
               | (_f, _lin, (_tyvars, ([_ps], _body)), _location, _t, _pos) -> false
               | _ -> true) defs
        then
          {< has_no_funs = false >}
        else
          super#bindingnode b
    | b -> super#bindingnode b
end
