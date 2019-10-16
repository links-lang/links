(* IR transformations are typeability preserving. *)

type state = { context: Context.t;
               primitive_vars: Utility.IntSet.t;
               datatype: Types.datatype }

type result = Result of { state: state;
                          program: Ir.program }

let context : state -> Context.t
  = fun { context; _ } -> context

let return : state -> Ir.program -> result
  = fun state program -> Result { state; program }

let with_type : Types.datatype -> state -> state
  = fun datatype state -> { state with datatype }

module type S = sig
  val name : string
  val program : state -> Ir.program -> result
end

class virtual ir_transformer =
        object (_ : 'self)
          method virtual program : Ir.program -> Ir.program * Types.datatype * 'self
        end

module Make(T : sig
             val name : string
             val obj : Types.typing_environment -> ir_transformer end) = struct
  let name = T.name
  let program : state -> Ir.program -> result
    = fun state program ->
    let tyenv = Context.typing_environment state.context in
    let (program', datatype, _) = (T.obj tyenv)#program program in
    return (with_type datatype state) program'
end
