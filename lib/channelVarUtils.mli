val affected_channels :
  Value.env -> (* Environment upon raising of exception *)
  Ir.computation list -> (* Pure frames in remainder of try-block *)
  Value.t list

val variables_in_computation :
  Ir.computation ->
  Var.var list (* List of values in the computation *)

