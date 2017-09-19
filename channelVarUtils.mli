val affected_channels :
  Value.env -> (* Environment upon raising of exception *)
  Value.env -> (* Environment upon installation of handler *)
  (Ir.scope * Ir.var * Value.env * Ir.computation) list -> (* Pure frames in remainder of try-block *)
  Value.t list
