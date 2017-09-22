val affected_channels :
  Value.env -> (* Environment upon raising of exception *)
  Value.env -> (* Environment upon installation of handler *)
  Ir.computation list -> (* Pure frames in remainder of try-block *)
  Value.t list
