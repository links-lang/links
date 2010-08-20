(** serialize an algebra DAG to the XML format expected by pathfinder and store
    the XML string in a buffer *)
val export_plan : Buffer.t -> Algebra_dag.t -> unit
