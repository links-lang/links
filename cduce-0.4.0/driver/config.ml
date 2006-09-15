(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

let features = ref []

let init_all () = List.iter (fun (_,d,f) -> f()) (List.rev !features)
let register n d f = features := (n,d,f) :: !features
let descrs () = List.rev_map (fun (n,d,_) -> (n,d)) !features
let inhibit n = features := List.filter (fun (n',_,_) -> n <> n') !features

