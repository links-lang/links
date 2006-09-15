(**************************************************************************)
(*  The CDuce compiler                                                    *)
(*  Alain Frisch <Alain.Frisch@inria.fr> and the CDuce team               *)
(*  Copyright CNRS,INRIA, 2003,2004,2005 (see LICENSE for details)        *)
(**************************************************************************)

type ext_info

let has_ext = ref false

let register = 
  ref (fun _ _ _ -> 
	 Location.raise_generic "No built-in support for ocaml externals")

let ext_info =
  ref (fun () -> assert false)

let resolve s args =
  has_ext := true;
  !register true s args

let typ s args =
  snd (!register false s args)


let get () = if !has_ext then Some (!ext_info ()) else None
