(*
let cu = Types.CompUnit.mk (Encodings.Utf8.mk "OCAML")
let () = Types.CompUnit.enter cu; Types.caml_mode := true
let init = ref []
let serialize = ref []

module Mk(X : Custom.T) = struct
  module A = Custom.Array(X)
  
  type pchunk = { mutable nb : int; mutable lst : X.t list }
  let put c x = 
    let i = c.nb in
    c.nb <- succ i;
    c.lst <- x::c.lst;
    i
    
  let init () = { nb = 0; lst = [] }
  let serialize s c = Serialize.Put.array X.serialize s 
		      (Array.of_list (List.rev c.lst))

  type 'a entry = Serialized of X.t | Computed of 'a
  type 'a chunk = 'a entry array

  let deserialize s = 
    Serialize.Get.array (fun s -> Serialized (X.deserialize s)) s

  let get f a i =
    match a.(i) with
      | Serialized x ->
	  let x = f x in
	  a.(i) <- Computed x;
	  x
      | Computed x ->
	  x
end

module PM = Mk(Custom.Pair(Types)(Custom.List(Patterns.Node)))
module CONST = Mk(Types.Const)
module LAB = Mk(Ident.LabelPool)
module T = Mk(Types)
module T2 = Mk(Custom.Pair(Types)(Types))
module LABA = Mk(Custom.Array(Ident.LabelPool))
module TAG = Mk(Atoms.V)
module TAGA = Mk(Custom.Array(Custom.Pair(Atoms.V)(Custom.Int)))

module P = struct
  type chunk =
      { pm : PM.pchunk;
	cst : CONST.pchunk;
	lab : LAB.pchunk;
	typ : T.pchunk;
 	laba : LABA.pchunk;
	tag : TAG.pchunk;
	taga : TAGA.pchunk;
	typ2 : T2.pchunk;
     }

  let init () = 
    { pm = PM.init ();
      cst = CONST.init ();
      lab = LAB.init ();
      typ = T.init ();
      laba = LABA.init ();
      tag = TAG.init ();
      taga = TAGA.init ();
      typ2 = T2.init ();
    }

  let serialize s c =
    PM.serialize s c.pm;
    CONST.serialize s c.cst;
    LAB.serialize s c.lab;
    T.serialize s c.typ;
    LABA.serialize s c.laba;
    TAG.serialize s c.tag;
    TAGA.serialize s c.taga;
    T2.serialize s c.typ2;
    ()

  let pm c = PM.put c.pm
  let const c = CONST.put c.cst
  let label c = LAB.put c.lab
  let typ c = T.put c.typ
  let label_array c = LABA.put c.laba
  let tag c = TAG.put c.tag
  let tag_array c = TAGA.put c.taga
  let typ2 c t1 t2 = T2.put c.typ2 (t1,t2)

  let mk c =
    let s = Serialize.Put.run serialize c in
    ignore (Types.CompUnit.close_serialize ());
    s

end

module G = struct
  type chunk =
      { pm :
	  (Patterns.Compile.dispatcher * int Patterns.Compile.rhs array)
	  PM.chunk;
	cst : Value.t CONST.chunk;
	lab : Ident.label LAB.chunk;
	typ : Types.t T.chunk;
	laba : Ident.label array LABA.chunk;
	tag : Value.t TAG.chunk;
	taga : int Atoms.map TAGA.chunk;
	typ2 : (Value.t -> unit) T2.chunk;
      }

  let deserialize s =
    let pm = PM.deserialize s in
    let cst = CONST.deserialize s in
    let lab = LAB.deserialize s in
    let typ = T.deserialize s in
    let laba = LABA.deserialize s in
    let tag = TAG.deserialize s in
    let taga = TAGA.deserialize s in
    let typ2 = T2.deserialize s in
    { pm = pm; cst = cst; lab = lab; typ = typ; laba = laba; tag = tag;
      taga = taga; typ2 = typ2 }

  let mk s = 
    Types.clear_deserialize_table ();
    Serialize.Get.run deserialize s

  let mk_pm (t,brs) =
    let brs = Array.to_list (Array.mapi (fun i x -> (x,i)) (Array.of_list brs))
    in
    Patterns.Compile.make_branches t brs

  let pm chunk i v =
    let (d,rhs) = PM.get mk_pm chunk.pm i in
    let (code,bindings) = Run_dispatch.run_dispatcher d v in
    match rhs.(code) with
      | Patterns.Compile.Fail -> (-1,[||])
      | Patterns.Compile.Match (bind,i) ->
	  i,
	  Array.map
	    (fun (_,i) -> if (i == -1) then v else bindings.(i))
	    (Array.of_list bind)


  let const chunk i =
    CONST.get Value.const chunk.cst i
	  
  let remove_label chunk i v =
    Value.remove_field (LAB.get (fun x -> x) chunk.lab i) v
  let get_field chunk i v =
    Value.get_field v (LAB.get (fun x -> x) chunk.lab i)

  let typ chunk i =
    T.get (fun x -> x) chunk.typ i

  let check chunk i v =
    T2.get (fun (t0,t) -> Explain.check t0 t) chunk.typ2 i v;
    v

  let record chunk i vs =
    Value.mk_record (LABA.get (fun x -> x) chunk.laba i) vs

  let constr_const chunk i =
    TAG.get (fun x -> Value.Atom x) chunk.tag i

  let constr chunk i vs =
    Value.ocaml2cduce_constr (constr_const chunk i) vs

  let taga chunk i = 
    TAGA.get
      (fun x ->
	 let x = Array.map (fun (t,i) ->
			      Atoms.atom t, i) x in
	 Atoms.mk_map (Array.to_list x))
      chunk.taga i

  let dconstr chunk i v =
    Value.cduce2ocaml_constr (taga chunk i) v
  let dvariant chunk i v =
    Value.cduce2ocaml_variant (taga chunk i) v
end
*)

module P = struct
  type chunk = {
    mutable nb : int;
    mutable objs : Obj.t list
  }

  let init () = { nb = 0; objs = [] }

  let mk c =
    let o = Array.of_list (List.rev c.objs) in
    Marshal.to_string (Value.extract_all (), o) []

  let put c x =
    let i = c.nb in
    c.nb <- succ i;
    c.objs <- Obj.repr x :: c.objs;
    i
end

module G = struct
  let mk s =
    let (pools,objs) = Marshal.from_string s 0 in
    Value.intract_all pools;
    objs
end
