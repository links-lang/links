(* Tools for rewriting trees *)

module type RewritePrimitives =
sig
  type t
  type rewriter = t -> t option

  (* Apply a, then apply b; return the result *)
  val both   : rewriter -> rewriter -> rewriter

  (* Apply a, then apply b; return the result as soon as there's a change *)
  val either : rewriter -> rewriter -> rewriter

  (* A trivial rewriter that always fails *)
  val never  : rewriter

  (* A trivial rewriter that always `succeeds' with no effect *)
  val always : rewriter

  (* Process all subnodes of this node *)
  val process_children : rewriter -> rewriter
end

module type Rewrite =
sig
  type t
  type rewriter = t -> t option
  val process_children : rewriter -> rewriter
  val both : rewriter -> rewriter -> rewriter
  val either : rewriter -> rewriter -> rewriter
  val never : rewriter
  val always : rewriter
  val any : rewriter list -> rewriter
  val all : rewriter list -> rewriter
  val topdown : rewriter -> rewriter
  val bottomup : rewriter -> rewriter
  val maxonce_td : rewriter -> rewriter
  val maxonce_bu : rewriter -> rewriter
  val loop : rewriter -> rewriter
end

module Rewrite (T : RewritePrimitives) : Rewrite
  with type t = T.t  =
struct
  include T

  (* Apply a sequence of rewriters in sequence, stopping as soon as
     one succeeds *)
  let any : rewriter list -> rewriter = List.fold_left T.either T.never

  (* Apply a sequence of rewriters in sequence. *)
  and all = List.fold_left T.both T.always

  (* Top-down: first the node, then its children *)
  let rec topdown rewrite : rewriter =
    let f x = topdown rewrite x in
    T.both rewrite (T.process_children f)

  (* Bottom-up: first the children, then the node *)
  let rec bottomup rewrite : rewriter =
    let f x = bottomup rewrite x in
      T.both (T.process_children f) rewrite

  (* Top-down; stop rewriting once a rewrite has been successful *)
  let rec maxonce_td rewrite : rewriter =
    let f x = maxonce_td rewrite x in
      T.either rewrite (T.process_children f)

  (* Bottom-up; stop rewriting once a rewrite has been successful *)
  let rec maxonce_bu rewrite : rewriter =
    let f x = maxonce_bu rewrite x in
      T.either (T.process_children f) rewrite


(* "the optimizations will continue until performance improves" *)

  (* Repeatedly rewrite until rewriting fails.  Return None if the
     first rewrite failed, Some e otherwise. *)
  let loop rewrite : rewriter =
    fun e ->
     let rec aux e =
       match rewrite e with
	 | None   -> e
	 | Some e -> aux e in
       match rewrite e with
	 | None   -> None
	 | Some e -> Some (aux e)
end

module SimpleRewrite
  (T : (sig type t
	    type rewriter = t -> t option
	    val process_children : rewriter -> rewriter
	end)) : RewritePrimitives
  with type t = T.t
  and type rewriter = T.t -> T.t option =
struct
  include T
    (* equivalent to let e' = (from_option e (a e)) in from_option e' (b e') *)
  let both a b e =
    match a e with
      | None   -> b e
      | Some e -> Some (Utility.from_option e (b e))
  and either a b e =
    match a e with
      | None -> b e
      | s    -> s
  and never  _ = None
  and always e = Some e
end

(** Utility for writing the process_children function in an unpleasant
    but concise style *)
let passto f exprs next =
  let rec aux passed es = function
    | [] -> passed, es
    | x::xs ->
        (match f x with
           | Some x -> aux true (x::es) xs
           | None   -> aux passed (x::es) xs) in
    match aux false [] exprs with
      | false, _ -> None
      | true,  es -> Some (next (List.rev es))

let do_rewrite f x =
  match f x with None -> x | Some y -> y
