module type Rewriter =
sig 
  type t
  type rewriter = t -> t option
  val process_children : rewriter -> rewriter
end

module type Type =
sig
  type a
end

module RewriterDefaults
  (R : 
    sig
      type t
      type rewriter = t -> t option
      val process_children' : rewriter -> t -> (t * bool)
    end)
  : Rewriter with type t = R.t
             and type rewriter = R.rewriter = 
struct
  include R
  let process_children rewriter v = 
    match process_children' rewriter v with
      | (_, false) -> None
      | (x, _) -> Some x
end
