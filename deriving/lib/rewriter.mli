module type Type = sig type a end

module type Rewriter =
  sig
    type t
    type rewriter = t -> t option
    val process_children : rewriter -> rewriter
  end

module RewriterDefaults (R : 
  sig
    type t
    type rewriter = t -> t option
    val process_children' : rewriter -> t -> (t * bool)
  end) : Rewriter with type t = R.t
