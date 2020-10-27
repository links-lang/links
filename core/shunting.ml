(* An implementation of Dijkstra's Shunting Yard algorithm. *)
open Sugartypes
open SourceCode.WithPos
open Operators

let name_of_binop = function
  | BinaryOp.Minus -> "-"
  | BinaryOp.FloatMinus -> "-."
  | BinaryOp.RegexMatch _ -> "=~"
  | BinaryOp.And -> "&&"
  | BinaryOp.Or -> "||"
  | BinaryOp.Cons -> "::"
  | BinaryOp.Name name -> name

let name_of_unop = function
  | UnaryOp.Minus -> "-"
  | UnaryOp.FloatMinus -> "-."
  | UnaryOp.Name name -> name

type 'a partial_op =
  { partial_node: (tyarg list * 'a);
    assoc: Associativity.t;
    precedence: int;
    position: SourceCode.Position.t }

type op =
  | Unary  of UnaryOp.t partial_op
  | Binary of BinaryOp.t partial_op

type exp =
  | Op of op
  | Operand of phrase

let assoc : 'a partial_op -> Associativity.t
  = fun { assoc; _ } -> assoc

let precedence : 'a partial_op -> int
  = fun { precedence; _ } -> precedence

let op_precedence : op -> int = function
  | Unary pnode -> precedence pnode
  | Binary pnode -> precedence pnode

let op_assoc : op -> Associativity.t = function
  | Unary pnode -> assoc pnode
  | Binary pnode -> assoc pnode

let partial_op : (tyarg list * 'a) -> Associativity.t -> int -> SourceCode.Position.t -> 'a partial_op
  = fun partial_node assoc precedence position ->
  { partial_node; assoc; precedence; position }

let binary : phrase -> BinaryOp.t partial_op -> phrase -> phrase
  = fun lhs op rhs ->
  make ~pos:op.position (InfixAppl (op.partial_node, lhs, rhs))

let unary : phrase -> UnaryOp.t partial_op -> phrase
  = fun exp op ->
  make ~pos:op.position (UnaryAppl (op.partial_node, exp))


module Optable = struct
  type t = (int * Associativity.t) Utility.StringMap.t
  let defaults : t =
    let open Associativity in
    let xs =
      [ "::" , (9, Right)

      ; "^"  , (8, Right)
      ; "^^" , (8, Right)
      ; "**" , (8, Right)

      ; "*"  , (7, Left)
      ; "/"  , (7, Left)
      ; "+"  , (6, Left)
      ; "*." , (7, Left)
      ; "/." , (7, Left)
      ; "+." , (6, Left)

      ; "-"  , (6, Left)
      ; "-." , (6, Left)

      ; "!"  , (6, Left)

      ; "++" , (5, Right)

      ; "=~" , (5, Right)

      ; "==" , (4, None)
      ; "<>" , (4, None)
      ; "<"  , (4, None)
      ; "<=" , (4, None)
      ; ">=" , (4, None)
      ; ">"  , (4, None)

      ; "&&" , (3, Right)

      ; "||" , (2, Right)

      ;  ">>" , (1, Left)
      ;  "$"  , (1, Right) ]
    in
    Utility.StringMap.from_alist xs

  let lookup : string -> t -> (int * Associativity.t)
    = fun name optable ->
    try Utility.StringMap.find name optable with
    | Notfound.NotFound _ -> (9, Associativity.Left)

  let add : string -> (int * Associativity.t) -> t -> t
    = fun name data optable ->
    Utility.StringMap.add name data optable
end


(* Pops an operator from a given operator stack and adds it to a given
   RPN expression. *)
let shift : op Stack.t -> exp Queue.t -> unit
  = fun opstack exps ->
  assert (not (Stack.is_empty opstack));
  let op = Stack.pop opstack in
  Queue.push (Op op) exps

(* Pops all operators from a given operator stack and adds them to a
   given RPN expression. *)
let rec shift_all : op Stack.t -> exp Queue.t -> unit
  = fun opstack exps ->
  if not (Stack.is_empty opstack)
  then (shift opstack exps; shift_all opstack exps)

(* Pushes an operator onto a given operator stack. As a side-effect
   the given RPN expression may be augmented. *)
let rec push_operator : op Stack.t -> exp Queue.t -> op -> unit
  = fun opstack exps tok ->
  (* Let 'tok' denote most recently scanned operator and 'op' be the
     top-most operator on the operator stack.

     The operator 'tok' is said to follow the operator 'op' if either
     condition is met:

     1) The precedence of 'op' is greater than the precedence of
     'tok',
     2) or 'op' and 'tok' have the same precedence and 'tok' is left
     associative. *)
  let follows : op -> op -> bool
    = fun tok op ->
    let tok_prec, tok_assoc, op_prec =
      op_precedence tok, op_assoc tok, op_precedence op
    in
    (op_prec > tok_prec)
    || (tok_prec = op_prec && tok_assoc = Associativity.Left)
  in
  if Stack.is_empty opstack
  then Stack.push tok opstack
  else if follows tok (Stack.top opstack)
  then (shift opstack exps;
        push_operator opstack exps tok)
  else Stack.push tok opstack

(* Builds an infix expression from a given RPN expression. *)
let reduce : exp Queue.t -> phrase
  = fun exps ->
  assert (Queue.length exps > 0);
  let rec loop operands exps =
    if not (Queue.is_empty exps)
    then begin (match Queue.pop exps with
                | Operand p ->
                   Stack.push p operands
                | Op (Unary pnode) ->
                   assert (Stack.length operands > 0);
                   let p = Stack.pop operands in
                   Stack.push (unary p pnode) operands
                | Op (Binary pnode) ->
                   assert (Stack.length operands > 0);
                   (* Operands are popped in reverse order. *)
                   let q = Stack.pop operands in
                   let p = Stack.pop operands in
                   Stack.push (binary p pnode q) operands);
               loop operands exps
         end
  in
  let operands = Stack.create () in
  loop operands exps;
  assert (Stack.length operands = 1);
  Stack.pop operands

(* Main idea: use two mutually recursive visitors. One visitor reorders
   infix expressions, whilst the other maps this visitor over infix
   expressions. *)

(* This class reorders infix expressions according to the precedence
   and associativity of the operators within them. *)
class reorder optable =
  object (o : 'self)
    inherit SugarTraversals.fold as _super

    (* Operator stack. *)
    val opstack :  op Stack.t = Stack.create ()
    (* The expression in Reverse Polish Notation (RPN). *)
    val exps    : exp Queue.t = Queue.create ()

    (* Reconstructs an infix expression from the resulting RPN
       expression. *)
    method reconstruct : unit -> phrase
      = fun () ->
      shift_all opstack exps;
      reduce exps

    method! phrase p =
      match node p with
      | Constant _ | Var _ ->
         Queue.push (Operand p) exps; o
      | InfixAppl ((_, op) as def, p', q') ->
         let o = o#phrase p' in
         let () =
           let prec, assoc = Optable.lookup (name_of_binop op) optable in
           let def' = partial_op def assoc prec (pos p) in
           push_operator opstack exps (Binary def')
         in
         o#phrase q'
      | UnaryAppl ((_, op) as def, p') ->
         let o = o#phrase p' in
         let () =
           let prec, assoc =
             match name_of_unop op with
             | "-" | "-." -> 9, Associativity.Right
             | _ -> Optable.lookup (name_of_unop op) optable
           in
           let def' = partial_op def assoc prec (pos p) in
           push_operator opstack exps (Unary def')
         in o
      | _ ->
         let p' = (new shunt optable)#phrase p in
         Queue.push (Operand p') exps; o

    (* This visitor should never visit any other nodes than [phrase]. *)
    method! binding _ = assert false
    method! bindingnode _ = assert false
    method! program _ = assert false

  end
  (* This visitor traverses the whole AST. On any phrase expression it
     effectively acts as a pair of enclosing parentheses, ensuring
     that left-to-right evaluation order is preserved for non-infix
     expression. *)
  and shunt optable =
      object
        inherit SugarTraversals.map as super

        (* The operator table may be updated as a result of running
           this visitor. *)
        val mutable optable = optable
        method get_optable () = optable

        method! phrase p =
          match node p with
          | InfixAppl _ | UnaryAppl _ ->
             let reorder = (new reorder optable)#phrase p in
             reorder#reconstruct ()
          | _ -> super#phrase p

        method! bindingnode = function
          | (Infix { name; assoc; precedence }) as node ->
             optable <- Optable.add name (precedence, assoc) optable;
             node
          | node -> super#bindingnode node
      end

module Untyped = struct
  let name = "shunting"

  let get_operator_table context =
    match Context.operator_table context with
    | None -> Optable.defaults
    | Some table -> table

  open Transform.Untyped
  let program state program =
    (* Printf.fprintf stderr "Before shunting:\n%s\n%!" (Sugartypes.show_program program); *)
    let context = context state in
    let optable = get_operator_table context in
    let shunter = new shunt optable in
    let program' = shunter#program program in
    (* Printf.fprintf stderr "After shunting:\n%s\n%!" (Sugartypes.show_program program'); *)
    let optable = shunter#get_optable () in
    let context' = Context.({ context with operator_table = Some optable }) in
    return context' program'

  let sentence state sentence =
    let context = context state in
    let optable = get_operator_table context in
    let shunter = new shunt optable in
    let sentence' = shunter#sentence sentence in
    let optable = shunter#get_optable () in
    let context' = Context.({ context with operator_table = Some optable }) in
    return context' sentence'
end
