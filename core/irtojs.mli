(* JavaScript code generation *)
open Utility

(** IR variable environment *)
module VEnv = Env.Int

(** Type of environments mapping IR variables to object variables *)
type venv = string VEnv.t

(** Intermediate language *)
module Code: sig
  module Var: sig
    type t = string
    [@@deriving show]
  end

  module Label: sig
    type t = string
    [@@deriving show]
  end

  type t = Var     of Var.t
         (** [Var] denotes a variable, e.g. x, foo, etc *)
         | Lit     of string
         (** [Lit] denotes an boolean, float, integer, or string
            literal, e.g. true, 3.14, 42, "Hello World!" *)
         | Fn      of Var.t list * t
         (** [Fn] denotes an anonymous function, e.g. function(x) {
            return x; }. The first component [Var.t list] is the
            formal parameter list and the second component [t] is the
            body. *)
         | LetFun  of (Var.t * Var.t list * t * Ir.location) * t
         (** [LetFun] denotes a named function, e.g.
               function add(x, y) { return x + y; }.
            The first component is a quadruple where: [Var.t] is the
            name of the function, [Var.t list] is the formal parameter
            list, [t] is the body, and [Ir.location] is the function
            location (client, server, or unknown). The second component
            [t] is the continuation of the function binding. *)
         | LetRec  of (Var.t * Var.t list * t * Ir.location) list * t
         (** [LetRec] denotes a group of named mutually recursive
            functions, e.g.
                function fac(n) { if (n === 0) return fac0();
                                  else return n * fac(n-1) }
                function fac0() { return 0; }.
             A [LetRec] is essentially a list of [LetFun]s. *)
         | Call    of t * t list
         (** [Call] denotes a function call, e.g. f(1,"foo"); where
            [t] is the function expression (abstractor) and [t list]
            is the argument list. *)
         | Unop    of Var.t * t
         (** [Unop] denotes a unary expression, e.g. !true; where
            [Var.t] is the name of the unary operator applied to the
            expression [t]. *)
         | Binop   of t * Var.t * t
         (** [Binop] denotes an infix binary operator expression,
            e.g. 2 + 40. The first [t] is the left hand side of the
            binary operator, [Var.t] is the name of the operator, and
            the second [t] is the right hand side of the operator. *)
         | If      of t * t * t
         (** [If] denotes a conditional statement, e.g.
               if (cond) print("true"); else print("false")
            where the first [t] is the condition-expression, the
            second [t] is the then-branch/true-branch statement,
            and the third [t] is the else-branch/false-branch statement. *)
         | Switch  of t * t stringmap * t option
         (** [Switch] denotes a switch statement, e.g.
               switch(s) {
                 case "Foo": doSomething();
                             break;
                 case "Bar": doSomethingElse();
                             break;
                 default: otherwiseDoThis();
               }
            where [t] is the scrutinee-expression, [t stringmap] is a
            mapping from string literals to statements, i.e. case
            statements, and [t option] is the default statement. *)
         | Dict    of (Label.t * t) list
         (** [Dict] denotes a literal object in JSON, e.g.
                 {'foo': 42, 'bar': false}
            where [(Label.t * t) list] is an association list/mapping
            from string literals to expressions. *)
         | Arr     of t list
         (** [Arr] denotes an array, e.g. [1,2,3,4,5]. The [t list] is
            the elements of the array. *)
         | Project of t * Label.t
         (** [Project] denotes a component selection, e.g.
                 obj.baz, obj[1]
            where [t] is an expression and [Label.t] is the name of
            component being selected. *)

         | Bind    of Var.t * t * t
         (** [Bind] denotes a variable binding, e.g.
               let x = 42; let y = true; doSomething(x, y)
            where [Var.t] is the name of the variable, the first [t]
            is the expression/value being bound, and the second [t] is
            the continuation of the binding. *)
         | Return  of t
         (** [Return] denotes a return statement, e.g.
                return 42;
             where [t] is an expression. *)

         | InlineJS of string
         (** [InlineJS] is a convenience mechanism for inlining
            handcrafted JavaScript into generated JavaScript
            code. Don't use otherwise you know what you are doing --
            and even then, don't use it. *)

         | Nothing
        (** [Nothing] denotes the empty code. It can be used to end a binding
            sequence whose continuation is empty. *)
[@@deriving show]

  module MetaContinuation: sig
    type nonrec t = (t -> t)
    val identity : t
  end
end

module type JS_PAGE_COMPILER = sig
  val generate_program : venv -> Ir.computation -> venv * Code.t
  val generate_stubs : Value.env -> Ir.binding list -> Code.MetaContinuation.t
  val generate_toplevel_bindings : Value.env -> Json.json_state -> venv -> Ir.binding list -> Json.json_state * venv * string list * Code.MetaContinuation.t
  val wrap_with_server_lib_stubs : Code.t -> Code.t
  val primitive_bindings : Code.t
end

module Compiler : JS_PAGE_COMPILER

module type JS_CODEGEN = sig
  val string_of_js : Code.t -> string
  val output : out_channel -> Code.t -> unit
end
module Js_CodeGen : JS_CODEGEN
