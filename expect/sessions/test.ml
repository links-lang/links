open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Access points" =
  run_expr {|fun f(ap) {send(42, request(ap))} f|};
  [%expect {|
    fun : (AP (?(Int).~a::Session)) ~> a::Session
    exit: 0 |}]

let%expect_test "Linear function annotation" =
  run_expr {|sig h : ((a::Any) -e-@ a::Any, a::Any) -e-> a::Any fun h(f, x) {f(x)}|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Non-linear use of linear function" =
  run_expr {|sig h : ((a::Any) -e-@ a::Any, a::Any) -e-> a::Any fun h(f, x) {f(f(x))}|};
  [%expect {|
    <string>:1: Type error: Variable f has linear type
        `(a::Any) -b-@ a::Any'
    but is used 2 times.
    In expression: fun h(f, x) {f(f(x))}.

    exit: 1 |}]

let%expect_test "Linear identity" =
  run_expr {|fun (x) {x}|};
  [%expect {|
    fun : (a::Any) -> a::Any
    exit: 0 |}]

let%expect_test "Non-linear dup" =
  run_expr {|fun (x) {(x, x)}|};
  [%expect {|
    fun : (a) -> (a, a)
    exit: 0 |}]

let%expect_test "Receive value" =
  run_expr {|fun (c) {receive(c).1}|};
  [%expect {|
    fun : (?(a::Any)._::(Unl,Session)) ~> a::Any
    exit: 0 |}]

let%expect_test "Receive channel" =
  run_expr {|fun (c) {receive(c).2}|};
  [%expect {|
    fun : (?(_).b::Session) ~> b::Session
    exit: 0 |}]

let%expect_test "Ignore send" =
  run_expr {|fun (c) {ignore(send(42, c))}|};
  [%expect {|
    fun : (!(Int)._::(Unl,Session)) ~> ()
    exit: 0 |}]

let%expect_test "Linear end" =
  run_expr {|ignore(request((new(): AP(End))))|};
  [%expect {|
    <string>:1: Type error: The function
        `ignore'
    has type
        `(a) -b-> ()'
    while the arguments passed to it have types
        `~End'
    and the currently allowed effects are
        `wild'
    In expression: ignore(request((new(): AP(End)))).

    exit: 1 |}]

let%expect_test "Non-linear generalisation (1)" =
  run_expr {|{var x = A; ()}|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Non-linear generalisation (2)" =
  run_expr {|fun (r) {var (x=42|q) = r; ()}|};
  [%expect {|
    fun : ((x:Int|_)) -> ()
    exit: 0 |}]

