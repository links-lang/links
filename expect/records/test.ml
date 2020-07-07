open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Record printing" =
  run_expr {|(x=1, y="two")|};
  [%expect {|
    (x = 1, y = "two") : (x:Int,y:String)
    exit: 0 |}]

let%expect_test "Quote record labels that are also keywords" =
  run_expr {|("client"=5, "fun"=7)|};
  [%expect {|
    ("client" = 5, "fun" = 7) : (client:Int,fun:Int)
    exit: 0 |}]

let%expect_test "Record comparisons" =
  run_expr {|(x=1, y="two") == (y="two", x=1)|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Record extension" =
  run_expr {|{var z = (y="three"); (x=4|z) }|};
  [%expect {|
    (x = 4, y = "three") : (x:Int,y:String)
    exit: 0 |}]

let%expect_test "Let pattern matching" =
  run_expr {|{var (x=a,y=(z=b)) = (y=(z=3),x="four"); a}|};
  [%expect {|
    "four" : String
    exit: 0 |}]

let%expect_test "Lambda pattern matching" =
  run_expr {|(fun((x=a,y=(z=b))) { a }) ((y=(z=3),x="four"))|};
  [%expect {|
    "four" : String
    exit: 0 |}]

let%expect_test "Projection of absent field" =
  run_expr {|(x="1").y;;|};
  [%expect {|
    ***: Parse error: <string>:1

      (x="1").y;;
                ^
    exit: 1 |}]

let%expect_test "Projections" =
  run_expr {|(y=(z=3),x="four").y.z|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Projection sections" =
  run_expr {|(.x)((y=(z=3),x="four"))|};
  [%expect {|
    "four" : String
    exit: 0 |}]

let%expect_test "Passing two different closed rows to an open-row function arg" =
  run_expr {|fun foo(x) { x.a } (foo((a="a", b=2)), foo((a=1, c=3)))|};
  [%expect {|
    ("a", 1) : (String, Int)
    exit: 0 |}]

let%expect_test "Passing two different list types to a polymorphic function" =
  run_expr {|fun foo(x) { hd(x) } (foo([1,2]), foo(['a', 'b', 'c']))|};
  [%expect {|
    (1, 'a') : (Int, Char)
    exit: 0 |}]

let%expect_test "Row types preserved across functions" =
  run_expr {|(fun (x) { var (r=r|s) = x; (r=3|s) })((r=3,s=4)).s|};
  [%expect {|
    4 : Int
    exit: 0 |}]

let%expect_test "With syntax (same type)" =
  run_expr {|((x = 3) with x = 4)|};
  [%expect {|
    (x = 4) : (x:Int)
    exit: 0 |}]

let%expect_test "With syntax (different type)" =
  run_expr {|((x = 3) with x = "four")|};
  [%expect {|
    (x = "four") : (x:String)
    exit: 0 |}]

let%expect_test "With syntax: multiple labels (a)" =
  run_expr {|((x=3,y=4) with y="four")|};
  [%expect {|
    (x = 3, y = "four") : (x:Int,y:String)
    exit: 0 |}]

let%expect_test "With syntax: multiple labels (b)" =
  run_expr {|((z='a',x=3,y=4) with x="four",y=3)|};
  [%expect {|
    (x = "four", y = 3, z = 'a') : (x:String,y:Int,z:Char)
    exit: 0 |}]

let%expect_test "With syntax (missing label)" =
  run_expr {|((x = 3) with y=4)|};
  [%expect {|
    <string>:1: Type error: A record can only be updated with compatible fields, but the expression
        `(x = 3)'
    has type
        `(x:Int)'
    while the update fields have type
        `(y:a|b::Any)'
    In expression: ((x = 3) with y=4).

    exit: 1 |}]

let%expect_test "Tables must have table type." =
  run_expr {|fun (t) { for (x <-- t) [(a=x.y)] }|};
  [%expect {|
    fun : (TableHandle((y:a::Base|_::Base),(|_::Base),(|_::Base))) {}-> [(a:a::Base)]
    exit: 0 |}]

let%expect_test "Duplicate fields" =
  run_expr {|(x=3,x=3)|};
  [%expect {|
    <string>:1: Type error: Duplicate labels (x) in record.
    In expression: (x=3,x=3).

    exit: 1 |}]

let%expect_test "Uninhabited recursive rows (questionable)" =
  run_expr {|(fun (x : (|(mu b . b))) {x})(())|};
  [%expect {|
    ***: Error: "Assert_failure core/unify.ml:1046:11"
    exit: 1 |}]

let%expect_test "Missing absent label (1)" =
  run_expr {|fun (r : (|a)) {(l=42|r)}|};
  [%expect {|
    <string>:1: Type error: Only a record can be extended, and it must be extended with different fields, but the expression
        `<dummy>'
    has type
        `(|a)'
    while the extension fields have type
        `(l-|b::Any)'
    In expression: (l=42|r).

    exit: 1 |}]

let%expect_test "Missing absent label (2)" =
  run_expr {|fun (r : (|a)) {(l=42|(r : (l-|a)))}|};
  [%expect {|
    ***: Parse error: <string>:1

      fun (r : (|a)) {(l=42|(r : (l-|a)))}
                                     ^
    exit: 1 |}]

let%expect_test "Possibly absent label in a closed row" =
  run_expr {|fun (x : (a{%b}|%c)) {x : ()}|};
  [%expect {|
    fun : (()) -> ()
    exit: 0 |}]

let%expect_test "Record field punning (1)" =
  run_expr {|var hello = "hellooo"; (=hello)|};
  [%expect {|
    (hello = "hellooo") : (hello:String)
    exit: 0 |}]

let%expect_test "Record field punning (2)" =
  run_expr {|var hello = "hellooo"; (=hello, world = 5)|};
  [%expect {|
    (hello = "hellooo", world = 5) : (hello:String,world:Int)
    exit: 0 |}]

let%expect_test "Record field punning (3)" =
  run_expr {|switch((hello=5, world=10)) { case (=hello, world=y) -> hello + y }|};
  [%expect {|
    15 : Int
    exit: 0 |}]

let%expect_test "Record field punning (4)" =
  run_expr {|var y = 2; (x=1, =y)|};
  [%expect {|
    (x = 1, y = 2) : (x:Int,y:Int)
    exit: 0 |}]

let%expect_test "Record field punning (5)" =
  run_expr {|var y = 2; (x=1, =y, z=3)|};
  [%expect {|
    (x = 1, y = 2, z = 3) : (x:Int,y:Int,z:Int)
    exit: 0 |}]

let%expect_test "Record field punning (6)" =
  run_expr {|var z = 3; (x=1, y=2, =z)|};
  [%expect {|
    (x = 1, y = 2, z = 3) : (x:Int,y:Int,z:Int)
    exit: 0 |}]

let%expect_test "Record field punning (7)" =
  run_expr {|var x = 1; var y = 2; var z = 3; (=x, =y, =z)|};
  [%expect {|
    (x = 1, y = 2, z = 3) : (x:Int,y:Int,z:Int)
    exit: 0 |}]

