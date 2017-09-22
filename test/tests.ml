open Test

let working_tests = [
  (* annotations *)

  "Type annotation that matches inference",
  "fun (x) {x} : (a) -> a",
  is_function ~with_type: "(a) -> a";

  "Too-general type annotation",
  "fun (x) {x+1} : (a) -> a",
  has_typeerror;

  "Annotations inside functions [1]",
  "fun (x:a) { x:a } : (a) -> a",
  is_function ~with_type: "(a) -> a";

  "Annotations inside functions [2]",
  "fun (x:a) { x:a } : (b) -> b",
  has_typeerror;

  "Annotations inside functions [3]",
  "fun (x:a) { error(\"boo\") } : (a) -> b",
  is_function ~with_type: "(a) -> b";

  "Nested scopes",
  "{ var x = 3; ({ var x = 4; x }, x)}",
  result "(4, 3)" ~with_type:"(Int,Int)";

  "Non-recursive top-level functions:",
  "var f = fun(x) { f(x) };",
  has_typeerror;

  "Non-recursive block-scope functions:",
  "{ var f = fun(x) { f(x) }; () }",
  has_typeerror;

  "Mutually recursive top-level functions",
  "fun evn(n) { n == 0 || od(n - 1) } fun od(n) { evn(n) == false } evn(20)",
  result "true" ~with_type:"Bool";

  "Mutually recurisve nested functions",
  "{ fun even(n) { n == 0 || odd(n - 1) } fun odd(n) { even(n) == false } even(20) }",
  result "true" ~with_type:"Bool";

  "Closures using anonymous functions",
  "fun addn(n) { fun(x) { x + n } } addn(3)(4)",
  result "7" ~with_type:"Int";

  "Closures using named functions",
  "fun addn(n) { fun f(x) { x + n } f } addn(3)(4)",
  result "7" ~with_type:"Int";

  "Closures where the environment contains a closure from a different scope",
  "fun add(x,y){x+y} fun baz(z, w) {z + w} fun foo(f, x) { fun bar(y) { f(3, y) } bar(x) } foo(add,4)",
  result "7" ~with_type:"Int";

  "as patterns",
  "{var x::xs as y = [1,2,3]; y}",
  result "[1, 2, 3]" ~with_type:"[Int]";

  "Check that recursive bindings don't destroy the local environments of values in the local environment (see bug report from Thierry, 2006-09-17 on links-users)",
  "fun (z) { fun s() {} z()}(fun (aa)() { aa(()) }(fun (x){x}))",
  result "()" ~with_type:"()";

  "Bug in interaction between pattern-matching and anonymous functions",
  "(fun (l) { switch (l) { case x::xs -> fun (x) { x } } })([1])(2)",
  result "2" ~with_type:"Int";

  (* booleans *)

  "Boolean not(true)",
  "not(true)",
  result "false" ~with_type: "Bool";

  "Boolean not(false)",
  "not(false)",
  result "true" ~with_type: "Bool";

  "Predefined 'javascript' value",
  "javascript",
  result "false" ~with_type: "Bool";

  (* collections *)

  "Iteration",
  "for (var i <- [1,2,3,4,5]) if (i == 3) [] else [i, i]",
  result "[1, 1, 2, 2, 4, 4, 5, 5]" ~with_type: "[Int]";

  "Where clause",
  "for (var i <- [1,2,3,4,5]) where (i <> 3) [i, i]",
  result "[1, 1, 2, 2, 4, 4, 5, 5]" ~with_type: "[Int]";

  "Concatenation/union",
  "[1,2,3] ++ [4,5]",
  result "[1, 2, 3, 4, 5]" ~with_type: "[Int]";

  "Head and tail",
  "hd(['a','b','c']) == 'a'",
  result "true" ~with_type: "Bool";

  "Equality",
  "[1,2,3] == [1,2,3] && [1,2,3] <> [2,1,3]",
  result "true" ~with_type: "Bool";

  "Cons",
  "1 :: 2 :: 3 :: 4 :: []",
  result "[1, 2, 3, 4]" ~with_type: "[Int]";

  "Let-patterns [1]",
  "{var x :: xs = [1,2,3] ; x}",
  result "1" ~with_type: "Int";

  "Let-patterns [2]",
  "{var (x :: y :: xs, z) = ([1,2,3],4) ; (x,y)}",
  result "(1, 2)" ~with_type: "(Int, Int)";

  "Let-patterns [3]",
  "{var ((v,w) :: (x,y) :: z) = [(1,2),(3,4)] ; (w,x)}",
  result "(2, 3)" ~with_type: "(Int, Int)";

  "Let-patterns [4]",
  "{var (x,y) :: [] = [(1,2),(3,4),(4,5)] ; (x,y)}",
  fails_at_runtime "(Int, Int)";

  "Let-patterns [5]",
  "{var [(x,y)] = [(1,2),(3,4),(4,5)] ; (x,y)}",
  fails_at_runtime "(Int, Int)";

  "Let-patterns [6]",
  "{var [] = [1,2,3] ; ()}",
  fails_at_runtime "()";

  "Let-patterns [7]",
  "{var [x] = [] ; ()}",
  fails_at_runtime "()";

  "Let-patterns [8]",
  "{var x::y = [] ; ()}",
  fails_at_runtime "()";

  "Case-patterns [1]",
  "switch ([]) { case [] -> 1 }",
  result "1" ~with_type: "Int";

  "Case-patterns [2]",
  "switch ([]) { case x::xs -> 1 }",
  fails_at_runtime "Int";

  "Case-patterns [3]",
  "switch ([]) { case [] -> 2 case x -> 1 }",
  result "2" ~with_type: "Int";

  "Case-patterns [4]",
  "switch ([]) { case x::xs -> 1 case x -> 2 }",
  result "2" ~with_type: "Int";

  "Case-patterns [5]",
  "switch ([1]) { case x::xs -> 1  case x -> 2 }",
  result "1" ~with_type: "Int";

  "Case-patterns [6]",
  "switch ([1,3]) { case x::y::z -> 3 case x::y -> 2 case x -> 1 }",
  result "3" ~with_type: "Int";

  "Case-patterns [7]",
  "switch ([1]) { case x::y::z -> 3 case x::y -> 2 case x -> 1 }",
  result "2" ~with_type: "Int";

  "Case-patterns [8]",
  "switch ([1,3]) { case x::y::[] -> 3 case x::y -> 2 case x -> 1 }",
  result "3" ~with_type: "Int";

  "Case-patterns [9]",
  "switch ([1]) { case x::[] -> 2 case x -> 1 }",
  result "2" ~with_type: "Int";

  "Case-patterns (redundancy) [10]",
  "fun (x) { switch (x) { case ([], []) -> 1 case (x::xs, y::ys) -> 2 case ([], _) -> 3 case (_, []) -> 4 }}",
  is_function ~with_type: "(([a],[b])) -> Int";

  "Case-patterns (singleton list pattern)",
  "switch ([1]) { case [x] -> 2 case x -> 1 }",
  result "2" ~with_type: "Int";

  "With parentheses:",
  "switch ([1]) { case (x::xs) -> 1  case x -> 2 }",
  result "1" ~with_type: "Int";

  "Length function",
  "fun len (l) { switch (l) { case [] -> 0 case x::xs -> 1 + len(xs) } } len ([1,2,3])",
  result "3" ~with_type: "Int";

  "Sorting:",
  "for (var i <- [2,1,3]) orderby (i) [i]",
  result "[1, 2, 3]" ~with_type: "[Int]";

  "Sorting:",
  "for (var i <- [2,1,3]) orderby (-i) [i]",
  result "[3, 2, 1]" ~with_type: "[Int]";

  "Empty-list comparison (1)",
  "[] < []",
  result "false" ~with_type: "Bool";

  "Empty-list comparison (2)",
  "[] > []",
  result "false" ~with_type: "Bool";

  "List comparison (1)",
  "[1] < []",
  result "false" ~with_type: "Bool";

  "List comparison (2)",
  "[1] > []",
  result "true" ~with_type: "Bool";

  (* conditionals *)

  "Conditional expressions",
  "if (true) \"yes\" else \"no\"",
  result "\"yes\"" ~with_type: "String";

  "Typing of the test",
  "if (3) \"yes\" else \"no\"",
  has_typeerror;

  "Typing of both branches",
  "if (true) \"yes\" else 41",
  has_typeerror;

  "Conditionals in polymorphic functions",
  "(fun (a, b, c) { if (a) [b] else [c] })(true, \"three\", \"four\")",
  result "[\"three\"]" ~with_type: "[String]";

  "Logical operators",
  "true && (false || true)",
  result "true" ~with_type: "Bool";

  "Logical operator short-circuiting",
  "(true || (1 / 0) == 0, false && (1 / 0) == 0)",
  result "(true, false)" ~with_type: "(Bool, Bool)";

  (* continuations *)

  "Escape expressions",
  "escape e in {2+2}",
  result "4" ~with_type: "Int";

  "Nested escapes",
  "escape return in {1+(escape e in {2+2})}",
  result "5" ~with_type: "Int";

  "Invoking escapes",
  "escape return in {1+(escape e in {return(2+2)})}",
  result "4" ~with_type: "Int";

  "Continuation typing [1]",
  "escape e in {if (false) 1+e else 2}",
  has_typeerror;

  "Continuation typing [2]",
  "escape e in { e(1) }",
  result "1" ~with_type: "Int";

  "continuation mailbox typing (see r321) ",
  "fun () {(escape e in {spawn { e(self()) }}) ! \"\"; 1 + recv(); }",
  has_typeerror;

  (* database *)

  "Orderby clause (not a semantic test, just syntax)",
  "for (var x <- [1, 2, 3]) orderby (x) [x]",
  result "[1, 2, 3]" ~with_type: "[Int]";

  "Function typing bug (see jdy's blog, 2005-10-24)",
  "(fun (x,y) { [x] ++ [y] }) ([1],\"a\")",
  has_typeerror;

  "Type annotations on functions",
  "fun (x) { x } : (String) -> String",
  result "fun" ~with_type: "(String) -> String";

  "Incorrect type annotations rejected",
  "fun (x) { x + 1 } : (Float) -> String",
  has_typeerror;

  "Loose type annotations on functions",
  "fun (x) { x } : (b) -> b",
  is_function ~with_type: "(a) -> a";

  "Trailing semicolon means \"ignore the final value\" [1]",
  "{ 2 }",
  result "2" ~with_type: "Int";

  "Trailing semicolon means \"ignore the final value\" [2]",
  "{ 2; }",
  has_typeerror;

  "Trailing semicolon means \"ignore the final value\" [3]",
  "fun () { 2 }",
  result "fun" ~with_type: "() -> Int";

  "Trailing semicolon means \"ignore the final value\" [4]",
  "fun () { 2; }",
  has_typeerror;

  "Type annotations",
  "fun (x:Int) {x:Int}",
  result "fun" ~with_type: "(Int) -> Int";

  "Identity annotation",
  "fun (x:a) {x:a}",
  is_function ~with_type: "(a) -> a";

  "Type annotation scope",
  "fun (x:a, y:a) {(x, y)}",
  is_function ~with_type: "(a,a) -> (a,a)";

  "Negative recursive type",
  "fun (x) {x:a} : a",
  has_typeerror;

  "Typename [1]",
  "typename Foo = Int; fun f(x:Foo) {x} f(1)",
  result "1" ~with_type: "Foo";

  "Typename [2]",
  "typename Bar(a,b,c) = (a,b,c); fun f(x:Bar(a,b,c)) {x} f((false, 1, \"two\"))",
  result "(false, 1, \"two\")" ~with_type: "Bar (Bool,Int,String)";

  "Typename [3]",
  "typename F(a) = (a) -> a; sig f : F(Int) fun f(x) {recv() + x} sig g : F(Int) fun g(x) {stringToInt(recv()) + x} g",
  result "fun" ~with_type: "(Int) -{String}-> Int";

  "Correct message type sent to a process",
  "spawn { recv() + 1 } ! 1",
  result "()" ~with_type: "()";

  "Incorrect message type sent to a process is a type error",
  "spawn { { recv() + 1} } ! \"two\"",
  has_typeerror;

  "Receive types must unify (correct, closed rows)",
  "fun f() { receive { case Bar -> () }} fun g() { receive { case Bar -> () }} fun () { f(); g() }",
  is_function ~with_type: "() -{[|Bar:()|]}-> ()";

  "Receive types must unify (incorrect, closed rows)",
  "fun f() { receive { case Bar -> () }} fun g() { receive { case Foo -> () }} fun () { f(); g() }",
  has_typeerror;

  "Receive types must unify (correct, open rows)",
  "fun f() { receive { case Bar -> () case x -> () }} fun g() { receive { case Foo -> () case x -> () }} fun () { f(); g() }",
  is_function ~with_type: "() -{[|Bar:()|Foo:()|a|]}-> ()";

  "Receive types must unify (incorrect, open rows)",
  "fun f() { receive { case Bar (x) -> x+1 case x -> 0 }} fun g() { receive { case Bar (s) -> s+.1. case x -> 0.0 }} fun () { f(); g() }",
  has_typeerror;

  "Basic send/receive test.",
  "var p = spawn { recv() ! \"The end\" } ! self(); recv()",
  result "\"The end\"" ~with_type: "String";

  "Mailboxes are not polymorphic [1]",
  "var p = spawn { recv() ! \"The end\" } ! self(); recv()",
  result "\"The end\"" ~with_type: "String";

  "Mailboxes are not polymorphic [2]",
  "var pid = spawn { recv() ++ [] }; {pid ! \"one\"; pid ! [2]}",
  has_typeerror;

  "Built-in functions are polymorphic in their mailbox types",
  "fun f() {var x = recv(); intToString(x)} fun g(x) {var () = recv(); intToString(x)} (f, g)",
  result "(fun, fun)" ~with_type: "(() -{Int}-> String, (Int) -{()}-> String)";

  (* numbers *)

  "Arbitrary precision arithmetic",
  "{ fun fact(n) { if (n == 0) 1 else n * fact(n-1) } fact(100)}",
  result "93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000" ~with_type:"Int";

  "Truncating integer division",
  "2/3 + 1/3",
  result "0" ~with_type: "Int";

  "Incompatibility of float and int types",
  "1 + 2.0",
  has_typeerror;

  "Incompatibility with everything else",
  "1 == \"two\"",
  has_typeerror;

  "Conversions between float and int types",
  "intToFloat(3)",
  result "3." ~with_type: "Float";

  "Integer literals",
  "200300400500600700200300400500600700200300400500600703",
  result "200300400500600700200300400500600700200300400500600703" ~with_type: "Int";

  "Floating point literals",
  "3.14",
  result "3.14" ~with_type:"Float";

  "Unary negation [1]",
  "{var x = 3; -x}",
  result "-3" ~with_type: "Int";

  "Unary negation [2]",
  "{var x = 21342; x == - -x}",
  result "true" ~with_type:"Bool";

  "Prefix arithmetic operators",
  "{var plus = (+); plus(1, (+)(2,3))}",
  result "6" ~with_type: "Int";

  "Operator precedence [1]",
  "(2 + 3 * 4 == 14) && (2 * 3 + 4 == 10)",
  result "true" ~with_type: "Bool";

  "Error on division by zero",
  "{var x = 10; x / (x - 10)}",
  fails_at_runtime "Int";

  "Equality and comparisons [integer comparison]",
  "({var x = 100000230 * 102300000; x < x + 1}) && ({var x = 1032452430 * 102300234234; x > x - 1})",
  result "true" ~with_type: "Bool";

  "Equality and comparisons [integer equality]",
  "({var x = 100000230 * 102300000; x == 10230023529000000}) && ({var x = 1032452430 * 102300234234; x == 105620125424462488620})",
  result "true" ~with_type: "Bool";

  "Equality and comparisons [floating comparison]",
  "(-.10.0 < -.9.5 && 10.0 > 9.5 && 10.0 > -.9.5 && -.10.0 < 9.5)",
  result "true" ~with_type: "Bool";

  (* patterns *)

  "Nested variant matching [1]",
  "switch (A (A)) { case A (A) -> 0 case A (B) -> 1 }",
  result "0" ~with_type: "Int";

  "Nested variant matching [2]",
  "switch (A (A)) { case A (B) -> 0 case A (A) -> 1 }",
  result "1" ~with_type: "Int";

  "Constant patterns [1]",
  "switch (A (1)) { case A (0) -> 0 case A (1) -> 1 }",
  result "1" ~with_type: "Int";

  "Constant patterns [2]",
  "switch (A (1)) { case A (0) -> 0 case _ -> 1 }",
  result "1" ~with_type: "Int";

  "Constant patterns [3]",
  "switch (A (1)) { case A (0) -> 0 case A (x) -> 1 }",
  result "1" ~with_type: "Int";

  "Default pattern",
  "switch (A) { case _ -> 0 }",
  result "0" ~with_type: "Int";

  "Integer pattern",
  "switch (0) { case x -> x+1 }",
  result "1" ~with_type: "Int";

  "List pattern",
  "switch (A ([])) { case A (_::_) -> 0 case A ([]) -> 1 }",
  result "1" ~with_type: "Int";

  "HasType pattern [1]",
  "switch (1) {case (0:Int) -> 0 case (1:Int) -> 1}",
  result "1" ~with_type: "Int";

  "HasType pattern [2]",
  "switch (1) {case (1:Int) -> 1 case (x:String) -> 0}",
  has_typeerror;

  "HasType pattern [3]",
  "switch (A) {case A -> 0 case (x:[|B:[|C:String|] |]) -> 1}",
  result "0" ~with_type: "Int";

  "HasType pattern [4]",
  "switch (A) {case A -> 0 case B(C(x:String)) -> 1}",
  result "0" ~with_type: "Int";

  "As pattern",
  "switch (1) {case 1 as x -> x}",
  result "1" ~with_type: "Int";

  "Absence typing in variant patterns",
  "fun f(x) {switch (x) {case A(B) -> B case A(y) -> A(f(y))}} f",
  is_function ~with_type:"([|A:[|B:()|(mu d . A:[|B:()|d|])|]|]) -> mu c . [|A:c | B:()|b|]";

  "Type-based redundant pattern",
  "fun (x) {switch (x) { case(true) -> 0 case(false) -> 1 case(y) -> 2}}",
  result "fun" ~with_type: "(Bool) -> Int";

  "Pattern matching twice against the same expression",
  "fun (x) {(switch (x) {case A -> 0 case _ -> 1}) + (switch (x) {case A -> 0 case _ -> 1})}",
  is_function ~with_type:"([|A:()|b|]) -> Int";

  (* polymorphism *)

  "Let polymorphism [1]",
  "fun pair(x) { (x, x) } (pair('a'), pair(13))",
  result "(('a', 'a'), (13, 13))" ~with_type: "((Char, Char), (Int, Int))";

  "Let polymorphism [2]",
  "{fun pair(x) { (x, x) } (pair('a'), pair(13))}",
  result "(('a', 'a'), (13, 13))" ~with_type: "((Char, Char), (Int, Int))";

  "Monomorphic lambda bindings",
  "(fun (p) { (p('a'), p(13))})(fun (x) { (x,x) })",
  has_typeerror;

  "No polymorphic recursion without signatures",
  "fun f(x) { f(\"a\"); f(1); 1} f",
  has_typeerror;

  "Invalid \"polymorphic recursion\"",
  "sig f : (a) -> Int fun f(x) { x == 1; f(\"a\"); f(1); 1 } f",
  has_typeerror;

  "Polymorphic mutual recursion [4]",
  "sig f : (a) -> Int fun f(x) { g(\"a\"); g(1); 1 } sig g : (a) -> Int fun g(x) { x == 1; f(\"a\"); f(1); 1 } f",
  has_typeerror;

  "Polymorphic functions",
  "fun cons(x,l) {[x] ++ l} (cons(5, [6]), cons(\"a\", [\"b\"]))",
  result "([5, 6], [\"a\", \"b\"])" ~with_type: "([Int], [String])";

  "Polymorphic functions in different function bodies",
  "fun cons(x,l) {[x] ++ l} fun f() { cons(5, [6]) } fun g() { cons(\"a\", [\"b\"]) } (f(), g())",
  result "([5, 6], [\"a\", \"b\"])" ~with_type: "([Int], [String])";

  "Polymorphic functions in different function bodies (not in call order)",
  "fun f() { cons(5, [6]) } fun cons(x,l) {[x] ++ l} fun g() { cons(\"a\", [\"b\"]) } (f(), g())",
  result "([5, 6], [\"a\", \"b\"])" ~with_type: "([Int], [String])";

  "Value restriction",
  "sig f : (a) -> a var f = id(id); f",
  has_typeerror;

  (* records *)

  "Record printing",
  "(x=1, y=\"two\") ",
  result "(x=1,y=\"two\")" ~with_type: "(x:Int,y:String)";

  "Record comparisons",
  "(x=1, y=\"two\") == (y=\"two\", x=1)",
  result "true" ~with_type: "Bool";

  "Record extension",
  "{var z = (y=\"three\"); (x=4|z) }",
  result "(x=4,y=\"three\")" ~with_type: "(x:Int,y:String)";

  "Let pattern matching",
  "{var (x=a,y=(z=b)) = (y=(z=3),x=\"four\"); a}",
  result "\"four\"" ~with_type: "String";

  "Lambda pattern matching",
  "(fun((x=a,y=(z=b))) { a }) ((y=(z=3),x=\"four\"))",
  result "\"four\"" ~with_type: "String";

  "Projections",
  "(y=(z=3),x=\"four\").y.z",
  result "3" ~with_type: "Int";

  "Projection sections",
  "(.x)((y=(z=3),x=\"four\"))",
  result "\"four\"" ~with_type: "String";

  "Passing two different closed rows to an open-row function arg",
  "fun foo(x) { x.a } (foo((a=\"a\", b=2)), foo((a=1, c=3)))",
  result "(\"a\", 1)" ~with_type: "(String, Int)";

  "Passing two different list types to a polymorphic function",
  "fun foo(x) { hd(x) } (foo([1,2]), foo(\"abc\"))",
  result "(1, 'a')" ~with_type: "(Int, Char)";

  "Row types preserved across functions",
  "(fun (x) { var (r=r|s) = x; (r=3|s) })((r=3,s=4)).s",
  result "4" ~with_type: "Int";

  "With syntax (same type)",
  "((x = 3) with x = 4)",
  result "(x=4)" ~with_type: "(x:Int)";

  "With syntax (different type)",
  "((x = 3) with x = \"four\")",
  result "(x=\"four\")" ~with_type: "(x:String)";

  "With syntax: multiple labels (a)",
  "((x=3,y=4) with y=\"four\")",
  result "(x=3,y=\"four\")" ~with_type: "(x:Int,y:String)";

  "With syntax: multiple labels (b)",
  "((z='a',x=3,y=4) with x=\"four\",y=3)",
  result "(x=\"four\",y=3,z='a')" ~with_type: "(x:String,y:Int,z:Char)";

  "With syntax (missing label)",
  "((x = 3) with y=4)",
  has_typeerror;

  "head and tail",
  "(hd(\"string\"), tl(\"string\")) == ('s', \"tring\")",
  result "true" ~with_type: "Bool";

  "list syntax",
  "['a', 'b', 'c']",
  result "\"abc\"" ~with_type: "String";

  "hex literals",
  "\"L\x69nks\"",
  result "\"Links\"" ~with_type: "String";

  "octal literals",
  "\"L\\151nks\"",
  result "\"Links\"" ~with_type: "String";

  "concatenation",
  "\"abc\" ++ \"def\"",
  result "\"abcdef\"" ~with_type: "String";

  "comparision",
  "\"a\" < \"z\"",
  result "true" ~with_type: "Bool";

  (* tuples *)

  "Tuple printing and typing",
  "(1,\"boo\")",
  result "(1, \"boo\")" ~with_type: "(Int, String)";

  "Tuple equality",
  "(1,\"foo\") == (1, \"foo\")",
  result "true" ~with_type: "Bool";

  "Tuple inequality",
  "(1,\"foo\") == (2, \"foo\")",
  result "false" ~with_type: "Bool";

  "Tuple comparisons [exhaustive]",
  "((1,\"foo\") < (2, \"foo\")) || ((1,\"foo\") > (2, \"foo\")) || ((1,\"foo\") == (2, \"foo\"))",
  result "true" ~with_type: "Bool";

  "Tuple comparisons [exclusive]",
  "(((1,\"foo\") < (2, \"foo\")) == false) || (((1,\"foo\") > (2, \"foo\")) == false)",
  result "true" ~with_type: "Bool";

  "Tuple patterns [match]",
  "{var ((x,y,(z,a,b)), c) = ((\"string\",3.21,(15,[1,2,3],(3,2,1))), ());  z}",
  result "15" ~with_type: "Int";

  "Tuple patterns [no match]",
  "{var ((x,y,(z,a,b)), c) = ((\"string\",3.21,(15,(3,2,1))), ());  z}",
  has_typeerror;

  "Tuple/record interchangeability",
  "(1,\"two\",3) == (2=\"two\", 3=3,1=1)",
  result "true" ~with_type: "Bool";

  "Tuple extension",
  "{var x = (1,\"two\"); (3='3'|x) }",
  result "(1, \"two\", '3')" ~with_type: "(Int, String, Char)";

  "Tuple projection",
  "((1,\"two\",3).2 == \"two\")",
  result "true" ~with_type: "Bool";

  "1-tuples",
  "(1=\"one\")",
  result "(1=\"one\")" ~with_type: "(1:String)";

  "Quasituples",
  "(1=\"foo\", 3=\"bar\")",
  result "(1=\"foo\",3=\"bar\")" ~with_type: "(1:String,3:String)";

  (* variants *)

  "Construction",
  "Foo",
  result "Foo()" ~with_type: "[|Foo:()|a|]";

  "Nested Construction",
  "Foo (Bar)",
  result "Foo(Bar())" ~with_type:"[|Foo:[|Bar:()|a|]|b|]";

  "Nested Construction + argument (nary)",
  "This(Is(A(Valid(Links(Program(42))))))",
  result "This(Is(A(Valid(Links(Program(42))))))" ~with_type: "[|This:[|Is:[|A:[|Valid:[|Links:[|Program:Int|a|]|b|]|c|]|d|]|e|]|f|]";

  "Trivial closed case",
  "fun (x) { switch (x) { case A(b) -> b } }",
  is_function ~with_type:"([|A:b|]) -> b";

  "Variant matching : Closed case type error",
  "fun () { switch (C (3)) { case A (a) -> a case B (b) -> b } }",
  has_typeerror;

  "Variant matching - Closed case, immediate value [1]",
  "switch (A(3)) { case A(a) -> a case B(b) -> b }",
  result "3" ~with_type: "Int";

  "Variant matching - Closed case, immediate value [2]",
  "switch (B(3)) { case A(a) -> a case B(b) -> b }",
  result "3" ~with_type: "Int";

  "Variant matching - Closed case, immediate value [3]",
  "switch (L(3)) { case L(x) -> x case M(y) -> y }",
  result "3" ~with_type: "Int";

  "Variant matching - Closed case in function [1]",
  "fun (f) { switch (f) { case A(a) -> not(a) case B(b) -> b } }",
  is_function ~with_type: "([|A:Bool|B:Bool|]) -> Bool";

  "Variant matching - Closed case in function [2]",
  "fun (f) { switch (f) { case A(a) -> not(a) case B(b) -> true } }",
  is_function ~with_type: "([|A:Bool|B:c|]) -> Bool";

  "Variant matching - Closed case in function [3]",
  "fun (f) { switch (f) { case B(a) -> not(a) case A(b) -> b } }",
  is_function ~with_type: "([|A:Bool|B:Bool|]) -> Bool";

  "Variant matching - Open case \"immediate value\" [1]",
  "switch (A(true)) { case A(a) -> a case B(b) -> b case c -> false }",
  result "true" ~with_type: "Bool";

  "Variant matching - Open case \"immediate value\" [2]",
  "switch (C(true)) { case A(a) -> a case B(b) -> b case c -> false }",
  result "false" ~with_type: "Bool";

  "Variant matching - Open case in function",
  "fun (f) { switch (f) { case A (a) -> a case B (b) -> b case c -> false } }",
  is_function ~with_type: "([|A:Bool|B:Bool|b|]) -> Bool";

  "Recursive variant types [1]",
  "fun (x) { switch (x) { case A(a) -> a case y -> x } }",
  is_function ~with_type: "(mu b . [|A:b|c|]) -> mu b . [|A:b|c|]";


  "Recursive variant types [2]",
  "fun increment(x) { switch (x) { case Zero -> Succ (Zero) case Succ (n) -> Succ ((increment(n))) }} increment",
  is_function ~with_type: "(mu d . [|Succ:d | Zero:()|]) -> [|Zero:()|(mu c . Succ:[|Zero:()|c|]|a)|]";

  "Recursive variant types [3]",
  "fun rev(x, r) { switch (x) { case Empty -> r case Cons(a, b) -> rev(b, Cons(a, r)) }} rev",
  is_function ~with_type: "(mu e . [|Cons:(a, e) | Empty:()|], mu f . [|Cons:(a, f)|b|]) -> mu d . [|Cons:(a, d)|b|]";

  "Recursive variant types [4]",
  "fun increment(x) { switch (x) { case Zero -> Succ (Zero) case Succ (n) -> Succ (increment(n))}} fun (x) {switch (increment(x)) { case Foo -> 0 case Zero -> 1 case Succ (n) -> 2 }}",
  is_function ~with_type: "(mu b . [|Succ:b | Zero:()|]) -> Int";

  "Recursive variant types [5]",
  "fun increment(x) {
      switch (x) {
         case Zero -> Succ (Zero)
         case Succ (n) -> Succ (increment(n))
      }
    }
    increment(increment(Zero))",
  result "Succ(Succ(Zero()))" ~with_type: "[|Zero:()|(mu a . Succ:[|Zero:()|a|]|b)|]";

  "Rows preserved across functions",
  "fun f(x) { switch (x) { case Foo -> Bar case s -> s } } f",
  is_function ~with_type: "([|Bar:() | Foo:()|b|]) -> [|Bar:() | Foo- |b|]";

  "Nullary variants with cases",
  "switch (None) { case None -> None case Some (x) -> Some (x) }",
  result "None()" ~with_type: "[|Some:a|None:()|b|]";

  "Nested variant unification",
  "[C (A), C (B)]",
  result "[C(A()), C(B())]" ~with_type: "[[|C:[|A:()|B:()|b|]|a|]]";

  "Type annotations",
  "fun increment(x) {(Succ (x)):([|Succ:(mu a . [|Zero | Succ:a|])|])} increment",
  is_function ~with_type: "(mu c . [|Succ:c | Zero:()|]) -> [|Succ:mu b . [|Succ:b | Zero:()|]|]";

  (* xml *)

  "Braced XML",
  "<f>{for (var i <- []) <br/>}</f>",
  result "<f/>" ~with_type: "Xml";

  "Escaped braces",
  "<p>A left: {{ and a right: }}</p>",
  result "<p>A left: { and a right: }</p>" ~with_type: "Xml";

  "Escaped braces (not doubled)",
  "<p>A left: {{ and a right: }</p>",
  has_syntaxerror;

  "Backslashes",
  "<p>A backslash \ </p>",
  result "<p>A backslash \ </p>" ~with_type: "Xml";

  "Top-level-bound XML object",
  "var x = for (var i <- []) <br/>; <f>{x}</f>",
  result "<f/>" ~with_type: "Xml";

  "Let-bound XML object",
  "{var x = for (var i <- []) <br/>; <f>{x}</f>}",
  result "<f/>" ~with_type: "Xml";

  "Whitespace preservation",
  "<a b=\"c\"> <d/> </a>",
  result "<a b=\"c\"> <d/> </a>" ~with_type: "Xml";

  "Element splicing [1]",
  "{var x = \"three\"; <a b=\"c\">{stringToXml(x)}</a>}",
  result "<a b=\"c\">three</a>" ~with_type: "Xml";

  "Element splicing [2]",
  "{var x = \"hre\"; <a b=\"c\">t{stringToXml(x)}e</a>}",
  result "<a b=\"c\">three</a>" ~with_type: "Xml";

  "Attribute splicing [1]",
  "{var x = \"three\"; <a b=\"{x}\"><c/></a>}",
  result "<a b=\"three\"><c/></a>" ~with_type: "Xml";

  "Attribute splicing [2]",
  "{var x = \"three\"; <a b=\"a{x}b\"><c/></a>}",
  result "<a b=\"athreeb\"><c/></a>" ~with_type: "Xml";

  "Rejection of incorrectly nested elements",
  "<a><b></a></b>",
  has_syntaxerror;

  "Rejection of incorrectly typed attributes",
  "{var x = 3; <a b=\"{x}\"><c/></a>}",
  has_typeerror;

  "Accept okay l:name attributes",
  "<form><input l:name=\"foo\"/></form>",
  result "<form><input name=\"foo\"/></form>" ~with_type: "Xml";

  "Looping in XML quasis",
  "var things = [1, 2]; <ul>{for (var x <- things) <br/>}</ul>",
  result "<ul><br/><br/></ul>" ~with_type: "Xml";

  "Amp-encoding (OK)",
  "<xml>&lt;</xml>",
  result "<xml>&lt;</xml>" ~with_type: "Xml";

  "Amp-encoding (ill-formed XML)",
  "<xml>this & that</xml>",
  has_syntaxerror;

  "Amp-encoding (converting from string)",
  "var x = \"this & that\"; <xml>{stringToXml(x)}</xml>",
  result "<xml>this &amp; that</xml>" ~with_type: "Xml";

  "Looping in XML quasis, with multiple content elements.",
  "var things = [1, 2];<ul>{for (var x <- things) <li>{stringToXml(intToString(x))}</li><li>1</li>}</ul>",
  result "<ul><li>1</li><li>1</li><li>2</li><li>1</li></ul>" ~with_type: "Xml";

  "XML forest",
  "<#>foo<a/><b>bar</b>fubar</#>",
  result "foo<a/><b>bar</b>fubar" ~with_type: "Xml";

  "Signatures on top-level variables",
  "sig x : Int var x = 3; x",
  result "3" ~with_type: "Int";


  "Projection of absent field",
  "(x=\"1\").y",
  has_typeerror;

  "Polymorphic recursion with signatures",
  "sig f : (a) -> Int
   fun f(x) {
      var x = f(\"a\");
      var y = f(1);
      1
   } f",
  is_function ~with_type:"(a) -> Int";

  "Polymorphic mutual recursion [1]",
  "sig f : (a) -> Int
   fun f(x) {
      var x = g(\"a\");
      var y = g(1); 1
   }

   sig g : (a) -> Int
   fun g(x) {
      var x = f(\"a\");
      var y = f(1);
      1
   }

   f",
  is_function ~with_type: "(a) -> Int";

  "Polymorphic mutual recursion [2]",
  "sig f : (a) -> Int
   fun f(x) {
     var x = f(\"a\");
     var y = g(1);
     1
   }

   sig g : (a) -> Int
   fun g(x) {
     var x = g(\"a\");
     var y = f(1);
     1
   }

   f",
  is_function ~with_type: "(a) -> Int";

  "Polymorphic mutual recursion [3]",
  "sig f : (a) -> Int
   fun f(x) {
      var w = f(\"a\");
      var x = f(1);
      var y = g(\"a\");
      var z = g(1);
      1
   }

   sig g : (a) -> Int
   fun g(x) {
      var w = f(\"a\");
      var y = f(1);
      var w = g(\"a\");
      var z = g(1);
      1
    }

   f",
  is_function ~with_type: "(a) -> Int";

  "Polymorphic row recursion",
  "sig h : ((|a)) -> Int fun h(x) {h((x,x))} h",
  is_function ~with_type: "((|a)) -> Int";

  "Range [1]",
  "\"3\" ~ /[0-9]/",
  result "true" ~with_type: "Bool";

  "Range [2]",
  "\"0\" ~ /[0-9]/",
  result "true" ~with_type: "Bool";

  "Range [3]",
  "\"9\" ~ /[0-9]/",
  result "true" ~with_type: "Bool";

  "Range [4]",
  "\".\" ~ /[0-9]/",
  result "false" ~with_type: "Bool";

  "Range [5]",
  "\"p\" ~ /[a-z]/",
  result "true" ~with_type: "Bool";

  "Range [6]",
  "\"p\" ~ /[A-Z]/",
  result "false" ~with_type: "Bool";

  "Star [1]",
  "\"23r2r3\" ~ /.*/",
  result "true" ~with_type: "Bool";

  "Star [2]",
  "\"\" ~ /.*/",
  result "true" ~with_type: "Bool";

  "Star [3]",
  "\"abc\" ~ /(abc)*/",
  result "true" ~with_type: "Bool";

  "Star [4]",
  "\"abcabc\" ~ /(abc)*/",
  result "true" ~with_type: "Bool";

  "Star [5]",
  "\"\" ~ /(abc)*/",
  result "true" ~with_type: "Bool";

  "Star [6]",
  "\"a\" ~ /(abc)*/",
  result "true" ~with_type: "Bool";

  "Star [7]",
  "\"abca\" ~ /(abc)*/",
  result "true" ~with_type: "Bool";

  "Plus [1]",
  "\"23r2r3\" ~ /.+/",
  result "true" ~with_type: "Bool";

  "Plus [2]",
  "\"\" ~ /.+/",
  result "false" ~with_type: "Bool";

  "Plus [3]",
  "\"abc\" ~ /(abc)+/",
  result "true" ~with_type: "Bool";

  "Plus [4]",
  "\"abcabc\" ~ /(abc)+/",
  result "true" ~with_type: "Bool";

  "Plus [5]",
  "\"\" ~ /(abc)+/",
  result "false" ~with_type: "Bool";

  "Plus [6]",
  "\"a\" ~ /(abc)+/",
  result "false" ~with_type: "Bool";

  "Plus [7]",
  "\"abca\" ~ /(abc)+/",
  result "true" ~with_type: "Bool";

  "Plus/grouping [1]",
  "\"ABBB\" ~ /AB+/",
  result "true" ~with_type: "Bool";

  "Plus/grouping [2]",
  "\"ABAB\" ~ /AB+/",
  result "true" ~with_type: "Bool";

  "Plus/grouping [3]",
  "\"ABAB\" ~ /((A)(B))+/",
  result "true" ~with_type: "Bool";

  "Plus/grouping [4]",
  "\"ABBB\" ~ /((A)(B))+/",
  result "true" ~with_type: "Bool";

  "Interpolation [1]",
  "var x = \"a\"; \"aaa\" ~ /{x}*/",
  result "true" ~with_type: "Bool";

  "Interpolation [2]",
  "var x = \"a\"; \"abc\" ~ /{x}*/",
  result "true" ~with_type: "Bool";

  "Escaping metacharacter",
  "\"some .*string$\\\" ++?\" ~ /some \\.\\*string\\$\\\" \\+\\+\\?/",
  result "true" ~with_type: "Bool";

  (* This doesn't work without the mailbox types annotations *)
  "More-specific type annotation with typevars",
  "fun (x) {x} : ((a)-{b}->a) -> ((a)-{b}->a)",
  is_function ~with_type: "((a) -{b}-> a) -> ((a) -{b}-> a)";

   "Redundant pattern [1]",
   "fun (x) { switch (x) { case x -> x case A -> A }}",
  has_syntaxerror;

   "Redundant pattern [2]",
   "fun (x) { switch (x) { case x -> A(1) case A -> A('2') } }",
  has_syntaxerror;

   "Redundant pattern [3]",
   "fun (x) { switch (x) { case x -> x case A -> A }}",
  has_syntaxerror;

  "Redundant pattern [4]",
  "fun (x) {switch (x) { case(A(B(C(1,2,3)))) -> 0 case(A(B(C(1,2,3)))) -> 1}}",
  has_syntaxerror;


  "Reject multiple occurrences of a name in a pattern [1]",
  "fun (x,x) { x }",
  has_syntaxerror;

  "Reject multiple occurrences of a name in a pattern [2]",
  "fun () { var (x,x) = (1,2); x }",
  has_syntaxerror;

  "Reject multiple occurrences of a name in a pattern [3]",
  "fun () { var (a=x,b=x) = (a=1,b=2); x }",
  has_syntaxerror;

   "No value recursion",
   "{fun f() { g() } var x = f(); fun g() { x } ()}",
  has_typeerror;
]

let known_failures = [

  "Rejection of incorrectly typed l:attributes",
  "{var x = 3; <a l:href=\"{x}\"><c/></a>}",
  has_typeerror;

  "Case patterns (with redefined hd)",
  "{ fun hd(_) { 1 } switch (\"a\") { case [y] -> y }}",
  result "'a'" ~with_type: "Char";

  "continuation typing [3]",
  "{ escape y in { (\"\" == y(1), true == y(1)); 2 } }",
  result "1" ~with_type: "Int";

  "Reject nonsense l:name attributes",
  "<form><input l:name=\"{1+1}\" /></form>",
  has_syntaxerror;

  (* duplicate top-level bindings *)
  "Lexical scoping at top-level",
  "var x = 3; fun y(a) { x + a } var x = 4; y(10)",
  result "13" ~with_type: "Int";

  "Tables must have table type.",
  "fun (t) { for (var x <-- t) [x.y]}",
  is_function ~with_type: "a";


  (* Next two fail because we're not loading the prelude at present *)
  "Test that prelude is loaded",
  "map",
  result "fun" ~with_type: "((a) -> c, [a]) -> [c]";

  "Map function",
  "map(curry((+))(1), [1,2,3])",
  result "[2, 3, 4]" ~with_type: "[Int]";

]

let broken_tests = [
(*
  "handle HTML with VAR -> HANDLER construction",
  "handle <html>{handler; <body/>}</html> with r -> r",
  failwith "regex: @<html>.*</html> : Xml";
*)
]
