open Test_common
open Expect_test_common.File.Location


let%expect_test "Let polymorphism [1]" =
  run_expr {|fun pair(x) { (x, x) } (pair('a'), pair(13))|}

let%expect_test "Let polymorphism [2]" =
  run_expr {|{fun pair(x) { (x, x) } (pair('a'), pair(13))}|}

let%expect_test "Monomorphic lambda bindings" =
  run_expr {|(fun (p) { (p('a'), p(13))})(fun (x) { (x,x) })|}

let%expect_test "No polymorphic recursion without signatures" =
  run_expr {|fun f(x) { ignore(f("a")); ignore(f(1)); 1}|}

let%expect_test "Polymorphic recursion with signatures" =
  run_expr {|sig f : (_) ~> Int fun f(x) { ignore(f("a")); ignore(f(1)); 1 } f|}

let%expect_test "Invalid \"polymorphic recursion\"" =
  run_expr {|sig f : (_) ~> Int fun f(x) { x == 1; f("a"); f(1); 1 }|}

let%expect_test "Polymorphic mutual recursion [1]" =
  run_expr {|mutual { sig f : (_) ~> Int fun f(x) { ignore(g("a")); ignore(g(1)); 1 } sig g : (a) ~> Int fun g(x) { ignore(f("a")); ignore(f(1)); 1 } } f|}

let%expect_test "Polymorphic mutual recursion [2]" =
  run_expr {|mutual { sig f : (_) ~> Int fun f(x) { ignore(f("a")); ignore(g(1)); 1 } sig g : (a) ~> Int fun g(x) { ignore(g("a")); ignore(f(1)); 1 } } f|}

let%expect_test "Polymorphic mutual recursion [3]" =
  run_expr {|mutual { sig f : (_) ~> Int fun f(x) { ignore(f("a")); ignore(f(1)); ignore(g("a")); ignore(g(1)); 1 } sig g : (a) ~> Int fun g(x) { ignore(f("a")); ignore(f(1)); ignore(g("a")); ignore(g(1)); 1 } } f|}

let%expect_test "Polymorphic mutual recursion [4]" =
  run_expr {|mutual { sig f : (_) ~> Int fun f(x) { ignore(g("a")); ignore(g(1)); 1 } sig g : (a) ~> Int fun g(x) { ignore(x == 1); ignore(f("a")); ignore(f(1)); 1 } } f|}

let%expect_test "Polymorphic row recursion" =
  run_expr {|sig h : ((| _)) ~> Int fun h(x) {h((x,x))} h|}

let%expect_test "Polymorphic functions" =
  run_expr {|fun cons(x,l) {[x] ++ l} (cons(5, [6]), cons("a", ["b"]))|}

let%expect_test "Polymorphic functions in different function bodies" =
  run_expr {|fun cons(x,l) {[x] ++ l} fun f() { cons(5, [6]) } fun g() { cons("a", ["b"]) } (f(), g())|}

let%expect_test "Signatures on top-level variables" =
  run_expr {|sig x : Int var x = 3; x|}

let%expect_test "Value restriction [1]" =
  run_expr {|sig f : (a) -> a var f = id(id); f|}

let%expect_test "Value restriction [2]" =
  run_expr {|sig foo : () {:a|r}~> a fun foo() {var x = recv(); x} foo|}

let%expect_test "Overly general signatures" =
  run_expr {|sig id : (a) -> b fun id(x) {x}|}

let%expect_test "Polymorphic effects for curried recursive functions" =
  run_expr {|fun zipwith(f)(xs)(ys) {switch ((xs, ys)) {case ((x::xs),(y::ys)) -> (f(x)(y))::(zipwith(f)(xs)(ys)) case _ -> []}} zipwith|}

let%expect_test "Polymorphic function parameters (unannotated)" =
  run_expr {|fun (f) {(f(42), f(true))}|}

let%expect_test "Polymorphic function parameters (annotated)" =
  run_expr {|fun (f : (forall a,e::Row.(a) -e-> a)) {(f(42), f(true))}|}

let%expect_test "Explicitly polymorphic signatures" =
  run_expr {|sig choose : forall a,d::Row,e::Row. (a) -d-> (a) -e-> a fun choose(x)(y) {x} choose|}

let%expect_test "Quantifiers in different orders" =
  run_expr {|fun (x:(forall a,b.(a,b)), y:(forall b,a.(a,b))) {[x,y]}|}

let%expect_test "Polymorphic function arguments and freeze" =
  run_expr {|fun poly(f : (forall a::Any,e::Row.(a) -e-> a)) {(f(42), f(true))} poly(~id)|}

let%expect_test "Top-level ill-typed polymorphism" =
  run_expr {|sig foo : forall a.((a) {}~> a) {}~> (Int) {}~> Int fun foo(f){f}|}

let%expect_test "Polymorphic type aliases (Church numerals)" =
  run_expr {|typename Nat = forall a.(a) {}~> ((a) {}~> a) {}~> a; sig zero : Nat var zero = fun (z)(s) {z}; sig succ : (Nat) {}~> Nat fun succ(n) {fun (z)(s){s(n(z)(s))}} succ(succ(zero))(0)(fun (x) {x+1})|}

let%expect_test "Recursive polymorphic type aliases" =
  run_expr {|typename Foo = forall a.Foo; sig bar : (Foo) {}~> () fun bar(m) {()} bar|}

let%expect_test "Extra quantifier with mixed kinds" =
  run_expr {|sig f : forall a, e::Row.(a) {}~> a fun f(x) {f(x)} f|}

let%expect_test "Missing quantifier with mixed kinds (1)" =
  run_expr {|sig f : forall a.(a) ~e~> a fun f(x) {f(x)} f|}

let%expect_test "Missing quantifier with mixed kinds (2)" =
  run_expr {|sig f : forall e::Eff.(a) ~e~> a fun f(x) {f(x)} f|}

let%expect_test "Missing quantifier with mixed kinds (3)" =
  run_expr {|sig f : forall a.(a) ~e~> a fun f(x) {x} f|}

let%expect_test "Missing quantifier with mixed kinds (4)" =
  run_expr {|sig f : forall e::Eff.(a) ~e~> a fun f(x) {x} f|}

let%expect_test "Inner quantifier with free variable" =
  run_expr {|sig f : (a) -e-> (a, a) fun f(x) {sig g : forall b.(a, b) -e-> (a, b) fun g(x, y) {(x, y)} g(x, x)} f|}

let%expect_test "Flexible quantifier elimination (fix introduced in 514187b)" =
  run_expr {|sig a : (() {A:() |e}~> a) -> () {A{_} |e}~> a fun a(m)() { error("a") } sig b : (() {B:() |e}~> a) -> () {B{_} |e}~> a fun b(m)() { error("b") } sig c : (() {A:(),B:() |e}~> a) {A{_}, B{_} |e}~> a fun c(m) { a(b(m))() }|}

let%expect_test "Calling non-instantiated functions" =
  run_expr {|(~id)(2)|}

let%expect_test "Projecting non-instantiated records" =
  run_expr {|var x = (id=id); (~x).id(2)|}

let%expect_test "Varying arities of foralls (1)" =
  run_expr {|(~id) : forall a::Any. forall e::Row. (a) -e-> a|}

let%expect_test "Varying arities of foralls (2)" =
  run_expr {|(~id) : forall a::Any. forall e::Row. forall b. (a) -e-> a|}

let%expect_test "Signatures (fun): free variable on parameter without sig (1)" =
  run_expr {|fun f(x : a) {x} f|}

let%expect_test "Signatures (fun): free variable in body without sig (2)" =
  run_expr {|fun f(x) {x : a} f|}

let%expect_test "Signatures (fun): free variable on parameeter with unquantified sig (3)" =
  run_expr {|sig f : (a) -> a fun f(x : a) {x} f|}

let%expect_test "Signatures (fun): free variable on parameter with quantified sig (4)" =
  run_expr {|sig f : forall a. (a) -> a fun f(x : a) {x} f|}

let%expect_test "Signatures (fun): free variable on body with quantified sig (5)" =
  run_expr {|sig f : forall a. (a) -> a fun f(x) {x : a} f|}

let%expect_test "Signatures (var): free variable in body, no sig (6)" =
  run_expr {|fun f(x) {x}     var x = f : ((a) -> a); x|}

