open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Identity handler (1)" =
  run_expr ~args:["--enable-handlers"] {|{ handle(42) {case Return(x) -> x} }|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Identity handler (2)" =
  run_expr ~args:["--enable-handlers"] {|handle(42) {case Return(x) -> x}|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Listify handler (1)" =
  run_expr ~args:["--enable-handlers"] {|{ handle(42) {case Return(x) -> [x]} }|};
  [%expect {|
    [42] : [Int]
    exit: 0 |}]

let%expect_test "Listify handler (2)" =
  run_expr ~args:["--enable-handlers"] {|handle(42) {case Return(x) -> [x]}|};
  [%expect {|
    [42] : [Int]
    exit: 0 |}]

let%expect_test "Listify handler (3)" =
  run_expr ~args:["--enable-handlers"] {|{ handle([42, 41, 40, 39]) {case Return(x) -> [x]} }|};
  [%expect {|
    [[42, 41, 40, 39]] : [[Int]]
    exit: 0 |}]

let%expect_test "Listify handler (4)" =
  run_expr ~args:["--enable-handlers"] {|handle([42, 41, 40, 39]) {case Return(x) -> [x]}|};
  [%expect {|
    [[42, 41, 40, 39]] : [[Int]]
    exit: 0 |}]

let%expect_test "Top level operation invocation" =
  run_expr ~args:["--enable-handlers"] {|{ do Foo }|};
  [%expect {|
    <string>:1: Type error: Invocation of the operation
        `Foo'
    requires an effect context
        `{Foo:() {}-> a|b::Eff}'
    but, the currently allowed effects are
        `{wild}'
    In expression: do Foo.

    exit: 1 |}]

let%expect_test "Return invocation (1)" =
  run_expr ~args:["--enable-handlers"] {|fun() { do Return }|};
  [%expect {|
    <string>:1: Type error: The implicit effect Return is not invocable
    In expression: do Return.

    exit: 1 |}]

let%expect_test "Return invocation (2)" =
  run_expr ~args:["--enable-handlers"] {|{ fun() { do Return } }|};
  [%expect {|
    <string>:1: Type error: The implicit effect Return is not invocable
    In expression: do Return.

    exit: 1 |}]

let%expect_test "Operation invocation sugar (1)" =
  run_expr ~args:["--enable-handlers"] {|{ fun() { do Foo } }|};
  [%expect {|
    fun : () {Foo:() {}-> a|_}-> a
    exit: 0 |}]

let%expect_test "Operation invocation sugar (2)" =
  run_expr ~args:["--enable-handlers"] {|{ fun() { do Foo() } }|};
  [%expect {|
    fun : () {Foo:() {}-> a|_}-> a
    exit: 0 |}]

let%expect_test "Operation invocation sugar (3)" =
  run_expr ~args:["--enable-handlers"] {|{ fun() { do Foo(()) } }|};
  [%expect {|
    fun : () {Foo:(()) {}-> a|_}-> a
    exit: 0 |}]

let%expect_test "Operation invocation sugar (4)" =
  run_expr ~args:["--enable-handlers"] {|fun() { do Foo }|};
  [%expect {|
    fun : () {Foo:() {}-> a|_}-> a
    exit: 0 |}]

let%expect_test "Operation invocation sugar (5)" =
  run_expr ~args:["--enable-handlers"] {|fun() { do Foo() }|};
  [%expect {|
    fun : () {Foo:() {}-> a|_}-> a
    exit: 0 |}]

let%expect_test "Operation invocation sugar (6)" =
  run_expr ~args:["--enable-handlers"] {|fun() { do Foo(()) }|};
  [%expect {|
    fun : () {Foo:(()) {}-> a|_}-> a
    exit: 0 |}]

let%expect_test "Exception handling (1)" =
  run_expr ~args:["--enable-handlers"] {|{ handle({do Fail; 42}) {case Fail(_) -> Nothing : Maybe(Int) case Return(x) -> Just(x) : Maybe(Int)} }|};
  [%expect {|
    Nothing : Maybe (Int)
    exit: 0 |}]

let%expect_test "Exception handling (2)" =
  run_expr ~args:["--enable-handlers"] {|{ handle(42) {case Fail(_) -> Nothing : Maybe(Int) case Return(x) -> Just(x) : Maybe(Int)} }|};
  [%expect {|
    Just(42) : Maybe (Int)
    exit: 0 |}]

let%expect_test "Exception handling (3)" =
  run_expr ~args:["--enable-handlers"] {|{ handle({var _ = do Fail : Zero; 42}) {case Fail(k) -> k(42) : Either(String,Int) case Return(x) -> Right(x) : Either(String, Int)} }|};
  [%expect {|
    <string>:1: Type error: The effect type of an input to a handle should match the type of its computation patterns, but the expression
        `{var _ = do Fail : Zero; 42}'
    has effect type
        `{|Fail:() {}-> Zero|a}'
    while the handler handles effects
        `{Fail:() {}-> Int,wild|b}'
    In expression: handle({var _ = do Fail : Zero; 42}) {case Fail(k) -> k(42) : Either(String,Int) case Return(x) -> Right(x) : Either(String, Int)}.

    exit: 1 |}]

let%expect_test "Exception handling (4)" =
  run_expr ~args:["--enable-handlers"] {|handle({do Fail; 42}) {case Fail(_) -> Nothing : Maybe(Int) case Return(x) -> Just(x) : Maybe(Int)}|};
  [%expect {|
    Nothing : Maybe (Int)
    exit: 0 |}]

let%expect_test "Exception handling (5)" =
  run_expr ~args:["--enable-handlers"] {|handle(42) {case Fail(_) -> Nothing : Maybe(Int) case Return(x) -> Just(x) : Maybe(Int)}|};
  [%expect {|
    Just(42) : Maybe (Int)
    exit: 0 |}]

let%expect_test "Exception handling (6)" =
  run_expr ~args:["--enable-handlers"] {|handle({var _ = do Fail : Zero; 42}) {case Fail(k) -> k(42) : Either(String,Int) case Return(x) -> Right(x) : Either(String, Int)}|};
  [%expect {|
    <string>:1: Type error: The effect type of an input to a handle should match the type of its computation patterns, but the expression
        `{var _ = do Fail : Zero; 42}'
    has effect type
        `{|Fail:() {}-> Zero|a}'
    while the handler handles effects
        `{Fail:() {}-> Int,wild|b}'
    In expression: handle({var _ = do Fail : Zero; 42}) {case Fail(k) -> k(42) : Either(String,Int) case Return(x) -> Right(x) : Either(String, Int)}.

    exit: 1 |}]

let%expect_test "Binary choice handling (1)" =
  run_expr ~args:["--enable-handlers"] {|{ handle({ var x = if (do Choose) 40 else 20; var y = if (do Choose) 2 else -20; x + y }) {case Choose(k) -> k(true) ++ k(false) case Return(x) -> [x]} }|};
  [%expect {|
    [42, 20, 22, 0] : [Int]
    exit: 0 |}]

let%expect_test "Binary choice handling (2)" =
  run_expr ~args:["--enable-handlers"] {|handle({ var x = if (do Choose) 40 else 20; var y = if (do Choose) 2 else -20; x + y }) {case Choose(k) -> k(true) ++ k(false) case Return(x) -> [x]}|};
  [%expect {|
    [42, 20, 22, 0] : [Int]
    exit: 0 |}]

let%expect_test "Deep continuation escape (1)" =
  run_expr ~args:["--enable-handlers"] {|{ fromJust(handle({ do Escape; print("Back in action"); do Escape}) { case Escape(k) -> Just(k) case Return(_) -> Nothing })(()) }|};
  [%expect {|
    Back in actionJust(Resumption) : mu a . [|Just:(()) {}~> a|Nothing|]
    exit: 0 |}]

let%expect_test "Deep continuation escape (2)" =
  run_expr ~args:["--enable-handlers"] {|fromJust(handle({ do Escape; print("Back in action"); do Escape}) { case Escape(k) -> Just(k) case Return(_) -> Nothing })(())|};
  [%expect {|
    Back in actionJust(Resumption) : mu a . [|Just:(()) {}~> a|Nothing|]
    exit: 0 |}]

let%expect_test "Type-and-effect signature for deep handler (1)" =
  run_expr ~args:["--enable-handlers"] {|sig allChoices : (Comp(a, {Choose:Bool|e})) {Choose{_}|e}~> [a] fun allChoices(m) {handle(m()) {case Return(x) -> [x] case Choose(k) -> k(true) ++ k(false) }}|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Type-and-effect signature for deep handler (2)" =
  run_expr ~args:["--enable-handlers"] {|sig allChoices : (Comp(a, {Choose:Bool|e})) -> Comp([a], {Choose{_}|e}) fun allChoices(m)() {handle(m()) {case Return(x) -> [x] case Choose(k) -> k(true) ++ k(false)}}|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Type-and-effect signature for deep handler (3)" =
  run_expr ~args:["--enable-handlers"] {|sig allChoices : (Comp(a, {Choose:Bool|e})) -> Comp([a], {Choose- |e}) fun allChoices(m)() {handle(m()) {case Return(x) -> [x] case Choose(k) -> k(true) ++ k(false)}}|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Type-and-effect signature for shallow handler (1)" =
  run_expr ~args:["--enable-handlers"] {|sig allChoices : (Comp(a, {Choose:Bool|e})) {Choose{_}|e}~> [a] fun allChoices(m) {shallowhandle(m()) {case Return(x) -> [x] case Choose(k) -> allChoices(fun() {k(true)}) ++ allChoices(fun(){k(false)}) }}|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Type-and-effect signature for shallow handler (2)" =
  run_expr ~args:["--enable-handlers"] {|sig allChoices : (Comp(a, {Choose:Bool|e})) -> Comp([a], {Choose{_}|e}) fun allChoices(m)() {shallowhandle(m()) {case Return(x) -> [x] case Choose(k) -> allChoices(fun(){k(true)})() ++ allChoices(fun(){k(false)})()}}|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Type-and-effect signature for shallow handler (3)" =
  run_expr ~args:["--enable-handlers"] {|sig allChoices : (Comp(a, {Choose:Bool|e})) -> Comp([a], {Choose- |e}) fun allChoices(m)() {shallowhandle(m()) {case Return(x) -> [x] case Choose(k) -> allChoices(fun(){k(true)})() ++ allChoices(fun(){k(false)})()}}|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Type inference for deep handler" =
  run_expr ~args:["--enable-handlers"] {|fun() { handle({do A; do B}) { case A(k) -> k(()) case Return(x) -> x } }|};
  [%expect {|
    fun : () {A{_},B:() {}-> b|_}~> b
    exit: 0 |}]

let%expect_test "Soundness" =
  run_expr ~args:["--enable-handlers"] {|{fun mapk(m) { handle(m()) {case Map(p,k) -> map(k,p) case Return(x) -> [x]} } }|};
  [%expect {|
    ***: Parse error: <string>:1

      {fun mapk(m) { handle(m()) {case Map(p,k) -> map(k,p) case Return(x) -> [x]} } }
                                                                                      ^
    exit: 1 |}]

let%expect_test "Deep state handling (1)" =
  run_expr ~args:["--enable-handlers"] {|{fun state(m) { handle(m()) { case Get(k) -> fun(s) { k(s)(s) } case Put(p,k) -> fun(s) { k(())(p) } case Return(x) -> fun(s) { x } } } fun runState(s0, c) { var f = state(c); f(s0) } runState(2, fun() { var s = do Get; do Put(s + 1); var s = do Get; do Put(s + s); do Get }) }|};
  [%expect {|
    6 : Int
    exit: 0 |}]

let%expect_test "Deep state handling (2)" =
  run_expr ~args:["--enable-handlers"] {|{fun state(m)(s) { handle(m())(s <- s) { case Get(k) -> k(s,s) case Put(p,k) -> k((),p) case Return(x) -> x } } fun runState(s0, c) { state(c)(s0) } runState(2, fun() { var s = do Get; do Put(s + 1); var s = do Get; do Put(s + s); do Get }) }|};
  [%expect {|
    6 : Int
    exit: 0 |}]

let%expect_test "Deep state handling (3)" =
  run_expr ~args:["--enable-handlers"] {|handle({ var s = do Get; do Put(s + 1); var s = do Get; do Put(s + s); do Get })(s <- 2) { case Get(k) -> k(s,s) case Put(p,k) -> k((),p) case Return(x) -> x }|};
  [%expect {|
    6 : Int
    exit: 0 |}]

let%expect_test "Shallow state handling (1)" =
  run_expr ~args:["--enable-handlers"] {|{fun state(m)(s) { shallowhandle(m()) { case Get(k) -> state(fun(){k(s)})(s) case Put(p,k) -> state(fun(){k(())})(p) case Return(x) -> x}} fun runState(s0, c) { var f = state(c); f(s0) } runState(2, fun() { var s = do Get; do Put(s + 1); var s = do Get; do Put(s + s); do Get }) }|};
  [%expect {|
    6 : Int
    exit: 0 |}]

let%expect_test "Shallow state handling (2)" =
  run_expr ~args:["--enable-handlers"] {|{ fun simpleState(m)(s) { shallowhandle(m()) { case Get(k) -> simpleState(fun() { k(s) })(s) case Put(s,k) -> simpleState(fun() { k(()) })(s) case Return(x) -> x } } fun count() { var n = do Get; if (n == 0) {n} else {do Put(n-1); count() }} simpleState(count)(10) }|};
  [%expect {|
    0 : Int
    exit: 0 |}]

let%expect_test "Shadowing handler parameter (1)" =
  run_expr ~args:["--enable-handlers"] {|{ handle({ var s = do Get; do Put(s + 1); var s = do Get; do Put(s + s); do Get })(s <- 0) { case Get(k) -> k(s,s) case Put(s,k) -> k((),s) case Return(x) -> x } }|};
  [%expect {|
    2 : Int
    exit: 0 |}]

let%expect_test "Shadowing handler parameter (2)" =
  run_expr ~args:["--enable-handlers"] {|{ handle({ var s = do Get; do Put(s + 1); var s = do Get; do Put(s + s); do Get })(s <- 0) { case Get(k) -> k(s,s) case Put(p as s,k) -> k((),s) case Return(x) -> x } }|};
  [%expect {|
    2 : Int
    exit: 0 |}]

let%expect_test "Shadowing handler parameter (3)" =
  run_expr ~args:["--enable-handlers"] {|{ handle({ var s = do Get; do Put(s + 1); var s = do Get; do Put(s + s); do Get })(s <- 0) { case Get(k) -> k(s,s) case Put(s as p,k) -> k((),s) case Return(x) -> x } }|};
  [%expect {|
    2 : Int
    exit: 0 |}]

let%expect_test "Operation parameter pattern-matching (1)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun (m) { handle(m()) { case Op1(_) -> 1 case Op2(_,k) -> 2 case Op3(_,_) -> 3 case Return(x) -> x } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Operation parameter pattern-matching (2)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(p as q,k) -> k(q) case Op2(s,t as w,k) -> k(t) case Op3(a,b,c as d,_) -> d case Return(x) -> x } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Operation parameter pattern-matching (3)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(1,k) -> k(1) case Op2(s,2,k) -> k(s) case Op3(a,b,3,_) -> 3 case Return(x) -> x } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Operation parameter pattern-matching (4)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(1.0,k) -> k(1.0) case Op2(s,2.0,k) -> k(s) case Op3(a,b,3.0,_) -> 3.0 case Return(x) -> x } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Operation parameter pattern-matching (5)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(Alice,k) -> k(1) case Op2(s,Bob,k) -> k(s) case Op3(a,b,Jenny,_) -> a case Return(x) -> x } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Operation parameter pattern-matching (6)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1((_,y),k) -> k(y) case Op2((x,y,z),k) -> k(z) case Op3(_,(a,b,c),_) -> a case Return(x) -> x } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Operation parameter pattern-matching (7)" =
  run_expr ~args:["--enable-handlers"] {|fun(m) { handle(m()) { case Move(Alice, _) -> 'A' case Move(Bob, _) -> 'B' case Move(_,_) -> 'U' case Return(x) -> x } }|};
  [%expect {|
    fun : (() {Move:([|Alice|Bob|_|]) {}-> _::Any|c}~> Char) {Move{_}|c}~> Char
    exit: 0 |}]

let%expect_test "Operation parameter pattern-matching (8)" =
  run_expr ~args:["--enable-handlers"] {|handle(do Move(Alice)) { case Move(Alice, _) -> 'A' case Move(Bob, _) -> 'B' case Move(_,_) -> 'U' case Return(x) -> x }|};
  [%expect {|
    'A' : Char
    exit: 0 |}]

let%expect_test "Operation parameter pattern-matching (9)" =
  run_expr ~args:["--enable-handlers"] {|handle(do Move(John)) { case Move(Alice, _) -> 'A' case Move(Bob, _) -> 'B' case Move(_,_) -> 'U' case Return(x) -> x }|};
  [%expect {|
    'U' : Char
    exit: 0 |}]

let%expect_test "Pattern-matching on continuation parameter (1)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op(_) -> 0 case Return(x) -> x } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Pattern-matching on continuation parameter (2)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op(k as f) -> f(1) case Return(x) -> x } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Pattern-matching on continuation parameter (3)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op(2) -> f(1) case Return(x) -> x } })|};
  [%expect {|
    <string>:1: Type error: Improper pattern matching on resumption
    In expression: 2.

    exit: 1 |}]

let%expect_test "Value parameter pattern-matching (1)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(k) -> 1 case Return(_) -> 0 } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Value parameter pattern-matching (2)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(k) -> 1 case Return(x as y) -> y } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Value parameter pattern-matching (3)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(k) -> 1 case Return(10) -> 10 } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Value parameter pattern-matching (4)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(k) -> 1 case Return(100.0) -> 0 } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Value parameter pattern-matching (5)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(k) -> 1 case Return(Alice) -> 0 } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Value parameter pattern-matching (6)" =
  run_expr ~args:["--enable-handlers"] {|ignore(fun(m) { handle(m()) { case Op1(k) -> 1 case Return((x,y)) -> 0 } })|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Pattern-matching on handler parameter (1)" =
  run_expr ~args:["--enable-handlers"] {|handle(true)(_ <- 100) { case Return(x) -> x }|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Pattern-matching on handler parameter (2)" =
  run_expr ~args:["--enable-handlers"] {|handle(true)(100 <- 100) { case Return(x) -> x}|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Pattern-matching on handler parameter (2)" =
  run_expr ~args:["--enable-handlers"] {|handle(true)(99 <- 100) { case Return(x) -> x}|};
  [%expect {|
    ***: Error: Links_core.Evalir.Exceptions.Wrong
    exit: 1 |}]

let%expect_test "Pattern-matching on handler parameter (3)" =
  run_expr ~args:["--enable-handlers"] {|handle(true)(Foo(s) <- Foo(42)) { case Return(_) -> s}|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Pattern-matching on handler parameter (4)" =
  run_expr ~args:["--enable-handlers"] {|handle(true)(Foo(s) <- Bar(42)) { case Return(_) -> s}|};
  [%expect {|
    <string>:1: Type error: The parameter pattern must match the expression in a handle parameter binding, but the pattern
        `Foo(s)'
    has type
        `[|Foo:a::Any|]'
    while the expression
        `Bar(42)'
    has type
        `[|Bar:Int|b::Any|]'
    In expression: handle(true)(Foo(s) <- Bar(42)) { case Return(_) -> s}.

    exit: 1 |}]

let%expect_test "Pattern-matching on handler parameter (5)" =
  run_expr ~args:["--enable-handlers"] {|handle(true)((x,y) <- (2,1)) { case Return(_) -> x + y}|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Pattern-matching on handler parameter (6)" =
  run_expr ~args:["--enable-handlers"] {|handle(true)("Hello" <- "Hello") { case Return(x) -> x}|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Pattern-matching on handler parameter (7)" =
  run_expr ~args:["--enable-handlers"] {|handle(true)((a=x, b=y) <- (a=44,b=(-2))) { case Return(_) -> x + y}|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Pattern-matching on handler parameter (8)" =
  run_expr ~args:["--enable-handlers"] {|handle(true)(r <- (a=44,b=(-2))) { case Return(_) -> r.a + r.b}|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Deep Handler composition" =
  run_expr ~args:["--enable-handlers"] {|fun h1(m)() { handle(m()) { case Op1(k) -> k(1) } } fun h2(m)() { handle(m()) { case Op2(k) -> k(2) } } fun h3(m)() { handle(m()) { case Op3(k) -> k(3) } } h1(h2(h3(fun() { do Op1 + do Op2 + do Op3 })))()|};
  [%expect {|
    6 : Int
    exit: 0 |}]

let%expect_test "Type annotation on deep continuation parameter" =
  run_expr ~args:["--enable-handlers"] {|fun h1(m) { handle(m()) { case Op(k : ((Int) {Op{_}|_}~> Int)) -> k(1) } }|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Type annotation on shallow continuation parameter" =
  run_expr ~args:["--enable-handlers"] {|fun h1(m) { shallowhandle(m()) { case Op(k : ((Int) {Op:Int|_}~> Int)) -> h1(fun() { k(1) }) } }|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Shallow addition with a single recursive handler" =
  run_expr ~args:["--enable-handlers"] {|{ fun h1(m) { shallowhandle(m()) { case One(k) -> h1(fun() { k(1) }) case Return(x) -> x - 1 } } h1(fun() { do One + do One }) }|};
  [%expect {|
    1 : Int
    exit: 0 |}]

let%expect_test "Shallow addition with two mutual recursive handlers" =
  run_expr ~args:["--enable-handlers"] {|{ fun h1(m) { shallowhandle(m()) { case One(k) -> h1(fun() { k(1) }) } } fun h2(m) { shallowhandle(m()) { case One(k) -> h1(fun() { k(2) }) } } h2(fun() { do One + do One }) }|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Shallow handler composition" =
  run_expr ~args:["--enable-handlers"] {|{ fun h1(m)() { shallowhandle(m()) { case Op1(k) -> h1(fun() { k(1) })() } } fun h2(m)() { shallowhandle(m()) { case Op2(k) -> h2(fun() { k(2) })() } } h1(h2(fun() { do Op1 + do Op2 }))() }|};
  [%expect {|
    3 : Int
    exit: 0 |}]

let%expect_test "Type ascription, parameterised handlers (1)" =
  run_expr ~args:["--enable-handlers"] {|{ fun(a : Int)(b : Float)(c : String)(m)() { handle (m())(x <- a, y <- b, z <- c) { case Op(p,k) -> k(c,42,p,"Foo") case Return(_) -> x } } }|};
  [%expect {|
    fun : (Int) -> (Float) -> (String) -> (() {Op:(Float) {}-> String|d}~> _) -> () {Op{_}|d}~> Int
    exit: 0 |}]

let%expect_test "Type ascription, parameterised handlers (2)" =
  run_expr ~args:["--enable-handlers"] {|{ fun(a : Float, b : String, c : Int)(m)() { handle(m())(x <- a, y <- b, z <- c) { case Op(p,k) -> k(x,p,"Bar",99) case Return(_) -> y } } }|};
  [%expect {|
    fun : (Float, String, Int) -> (() {Op:(Float) {}-> Float|b}~> _) -> () {Op{_}|b}~> String
    exit: 0 |}]

let%expect_test "Instantiate.ArityMismatch #132 (RESOLVED)" =
  run_expr ~args:["--enable-handlers"] {|sig f : (() {Foo:Int|a}~> b) {Foo{_}|a}~> b fun f(m) { error("N/A") } fun g(m) { var x = f(m); x }|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Operation polymorphism" =
  run_expr ~args:["--enable-handlers"] {|sig catch : (() {Fail:forall a.a |e}~> b) {Fail{_} |e}~> Maybe(b) fun catch(m) { handle(m()) { case Fail(k) -> Nothing case Return(x) -> Just(x) } } catch(fun() { 42 })|};
  [%expect {|
    Just(42) : Maybe (Int)
    exit: 0 |}]

let%expect_test "Generalise (1)" =
  run_expr ~args:["--enable-handlers"] {|gen0(fun(m)() { handle(m()) { case Foo(k) -> 42 case Return(x) -> x } }(fun(){42}))|};
  [%expect {|
    fun : Comp (Int,{ |_ })
    exit: 0 |}]

let%expect_test "Generalise (2)" =
  run_expr ~args:["--enable-handlers"] {|gen0(fun(m)() { handle(m()) { case Foo(k : ((()) {Foo- |_}~> Int)) -> 42 case Return(x) -> x } }(fun(){42}))|};
  [%expect {|
    fun : Comp (Int,{ |_ })
    exit: 0 |}]

let%expect_test "Recursive nesting of deep handlers" =
  run_expr ~args:["--enable-handlers"] {|{ fun h1(m,h) { handle(m()) { case Foo(k) -> h(fun() { k(()) },h1) case Return(x) -> x } } fun h2(m,h) { handle(m()) { case Foo(k) -> h(fun() { k(()) },h2) case Return(x) -> x } } h1(fun(){42},h2) }|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Parameterised handler with multiple parameters (1)" =
  run_expr ~args:["--enable-handlers"] {|handle({do A; do B; do C; do D})(a <- 0, b <- 1, c <- 2, d <- 3) { case A(k) -> k((),a+1,b,c,d) case B(k) -> k((),a,b+1,c,d) case C(k) -> k((),a,b,c+1,d) case D(k) -> k((),a,b,c,d+1) case Return(_) -> (a,b,c,d) }|};
  [%expect {|
    (1, 2, 3, 4) : (Int, Int, Int, Int)
    exit: 0 |}]

let%expect_test "Parameterised handler with multiple parameters (2)" =
  run_expr ~args:["--enable-handlers"] {|handle({do A; do B; do C; do D})(a <- 0, b <- false, (c0, c1) as c <- (true,0), d <- "Hello") { case A(k) -> k((),a+1,b,c,d) case B(k) -> k((),a,not(b),c,d) case C(k) -> k((),a,b,(not(c0), c1+1),d) case D(k) -> k((),a,b,c,d ^^ " World") case Return(_) -> (a,b,c,d) }|};
  [%expect {|
    (1, true, (false, 1), "Hello World") : (Int, Bool, (Bool, Int), String)
    exit: 0 |}]

let%expect_test "Effect type sugar (1)" =
  run_expr ~args:["--enable-handlers"] {|fun(g : (() {A:a,B:(a) -> b|_}~> b)) { g }(fun(){error("N/A")})|};
  [%expect {|
    fun : () {A:() {}-> a,B:(a) {}-> b|_}~> b
    exit: 0 |}]

let%expect_test "Effect type sugar (2)" =
  run_expr ~args:["--enable-handlers"] {|fun(g : (() {:a|_}~> a)) { g }(fun(){error("N/A")})|};
  [%expect {|
    fun : () {:a|_}~> a
    exit: 0 |}]

let%expect_test "Effect type sugar (3)" =
  run_expr ~args:["--enable-handlers"] {|fun(g : (() {wild:()|_}-> a)) { g }(fun(){error("N/A")})|};
  [%expect {|
    fun : () ~> _
    exit: 0 |}]

let%expect_test "Implicit return case (1)" =
  run_expr ~args:["--enable-handlers"] {|handle(42) { }|};
  [%expect {|
    42 : Int
    exit: 0 |}]

let%expect_test "Implicit return case (2)" =
  run_expr ~args:["--enable-handlers"] {|handle(do Op) { case Op(resume) -> resume(true) }|};
  [%expect {|
    true : Bool
    exit: 0 |}]

let%expect_test "Omission of resumption for nullary operations (1)" =
  run_expr ~args:["--enable-handlers"] {|handle(do Foo) { case Foo -> 5 }|};
  [%expect {|
    5 : Int
    exit: 0 |}]

let%expect_test "Omission of resumption for nullary operations (2)" =
  run_expr ~args:["--enable-handlers"] {|handle(do Foo) { case Foo() -> 6 }|};
  [%expect {|
    6 : Int
    exit: 0 |}]

let%expect_test "Omission of resumption for nullary operations (3)" =
  run_expr ~args:["--enable-handlers"] {|fun(m) { handle(m()) { case Foo -> 5 } }|};
  [%expect {|
    fun : (() {Foo:() {}-> _::Any|b}~> Int) {Foo{_}|b}~> Int
    exit: 0 |}]

let%expect_test "Examples" =
  run_file ~args:["--enable-handlers"; "--path=examples/handlers"; "--config=tests/effect_sugar.config"] {|./examples/handlers/tests.links|};
  [%expect {|
    () : ()
    exit: 0 |}]

