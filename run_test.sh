#!/bin/bash

make -s -j10 || exit $?

echo "Test 1"
_build/default/bin/links.exe --set generate_wizard=false --mode wat <(echo 'var w = 0.0; fun f(a: Int) { a + 4 } fun g(b,c : Int) { fun h(d: Int) { fun i() { b+c+d } i() } h(b) * f(b) } g(0,0) + 1') -o output_test.wast || exit $?
echo '(assert_return (get "w") (f64.const 0)) (assert_return (get "_init_result") (i64.const 1)) (assert_return (invoke "f" (i64.const 0)) (i64.const 4)) (assert_return (invoke "g" (i64.const 1) (i64.const 2)) (i64.const 20))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?

echo "Test 2"
_build/default/bin/links.exe --set generate_wizard=false --mode wat <(echo 'var a = 1; fun f() { 0 } fun g(b: Int) { fun h() { a + b } fun i() { h() } i } g(0)()') -o output_test.wast || exit $?
echo '(assert_return (get "a") (i64.const 1)) (assert_return (get "_init_result") (i64.const 1))' >> output_test.wast # Missing: (invoke "g" (i64.const 1))
../stack-switching/interpreter/wasm output_test.wast || exit $?

echo "Test 3"
_build/default/bin/links.exe --set generate_wizard=false --mode wat <(echo 'if (true) 0 else 1') -o output_test.wast || exit $?
echo '(assert_return (get "_init_result") (i64.const 0))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?

echo "Test 4"
_build/default/bin/links.exe --set generate_wizard=false --mode wat <(echo 'if (true) false else true') -o output_test.wast || exit $?
echo '(assert_return (get "_init_result") (i32.const 0))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?

echo "Test 5"
_build/default/bin/links.exe --set generate_wizard=false --mode wat <(echo 'var b = true; var a = 2; if (b && (a == 1)) false else true') -o output_test.wast || exit $?
echo '(assert_return (get "_init_result") (i32.const 1))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?

echo "Test 6"
_build/default/bin/links.exe --set generate_wizard=false --mode wat <(echo 'var b = false; var a = 2; if (b || (a == 2)) false else true') -o output_test.wast || exit $?
echo '(assert_return (get "_init_result") (i32.const 0))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?

echo "Test 7"
_build/default/bin/links.exe --set generate_wizard=false --mode wat <(echo 'fun f(x : Int) { fun g() { x } g } f(1)()') -o output_test.wast || exit $?
echo '(assert_return (get "_init_result") (i64.const 1))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?

echo "Test 8"
_build/default/bin/links.exe --set generate_wizard=false --enable-handlers --mode wat <(echo 'handle (5) { case <Foo => k> -> 2 }') -o output_test.wast || exit $?
echo '(assert_return (get "_init_result") (i64.const 5))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?
_build/default/bin/links.exe --set generate_wizard=false --enable-handlers --mode wat <(echo 'handle (do Foo) { case <Foo => k> -> 2 }') -o output_test.wast || exit $?
echo '(assert_return (get "_init_result") (i64.const 2))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?

echo "Test 9"
_build/default/bin/links.exe --set generate_wizard=false --enable-handlers --mode wat <(echo 'handle (()) { case <Foo => k> -> k(false) }') -o output_test.wast || exit $?
echo '(assert_return (get "_init_result") (ref.null none))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?
_build/default/bin/links.exe --set generate_wizard=false --enable-handlers --mode wat <(echo 'handle (do Foo) { case <Foo => k> -> k(false) }') -o output_test.wast || exit $?
echo '(assert_return (get "_init_result") (i32.const 0))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast || exit $?

rm output_test.wast
