#!/bin/bash

make -s -j10 || exit $?

_build/default/bin/links.exe --mode wasm <(echo 'var w = 0.0; fun f(a: Int) { a + 4 } fun g(b,c : Int) { fun h(d: Int) { fun i() { b+c+d } i() } h(b) * f(b) } g(0,0) + 1') -o output_test.wast || exit $?
echo '(assert_return (get "w") (f64.const 0)) (assert_return (get "_main_result") (i64.const 1)) (assert_return (invoke "f_2563" (i64.const 0)) (i64.const 4)) (assert_return (invoke "g_2574" (i64.const 1) (i64.const 2)) (i64.const 20))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast

_build/default/bin/links.exe --mode wasm <(echo 'var a = 1; fun f() { 0 } fun g(b: Int) { fun h() { a + b } fun i() { h() } i } g(0)()') -o output_test.wast || exit $?
# Missing: (invoke "g_2565" (i64.const 1))
echo '(assert_return (get "a") (i64.const 1)) (assert_return (get "_main_result") (i64.const 1))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast

_build/default/bin/links.exe --mode wasm <(echo 'if (true) 0 else 1') -o output_test.wast || exit $?
echo '(assert_return (get "_main_result") (i64.const 0))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast

_build/default/bin/links.exe --mode wasm <(echo 'if (true) false else true') -o output_test.wast || exit $?
echo '(assert_return (get "_main_result") (i32.const 0))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast

_build/default/bin/links.exe --mode wasm <(echo 'var b = true; var a = 2; if (b && (a == 1)) false else true') -o output_test.wast || exit $?
echo '(assert_return (get "_main_result") (i32.const 1))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast

_build/default/bin/links.exe --mode wasm <(echo 'var b = false; var a = 2; if (b || (a == 2)) false else true') -o output_test.wast || exit $?
echo '(assert_return (get "_main_result") (i32.const 0))' >> output_test.wast
../stack-switching/interpreter/wasm output_test.wast
