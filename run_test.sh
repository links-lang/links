#!/bin/bash

make -s -j10 || exit $?

echo "Test 1"
_build/default/bin/links.exe --mode wasm <(echo 'var w = 0.0; fun f(a: Int) { a + 4 } fun g(b,c : Int) { fun h(d: Int) { fun i() { b+c+d } i() } h(b) * f(b) } g(0,0) + 1') -o output_test.wasm || exit $?
echo -n "Returns 1: "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?

echo "Test 2"
_build/default/bin/links.exe --mode wasm <(echo 'var a = 1; fun f() { 0 } fun g(b: Int) { fun h() { a + b } fun i() { h() } i } g(0)()') -o output_test.wasm || exit $?
echo -n "Returns 1: "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?

echo "Test 3"
_build/default/bin/links.exe --mode wasm <(echo 'if (true) 0 else 1') -o output_test.wasm || exit $?
echo -n "Returns 0: "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?

echo "Test 4"
_build/default/bin/links.exe --mode wasm <(echo 'if (true) false else true') -o output_test.wasm || exit $?
echo -n "Returns false: "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?

echo "Test 5"
_build/default/bin/links.exe --mode wasm <(echo 'var b = true; var a = 2; if (b && (a == 1)) false else true') -o output_test.wasm || exit $?
echo -n "Returns true: "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?

echo "Test 6"
_build/default/bin/links.exe --mode wasm <(echo 'var b = false; var a = 2; if (b || (a == 2)) false else true') -o output_test.wasm || exit $?
echo -n "Returns false: "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?

echo "Test 7"
_build/default/bin/links.exe --mode wasm <(echo 'fun f(x : Int) { fun g() { x } g } f(1)()') -o output_test.wasm || exit $?
echo -n "Returns 1: "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?

echo "Test 8"
_build/default/bin/links.exe --enable-handlers --mode wasm <(echo 'handle (5) { case <Foo => k> -> 2 }') -o output_test.wasm || exit $?
echo -n "Returns 5: "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?
_build/default/bin/links.exe --enable-handlers --mode wasm <(echo 'handle (do Foo) { case <Foo => k> -> 2 }') -o output_test.wasm || exit $?
echo -n "Returns 2: "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?

echo "Test 9"
_build/default/bin/links.exe --enable-handlers --mode wasm <(echo 'handle (()) { case <Foo => k> -> k(()) }') -o output_test.wasm || exit $?
echo -n "Returns (): "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?
_build/default/bin/links.exe --enable-handlers --mode wasm <(echo 'handle (do Foo) { case <Foo => k> -> k(()) }') -o output_test.wasm || exit $?
echo -n "Returns (): "
../wizard-engine/bin/wizeng -ext:stack-switching -ext:wizeng output_test.wasm || exit $?

rm output_test.wasm
