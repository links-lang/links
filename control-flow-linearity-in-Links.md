# Control-Flow Linearity

Enable the feature of tracking control-flow linearity by passing the
flag `--track-control-flow-linearity`. It is usually used with
`--enabled-handlers`.

## New constructs

New types and kinds:

* `A =@ B` is the signature for linear operations (`A => B` for unlimited operations)
* `Lin` is the kind for linear effect variables (`Any` for effect
  variables with no linear restriction)

New terms:

* `lindo L` invokes the linear operation `L`
* `case <L =@ r> -> M` handles the linear operation `L`
* `xlin` switches the current control flow to linear


## Control flow

We use the concept *control flow* to mean terms that are sequencing
evaluated. Control flows carry effects that are invoked by the terms.
In other words, the terms on the same control flow share effects.
Since Links' effect system is based on row polymorphism, the effect
types of all terms on the same control flow are unified.

Function bodies introduce their own control flows. The computations
being handled (the `M` in `handle M {...}`) also have their own
control flows, which will be merged with the control flow outside
after being handled.

Control flows are unlimited by default. We are allowed use both
unlimited and linear operations, but only unlimited variables. We can
switch the current control flow to linear by invoking the keyword
`xlin`. In a linear control flow, we are allowed to use both unlimited
and linear variables, but only linear operations. Once the control
flow is switched to linear, all invocations of unlimited operations in
it (even before `xlin`) would cause type errors.

## Examples

1. We can mix linear and unlimited operations in an unlimited control
   flow. The anonymous effect variable `_` has kind `Any` by default
   which can be unified with any operations.

    ```
    links> fun() {do U; lindo L};
    fun : () {L:() =@ a,U:() => ()|_}-> a
    ```

2. We can only invoke linear operations in a linear control flow. A
   linear control flow allows the usage of the linear channel `ch`.
   Now the effect variable `_` has kind `Lin` explicitly, which can
   only be unified with linear operations of signatures `=@`.

    ```
    links> fun(ch:End) {xlin; lindo L; close(ch)};
    fun : (End) {L:() =@ ()|_::Lin}~> ()
    ```

3. We can only handle linear operations using a linear handler
   (indicated by `=@` in the clause) which guarantees the continuation
   is used exactly once.

    ```
    links> handle ({xlin; lindo L + 40}) { case <L =@ r> -> xlin; r(2) };
    42 : Int
    ```

4. We can handle unlimited operations before merging with a linear
   control flow as long as we guarantee that they are all handled.
   Note that after handling, the presence variable of `Choose` and row
   variable are both linear.

    ```
    links> fun(ch:End) { xlin; close(ch); handle ({if (do Choose) 40 else 2}) {case <Choose => r> -> r(true) + r(false)} };
    fun : (End) {Choose{_::Lin}|_::Lin}~> Int
    ```

5. When writing explicit quantifiers, we must explicitly annotate the
   kinds of row variables using `e::Row(Lin)` or `e::Row(Any)`. If the
   subkind is not specified, it means `Lin` in order to be compatible
   with variants and records in Links.

    ```
    links> sig f:forall e::Row(Any). () {Get:() => Int|e}-> Int fun f() {do Get}; f();
    f = fun : () {Get:() => Int|_}-> Int
    ```

## Design choices

It is necessary to have `xlin` for the same reason of having `linfun`
in Links. For example, neither of the following functions has a more
general type than the other one.

```
links> fun(x) {lindo L; x};
fun : (a) {L:() =@ ()|_}-> a
links> fun(x) {xlin; lindo L; x};
fun : (a::Any) {L:() =@ ()|_::Lin}-> a::Any
```

## Compatibility

* Compatible with all previous handler tests (except part of
  polymorphic operations and effect sugar).
* Not entirely compatible with FreezeML, SessionFail, etc.
* Passes all tests with the flag disabled, except
  * `!FAILURE: Operation polymorphism (2)`
  * `!FAILURE: Operation polymorphism (3)`
  * `!FAILURE: Typecheck example file examples/handlers/monadic_reflection.links`
