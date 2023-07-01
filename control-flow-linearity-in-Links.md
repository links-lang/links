# Control-Flow Linearity

Enable the feature of tracking control-flow linearity by passing the
flag `-track-control-flow-linearity`. It is usually used with
`--enabled-handlers`.

## New constructs

New types and kinds:

* `A =@ B` is the signature for linear operations (`A => B` for unlimited operations)
* `Lin` is the kind for linear effect variables (`Any` for effect
  variables with no linear restriction)

New terms:

* `lindo L` invokes the linear operation `L`
* `case <L =@ r> -> M` handles the linear operation `L`
* `xlin` switches the current effect scope to linear

## Effect scope

We introduce the concept *effect scope* to mean the scope where terms
share effects. Since Links' effect system is based on row
polymorphism, the effect types of all computations in the same effect
scope are unified.

Function bodies have their own effect scope. The computations being
handled (the `M` in `handle M {...}`) have their own effect scope but
also share some effects (which are not handled) with terms outside.

Effect scopes are unlimited by default. We are allowed use both
unlimited and linear operations, but only linear variables. We can
switch the current effect scope to linear by invoking `xlin`. In a
linear effect scope, we are allowed to use both unlimited and linear
variables, but only linear operations. Once the scope is switched to
linear, all invocations of unlimited operations in this scope (even
before `xlin`) would cause type errors.

## Examples

1. We can mix linear and unlimited operations in a unlimited scope.
   The anonymous effect variable `_` has kind `Any` by default which
   can be unified with any operations.

    ```
    links> fun() {do U; lindo L};
    fun : () {L:() =@ a,U:() => ()|_}-> a
    ```

2. We can only invoke linear operations in a linear scope. Linear
   scopes enable the usage of the linear channel `ch`. Now the effect
   variable `_` has kind `Lin` explicitly, which can only be unified
   with linear operations of signatures `=@`.

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

4. We can handle unlimited operations in a linear scope as long as we
   guarantee that they are all handled. Here, after handling, the
   presence variable of `Choose` and row variable are both linear.

    ```
    links> fun(ch:End) { xlin; close(ch); handle ({if (do Choose) 40 else 2}) {case <Choose => r> -> r(true) + r(false)} };
    fun : (End) {Choose{_}::Lin|_::Lin}~> Int
    ```

5. When writing explicit quantifiers, we must explicitly annotate the
   kinds of row variables using `e::Row(Lin)` or `e::Row(Any)`. If the
   subkind is not specified, it means `Lin` in order to be compatible
   with variants in Links.

    ```
    links> sig f:forall e::Row(Any). () {Get:() => Int|e}-> Int fun f() {do Get}; f();
    f = fun : () {Get:() => Int|_}-> Int
    ```

## Design choices

The `xlin` corresponds to `Let^Lin x <- Return () In ...` in the
paper. We use the trick that the linearity of `Let` is implicitly
inherited to avoid having them everywhere. It is necessary to have
`xlin` for the same reason of having `linfun` in Links. For example,
neither of the following functions has a more general type than the
other one.

```
links> fun(x) {lindo L; x};
fun : (a) {L:() =@ ()|_}-> a
links> fun(x) {xlin; lindo L; x};
fun : (a::Any) {L:() =@ ()|_::Lin}-> a::Any
```

## Compatibility

* Compatible with all previous handler tests (except part of
  polymorphic operations and effect sugar).
* Not entirely compatible with FreezeML.
* Entirely not compatible with SessionFail.
* Some printing behaviors are a little inconsistent with previous tests.
