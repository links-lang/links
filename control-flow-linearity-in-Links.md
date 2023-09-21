# Control-Flow Linearity

Enable the feature of tracking control-flow linearity by passing the
flag `--track-control-flow-linearity`. It is usually used with
`--enabled-handlers`.

## New constructs

New types and kinds:

* `A =@ B` is the signature for control-flow-linear operations (`A =>
  B` for control-flow-unlimited operations)
* `Lin` is the kind for control-flow-linear effect variables (`Any`
  for effect variables with no linear restriction)

New terms:

* `lindo L` invokes a control-flow-linear operation `L`
* `case <L =@ r> -> M` handles a control-flow-linear operation `L`
* `xlin` switches the current control flow linearity to linear


## Effect scope

We introduce the concept *effect scope* to mean the scope where terms
are sequencing evaluated. In other words, since Links' effect system
is based on row polymorphism, all terms in the same effect scope share
the same effects (their effect types are unified). Thus, all effects
invoked in the same effect scope have the same control flow linearity.

Function bodies hold their own effect scopes. The terms being handled
(the `M` in `handle M {...}`) also have their own effect scope but
also share some effects (which are not handled) with terms outside the
scope.

By default, the control-flow linearity of every effect scope is
unlimited. We are allowed use both control-flow-linear and
control-flow-unlimited operations, but only unlimited resources. We
can switch the current effect scope to control-flow-linear by invoking
`xlin`. In a control-flow-linear effect scope, we are allowed to use
both unlimited and linear resources, but only control-flow-linear
operations. Once the effect scope is switched to control-flow-linear,
all invocations of unlimited operations in this scope (even before
`xlin`) would cause type errors. Moreover, the switch is irreversible
because control-flow-linear effect variables can never be made
unlimited.


## Examples

1. We can mix control-flow-linear and control-flow-unlimited
   operations in a control-flow-unlimited effect scope (the default
   case). The anonymous effect variable `_` has kind `Any` by default
   which can be unified with any operations.

    ```
    links> fun() {do U; lindo L};
    fun : () {L:() =@ a,U:() => ()|_}-> a
    ```

2. We can only invoke control-flow-linear operations in a
   control-flow-linear effect scope (in other words, we cannot invoke
   control-flow-unlimited operations in a control-flow-linear effect
   scope). The linear control flow enables the usage of the linear
   channel `ch`. Now the effect variable `_` has kind `Lin`
   explicitly, which can only be unified with linear operations with
   signatures `=@`.

    ```
    links> fun(ch:End) {xlin; lindo L; close(ch)};
    fun : (End) {L:() =@ ()|_::Lin}~> ()
    ```

3. We can only handle control-flow-linear operations using a linear
   handler (indicated by `=@` in the clause) which guarantees the
   continuation is used exactly once.

    ```
    links> handle ({xlin; lindo L + 40}) { case <L =@ r> -> xlin; r(2) };
    42 : Int
    ```

4. We can handle control-flow-unlimited operations before entering a
   control-flow-linear effect scope as long as we guarantee that they
   are all handled. Note that after handling, the presence variable of
   `Choose` and row variable are both linear.

    ```
    links> fun(ch:End) { xlin; close(ch); handle ({if (do Choose) 40 else 2}) {case <Choose => r> -> r(true) + r(false)} };
    fun : (End) {Choose{_::Lin}|_::Lin}~> Int
    ```

5. When writing explicit quantifiers, we must explicitly annotate the
   kinds of row variables using `e::Row(Lin)` or `e::Row(Any)`. If the
   subkind is not specified, it means `Lin` instead of `Any` in order
   to be compatible with variants and records in Links which also use
   row variables. It is meaningful future work to explicitly separate
   value row variables and effect row variables in Links.

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

* This extension passes all previous tests with the flag disabled.
* Since this extension more focuses on the compatibility with the
  effect handler extension of Links, it passes all previous effect
  handler tests with the flag enabled, including
  * `/tests/handlers_with_cfl_on.tests`, and
  * `/tests/typecheck_examples_with_cfl_on.tests`.
* For other tests especially those use session types, since this
  extension changes some parsing/printing behaviours and does not
  allow linear resources by default, appropriate changes are required
  to make it pass the tests with the flag enabled, e.g.,
  `tests/sessions_with_cfl_on.tests`.