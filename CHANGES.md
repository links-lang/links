# Unreleased

List of changes since the latest release.
* The package `links-mysql`, based on the `mysql` opam package is no
  longer supported. Instead, the package `links-mysql8`, based on the
  `mysql8` package is provided, which also supports version of MySQL
   prior to 8.
   
# 0.9.7

This release includes minor bug fixes, improvements, and a breaking
change.

* **Breaking change**: The Links standard library has adopted `Maybe`
  as it optional type. Previously, the standard library made
  inconsistent use of both `Option` and `Maybe` types. As a
  consequence of this change, every instance of `Option` has been
  turned into a `Maybe` type.
* Uniform paths: The file paths used by Links internally are now
  uniformly computed at compile time from the base of the current OPAM
  switch.
* Support for record extension in the presence of temporal projections.
* It is now to use `default` as a setting value in the REPL.
* Fixed an assertion failure in the relational lens code base.
* Effect aliases: It is now possible to define aliases for effect
  signatures, e.g.

```links
effectname State(s, e::Eff) = {Get:s, Put:(s) -> () |e};
```

defines an alias named `State` for an effect row with two operations
`Get`, and `Put`. This alias is analogous to aliases introduced via
`typename`, except that `effectname` aliases may be used in places
where an effect row is expected, e.g.

```links
sig f : () ~State(Int, {})~> ()
```

is equivalent to

```links
sig f : () {Get:Int, Put:(Int) -> ()}~> ()
```

* Client/server annotations are now permitted on local and anonymous
  functions.
* Fixed a bug where remote calls involving a client closure would
  cause the server side to crash.

# 0.9.6

This release extends the core features of Links and resolves various
minor bugs.

* Links now supports System F-style explicit type abstractions:
  For instance, writing `/\ [a, e::Row] { foo }` abstracts the expression `foo`
  over type variable `a` and row variable `e`. Here, `foo` must have a unique
  type and must be pure (to satisfy the value restriction).
* Fixed a bug in "mixing" query normalisation, which prevented certain queries using
  concatenation inside `for` statements from being correctly converted to SQL.
* Links now has basic support for temporal database operations. More information
  can be found on the [Wiki](https://github.com/links-lang/links/wiki/Temporal-Databases).
  There are new keywords: `valid`, `to`, `vt_insert`, `tt_insert`, and `TemporalTable`.
* A new commandline option `--compile` (shorthand `-c`) has been
  added, which runs Links in a "compile only" mode. In this mode the
  JavaScript compilation artefact can be saved to a file (the naming
  of this file is controlled via the commandline option `-o`). Note
  that the generated file may not be directly runnable without linking
  the runtime system first. Currently, the runtime system must be
  linked manually.
* Fixed a bug where calling either of `newAP`, `newClientAP`, and
  `newServerAP` on the client-side would crash the client.
* It is now possible to dispatch an MVU message from outside of the
  event loop. This is particularly useful, for example, when dealing
  with a persistent, stateful thread which is receiving messages from
  a server. New things include:
  + A new type alias `MvuHandle(msg)`.
  + A family of runners: `runHandle`, `runCmdHandle`,
    `runSimpleHandle` which return an `MvuHandle(msg)` rather than the
    unit value.
  + A new dispatcher `Mvu.dispatch : (msg, MvuHandle(msg)) ~> ()`,
    which directly dispatches a message to the MVU loop.

* The built-in webserver now supports SSL connections.  To enable
  secure connections, you must first obtain an adequate certificate
  and key, e.g. via Let's Encrypt or a self-signed certificate. The
  latter can be useful for testing, e.g. the following command starts
  an interactive process to create a self-signed certificate (that
  uses 4096 bits RSA encryption and is valid for 365 days):
```shell
openssl req -x509 -newkey rsa:4096 -keyout server.key -out server.crt -days 365 -nodes
```
  After obtaining a valid certificate, you must tell Links to run in SSL
  mode and you must also tell it how to locate the `key` and `crt`
  file. This can be done via a configuration file, e.g.

```
# ssl.config
ssl=true
ssl_cert_file=server.crt
ssl_key_file=server.key
```
  Then running `./links --config=ssl.config <file.links>` will cause
  the webserver to only serve requests via https.

  When a webpage is served via https, then the websocket layer will
  automatically communicate via the wss protocol.
* Other various bug fixes.

# 0.9.5

This is a minor hotfix release.

* The database query deduplication now correctly handles subexpressions recursively.
* Fixed a bug whereby messages received on the client-side would not
  be deserialised correctly.
* The Links runtime, now internally, uses `Lwt.pause` rather than the
  deprecated `Lwt_main.yield`. As a side effect we have updated the
  Lwt version constraint to be greater or equal to `5.0.0`.

# 0.9.4

## Queries mixing set and bag semantics
Links now provides experimental support for SQL queries mixing set and bag semantics.

When the `mixing_norm=on` flag is added to the configuration file, or when a query is defined in a `query mixing { ... }` block, Links will use a new query evaluator, allowing the programmer to call deduplication functions (`dedup` and `distinct`) within database queries. These are handled with set-based SQL statements `select distinct / union`, in addition to the usual bag-based `select / union all`.

    # will run on the DB as "select distinct e.dept as dept from employees"
    query mixing {
      dedup(for (e <-- employees) [(dept = e.dept)])
    }

Queries mixing set and bag semantics may, in some cases, require the use of the SQL:1999 keyword `lateral`; Links implements an optional query transformation to produce queries that do not use `lateral` (allowing the use of older DBMSs): this behaviour is enabled by using `query delat` in place of `query mixing`.

Further information on this feature is provided in the [Links GitHub wiki](https://github.com/links-lang/links/wiki/Deduplication-in-database-queries).

## DateTime type
Links now includes a primitive type, `DateTime`, for dates and times.
This is a *breaking change* from previous versions, where `dateToInt`
and `intToDate` operated on a record.

The primitive type allows us to better timezones, and also allows us
to work seamlessly with timestamps in the database.

Obtain a DateTime via:

  *  the `now()` function to get a timestamp for the current local time
  *  Using `parseDate` on an ISO-formatted string (e.g., `parseDate("2021-07-26 14:26:00+1")`)
  *  A `DateTime` field in the database
  *  `intToDate(X)` where `X` is a UNIX timestamp
  *  `beginningOfTime` and `forever`, which are special timestamps guaranteed to be less than (resp. greater than) all other timestamps

Project fields out of the type:

  * utcYear, utcMonth, utcDay, utcHours, utcMinutes, utcSeconds, utcMilliseconds projects the given field in the UTC time zone
  * localYear, localMonth, localDay, localHours, localMinutes, localSeconds, localMilliseconds projects the given field in the local time zone
  * dateYear, dateMonth, dateDay, dateHours, dateMinutes, dateSeconds, dateMilliseconds projects the given field in a given timezone (e.g., to project the hours field of a DateTime dt in BST, one would write `dateHours(dt, 1)`, where 1 is the timezone offset.

You can also print out the `DateTime` using `show` (which is an alias of
`showLocal`) and `showUTC`.

`DateTime`s are comparable as normal.

Due to limitations of the underlying library, the minimum timezone granularity is one hour. Unfortunately, this means we can't handle Indian timezones, for example.

## New surface syntax

### Presence type arguments

**Breaking change**: New syntax has been added to support type arguments of kind `Presence`. Here are some example of the syntax:

```links
typename T(p::Presence) = (foo{p});

(foo=4200) : T({:Int})         # Present with type Int
()         : T({-})            # Absent
(foo=4200) : T({%})            # Unnamed flexible variable
(foo=true) : T({%p})           # Named flexible variable
fun(r : T({_})) { () }         # Anonymous presence variable
fun(r : T({p})) { r : T({p}) } # Named presence variable
```

The syntactic sugar for effect and record fields which lets one omit the `()` has been removed in order to resolve the otherwise ambiguity between the presence type argument `{wild}` from the row type argument `{wild}`. Note however that it is still possible to omit the `()` for variant fields.

### Mono restriction

It is now possible to annotate type variables with `Mono` restriction, e.g. `sig id : (a::(Any,Mono)) -> a::(Any,Mono)`.

### Recursive rows

* Effect variables can be recursive, e.g. `{ |(mu a.F:(() { |a}-> ()) {}-> b|c)}`.
* **Breaking change**: Recursive rows are no longer restricted to variant syntax, i.e. separating fields using `|`. Recursive record and effects rows separate fields using `,` now.
* Recursive variants with no directly exposed fields no longer require the vertical bar separating fields from the row variable, e.g. `[|(mu a. Foo)|]` is equivalent to `[| |(mu a . Foo)|]`.

## Roundtrip: New pretty printer for types

This version of Links introduces a new pretty printer for types, called Roundtrip. This fixes various round-tripping issues.

The Roundtrip printer is now active by default. The old printer is still present.

The printer(s) to be used can be selected using the setting `types_pretty_printer_engine`, with the following values:

  * `roundtrip`: the new printer
  * `old`: the original printer
  * `derived`: no pretty printing - prints the OCaml representation of the types

Note that one can select multiple printers at once, for comparison; this is done by separating printer names by commas, e.g.:

```links
@set types_pretty_printer_engine "roundtrip,old";
```

## Effect Syntactic Sugar

This version implements enhanced syntactic sugar for effects. The changes influence both the Roundtrip printer (see above) and the desugaring passes (between parsing and typechecking).

(*Note: Most of effect sugar, and in particular the changes introduced in this version, requires the `effect_sugar` setting to be `true`.*)

There is a new setting `effect_sugar_policy` which allows one to set which components of effect sugar to use. The available options (with shortcuts for convencience) are:

  * `presence_omit` [shotcut `pres`]: omit presence polymorphic operations within effect rows
  * `alias_omit` [shortcut `alias`]: hide empty (and emptied using `pres`) shared effect rows in the last argument of aliases
  * `arrows_show_implicit_effect_variable` [shortcut `show_implicit`]: display the imlicit shared effect on arrows
  * `arrows_curried_hide_fresh` [shortcut `chf`]: in curried functions, argument collection arrows are assumed to have fresh effects and these are hidden
  * `contract_operation_arrows` [shortcut `contract`]: contract operation arrows: `E:() {}-> a` to `E:a` and `E:(a) {}-> b` to `E:(a) -> b`
  * `open_default` [shortcut `open`]: effect rows are open by default, closed with syntax `{ | .}`
  * `final_arrow_shares_with_alias` [shortcut `final_arrow`]: final arrow and a following type alias may share implicit effects
  * `all_implicit_arrows_share` [shortcut `all_arrows`]: all arrows with implicit effect vars will be unified, an experimental setting

Multiple of these can be selected, separated by commas, e.g.:

```links
@set effect_sugar_policy "pres,alias,contract";
```

A version of the above is also available by entering `@help effect_sugar_policy;` in Links.

These changes are explained in more depth and with examples in [Links GitHub Wiki/Effect Sugar](https://github.com/links-lang/links/wiki/Effect-Sugar).

## Other fixes / Miscellaneous

* Relational lenses are now enabled by default.
* Fixed a bug where the REPL would unconditionally print a stacktrace for unknown directives.
* Fixed a bug where deeply nested JSON literals would cause the client to stack overflow.
* Fixed a bug where big server side values would cause the client to stack overflow.
* Fixed JavaScript compilation of top-level anonymous functions.
* Fixed a bug where the server would inadvertently respond with response `500` following the (successful) termination of a server side process.
* The body of an escape expression has been made more permissive (grammatically) as it can now be any expression.

# 0.9.3

This minor release fixes a few bugs.

## MVU library is now distributed as part of Links

The JavaScript dependencies of the MVU library are now correctly
installed alongside Links. As a result the MVU examples now work
out-of-the-box following a fresh install of Links. For instance, the
following command will now successfully run the TODO example:

```
$ linx $OPAM_SWITCH_PREFIX/share/links/examples/mvu/todomvc/todoMVC.links
```

## Limited support for regular expressions in SQL where clauses

Links now support compilation of regular expressions in SQL where
clauses, however, only for regular expressions that can be translated
to SQL `LIKE` clauses. Consider the following example.

```
# Suppose we had configured two tables as follows
#  insert staff values (name, dept)
#    [(name = "Alice", dept = "math"),
#     (name = "Bob", dept = "computer science"),
#     (name = "Carol", dept = "dentistry")];
#  insert depts values (name, coffee_budget)
#    [(name = "mathematics", coffee_budget = 10000),
#     (name = "computer science", coffee_budget = 20000),
#     (name = "dentistry", coffee_budget = 30000)]

query flat {
  for (s <-- staff)
    for (d <-- depts)
       where (d.name =~ /.*{s.dept}.*/)
         [(name = s.name, dept = d.name, coffee_budget = d.coffee_budget)]
}
```

When `s` is bound to the record `(name = "Alice", dept = "math")` the
regular expression `.*{s.dept}.*` will match the department record
with `name = "mathematics"`, and thus the query yields

```
[ (coffee_budget = 10000, dept = "mathematics", name = "Alice")
, (coffee_budget = 20000, dept = "computer science", name = "Bob")
, (coffee_budget = 30000, dept = "dentistry", name = "Carol") ]
```

## Other fixes

* Compatibility with OCaml 4.12 (thanks to @kit-ty-kate).
* The webserver now correctly sends HTTP responses with code 500 for errors.
* Various internal improvements.

# 0.9.2

This minor release contains various bug fixes, improvements, and a
**breaking** change.

## Breaking change: Trailing semicolons are no longer permitted
The surface syntax of Links has been changed.  Up until now it was possible to
end a block with a semicolon.  A trailing semicolon was interpreted as
implicitly ending the block with a `()` expression.  The rationale for this
change is to make the Links syntax more consistent, i.e. now all blocks must end
with an explicit expression.  To sum up, previously both of the following were
allowed

```links
fun foo(x) {
  bar(y);
  baz(x);
}
fun foo'(x) {
  bar(y);
  baz(x)
}
```

Now the first form is no longer accepted. Instead you have to drop the semicolon
and either end the block with an explicit `()` or wrap the last expression in an
`ignore` application.

```links
fun foo(x) {
  bar(y);
  baz(x);
  ()
}

fun foo(x) {
  bar(y);
  ignore(baz(x))
}
```

A third option is to simply drop the trailing semicolon, though, this only works
as intended if the type of the last expression is `()`.


## SML-style function definitions

Links now supports "switch functions", a new syntax for defining functions in
terms of match clauses directly, similar to SML. This allows writing the
following function
```links
fun ack(_,_) switch {
  case (0, n) -> n + 1
  case (m, 0) -> ack(m - 1, 1)
  case (m, n) -> ack(m - 1, ack(m, n - 1))
}
```
instead of the following, more verbose version:

```links
fun ack(a, b) {
  switch(a, b) {
    case (0, n) -> n + 1
    case (m, 0) -> ack(m - 1, 1)
    case (m, n) -> ack(m - 1, ack(m, n - 1))
  }
}
```

Switch functions can also be anonymous, allowing function like the following:
```links
fun(_, _) switch {
  case (0, n) -> 0
  case (m, n) -> m + n
}
```
Note: currently switch function syntax is only supported for uncurried functions.
As switch functions have experimental status they are disabled by default. To
enable them you must set the option `switch_functions=true` in a
configuration file.
## Require OCaml 4.08

The minimum required OCaml version has been raised to 4.08.


## Miscellaneous

- Fixed a bug breaking the TODO list example (#812)
- Checkboxes and radio groups in form elements are now handled correctly (#903)
- Links supports MySQL databases again! (#858)
- Fixed a bug where the effect of `orderby` was inconsistent between database
  drivers w.r.t. reversing the order of results (#858)
- Relational lenses can now be used with MySQL and Sqlite3 databases, too (#897)
- Remove setting `use_keys_in_shredding`, behaving as if it was always true (#892)
- Remove setting `query`, behaving as if it was off
  (i.e., `query` behaves like `query flat`) (#892)
- Fixed a bug where regular expressions in nested queries did not work correctly
  (#852)
- Implemented support for negative patterns in let bindings (#811)




# 0.9.1

This minor release contains various bug fixes and improvements.

## Listing Command Line Options

Invoking Links with either `--help` or `--h` option causes a help
message to be printed to standard out. The help message describes the
usage format and lists some typical options with their default values.

```
usage: links.exe [options] [source-files [-- arguments]]

Options are:
     --config=<file>             Initialises Links according to the given configuration file (default: /home/links/.opam/4.09.0/etc/links/config)
 -d, --debug                     Prints internal debugging information (development) (default: false)
     --enable-handlers           Enables the effect handlers extension (default: false)
 -e, --evaluate=<expression>     Evaluates an expression
 -h, --help                      Print help message and exit
     --path=<dir[,dir']...>      Search paths for Links modules (default: .)
     --measure_performance       Instruments the server-side runtime to measure various performance characteristics (default: false)
 -r, --rlwrap                    Selects whether to use the native readline support in REPL mode (default: true)
     --optimise                  Optimises the generated code (default: false)
     --print-keywords            Print keywords and exit
     --session-exceptions        Enables the session exceptions extension (default: false)
     --set=<setting=value>       Sets the value of a particular setting via the commandline
 -v, --version                   Print version and exit
 -w, --web-mode                  Start Links in web mode (default: false)
```

The `--set` option provides a lightweight means for setting the value
of some setting at invocation of time of Links. The command line
options are lexically scoped, meaning the effect of later options may
shadow the effect of earlier options. For example using the options
`--set=debug=false --set=debug=true` will cause Links to start in
debug mode.

The command line interface also supports enabling of transitive
dependencies, meaning that is no longer necessary to pass
`--enable-handlers` at the same time as `--session-exceptions`. Simply
passing `--session-exceptions` starts Links with the effect handlers
runtime.

## Modules as a Core Feature

Modules are now a core feature and therefore enabled by default. The
command line option `-m` and setting `modules` have been removed.

## MVU Commands

The Links MVU library now supports commands. Commands allow
side-effecting computations to be performed inside the MVU event
loop. A particularly important side-effect is to spawn a computation
which evaluates asynchronously, returning a message when it is
complete.

Key changes:

- add a new type, `Command(Message)` which describes a computation which
  will produce a message of type Message
- revise most general type of updt function from `(Message, Model) ~>
  Model` to `(Message, Model) ~> (Model, Command(Message))`

You can see the gist of the new functionality in the
examples/mvu/commands.links example, which spawns an expensive
computation asynchronously and awaits the result.

## Simultaneous Support for Flat and Shredded Queries

It is now possible to specify whether a query should be treated as
flat or shredded. The query syntax has been extended as follows:

```
query [range] policy {
  ...
}
```

where `policy` is either `plain`, `nested`, or omitted. If `plain` is
used, then the query will evaluated using the default evaluator, and
if `nested` is used, then the query will be shredded. If the policy is
omitted, then the `shredding` setting is used to decide, as before.

## Relational Lenses: Support for Serial Type Columns

The Relational Lenses extension now supports postgresql serial type
columns. In Links, the serial type is encoded as a variant type
`Serial` with three constructors:

- `Key(Int)` which indicates a known key value retrieved from the
database.
- `NewKey` which indicates that a value should be chosen by the
database server.
- `NewKeyMapped(Int)` which is similar to `NewKey`, except that it
allows multiple entries to refer to the same key (e.g. in the case of
a table join, where two new entries are inserted referring to the same
right table).

## Miscellaneous

- Links now builds with Lwt version 5.
- Added the `log10` and `exp` functions to the standard library.
- The hear syntactic sugar is now more flexible (#739).
- Fixed process identifier serialisation (#759).
- Fixed printing of values and typenames in the REPL (#805).
- Fixed handling of linearity in arguments in anonymous functions (#797).

# 0.9 (Burghmuirhead)

Version 0.9 (Burghmuirhead) presents multiple major new features, bugfixes, as
well as significant changes and improvements to the Links internals.

## Major Changes

### Model-View-Update

Links now includes a Model-View-Update library for writing client code, as
pioneered by the Elm programming langauge. More can be found on
[the Wiki page](https://github.com/links-lang/links/wiki/Model-View-Update-(Elm-Architecture)).

### Relational Lenses: Dynamic Predicates and Typechecking

The implementation of relational lenses has been extended to support _dynamic_
predicates in `select` queries. Additionally, much work has been done in order
to allow better typechecking of relational lenses.

### Mutual Blocks

Mutually-recursive functions and types should now be enclosed in a `mutual`
block:

```
mutual {
  typename Even = [| Z | SuccE:Odd |];
  typename Odd = [| SuccO:Even |];

  sig three : () -> Odd
  fun three() {
    SuccO(SuccE(SuccO(Z)))
  }
}
```

This leads to significant improvements with the performance of the typechecker
when considering mutually-recursive types, simplifies the code, and solves
multiple bugs.

### FreezeML

FreezeML is a new approach to integrating first-class, System F-style
polymorphism with ML-style type inference. The system relies on not guessing
polymorphism, and the ability to "freeze" variables in order to suppress
instantiation. See the [TyDe abstract](http://tydeworkshop.org/2019-abstracts/paper19.pdf) for more details.

Version 0.9 includes an implementation of FreezeML:

  - Frozen variables (i.e., variables which are not instantiated) are written
    `~e`
  - Frozen lets (i.e., function definitions which never generalise their types)
    are written `~fun f(x) { x }`
  - Explicit function generalisation is written `$(fun(x) { x }` and produces a
    term of type `forall a::Any,b::Row. (a) -b-> a`
  - Explicit instantiation is written using `@`

The previous work on "explicit quantification" has been removed.

### Effect Sugar

If the `effect_sugar` flag is set, there are many improvements which decrease
the number of times where explicit effect variables are required.

  - Fresh effect variables are no longer generated by the parser
  - Higher-order functions share an effect variable with their calling context: for example,
    `map : ((a) -e-> b, [a]) -e-> [b]` can be written as `map : ((a) -> b, [a]) -> [b]`.
  - Type names can now be written with an implicit effect variable, for example
    `typename Comp(a) = () ~> a` can be written insetad of `Comp(a, e::Eff) = () ~e~> a, and
    `forever : Comp(()) ~> ()` can be written instead of `forever : Comp((), e) ~e~> ()`
  - Presence variables can be omitted from effects. As an example, we can
    write
      ```
      sig evalState : (s) ->
                    (Comp(a, {Get:s,Put:(s) {}-> () |_})) ->
                     Comp(a, {                      |_})
      ```
      instead of
      ```
      sig evalState : (s) ->
                    (Comp(a, {Get:s,Put:(s) {}-> () |_})) ->
                     Comp(a, {Get{_},Put{_}         |_})
      ```

### Non-user-visible changes

Much work on 0.9 has concentrated on significant non-user-visible changes. In
particular:

  - Desugaring passes which occur after the typechecker are now type-preserving
  - Changes to internal representation of types
  - Many internal refactorings

## Minor Changes

### Module system changes

 - Modules now work better on the REPL

 - `import` is now used to allow a module to be used in the current file,
   whereas `open` brings it into the global scope. To get the previous
   behaviour, use `open import X` for a module `X`. (breaking change)

 - Many behind-the-scenes improvements and bugfixes

### Other minor changes

 - Compilation on OCaml 4.08 now supported
 - `select` queries now printed when debugging enabled
 - Query shredding now supported in SQLite
 - The `End` session type is now linear, and must be eliminated using `close`.
   This solves a class of memory leaks with session-typed applications.
 - Record field punning is now supported. For example, instead of writing
   `var hello = "hi"; (hello = hello, world="universe")`, it is now possible to
   write `var hello = "hi"; (=hello, world="universe")`.
 - `lines`, `unlines`, `partition`, and `span` added to prelude
 - Hyphenated HTML attributes are now allowed
 - "Primed" variables such as `x'` are now allowed
 - Initial (hacky) support for null integers in the database. If
   the `coerce_null_integers` setting is set to `true`, then `null_integer`
   (default -1) will be used instead of crashing at runtime.
 - Fix draggable list (sessions), draggable cropping frame examples

# 0.8 (Merchiston)

## Relational Lenses

Links 0.8 introduces support for Incremental Relational Lenses. See [the
paper](https://arxiv.org/pdf/1807.01948.pdf) for details.


## Minor Changes

  1. Session exception handling is now more liberal, allowing variables
     to be shared between the success and failure blocks.

  2. Migrate to the Dune build system.

## Bugfixes

  1. Fix non-typechecking examples.
  2. Fix type signatures for `domSetAttributeFromRef`,
     `domSetPropertyFromRef`, and `domSetStyleAttrFromRef`.
  3. Fix buggy implementation of `getValue` in `jslib`.
  4. Fix `domGetChildrenFromRef` to return only DOM elements
     instead of whitespaces

# 0.7.3

This minor release contains various bug fixes and improves the dynamic
loading facility for database drivers that was introduced in the
previous release.

## Lazy Loading of Present Database Drivers

The loading of present database drivers is now deferred until an
actual attempt to connect to a database is made.

# 0.7.2

Version 0.7.2 contains mainly behind-the-scenes improvements, however they may
have a large impact on performance.

## Package Structure and JBuilder

Links now uses JBuilder as its build system. In particular, this means that
there are now multiple Links packages:

  - `links` contains the executable
  - `links-core` exposes the Links source as a library
  - (optional) `links-postgresql` is a dynamic loadable database driver for PostgreSQL.

Additionally, `links-mysql` and `links-sqlite` are dynamically-loadable database
drivers for MySQL and SQLite3 respectively, but while present in the source, are
not formally released or officially supported.

## Exception Handling with Session Types

A recent [draft paper](http://simonjf.com/drafts/chomp-draft-nov17.pdf)
describes an exception handling mechanism which works in the presence of session
types. This is included in 0.7.2.
You can find out more on the [wiki](https://github.com/links-lang/links/wiki/Exceptions-and-Session-Types).

## Performance Improvements

  - The JavaScript runtime library is now tail-call-optimised.

  - Links now uses linked lists on the client-side, as opposed to JS arrays. As
    a rough idea of the performance increase: "The running time for calling map
    on a 100,000 element list went from browser-angrily-kills-page-after-30-seconds
    to taking around 1 second."

## Bugfixes
  - Issue 293: REPL responses now go to stdout and not stderr.
  - Issue 149: inner functions may now have type annotations.
  - Issue 280: polymorphic FFI annotations are now correctly universally quantified.

# 0.7.1

Fixes some issues with configuration files, and allows better navigation on the examples pages.

# 0.7 (Dalry)

Version 0.7 of Links brings many new features (some of which have been in the works for over two years!)

## Handlers for Algebraic Effects
Algebraic effects abstractly specify operations, which are given meaning by
handlers. These provide a powerful mechanism for designing effectful code.

Read more in Hillerström & Lindley's
[Liberating Effects with Rows and Handlers](http://homepages.inf.ed.ac.uk/slindley/papers/links-effect.pdf) and in
`examples/handlers`.

## Distributed Concurrency
Whereas before, communication using session types and actor-style processes was
limited to processes on the same concurrency runtime, Dalry provides the
infrastructure to use these concurrency primitives across concurrency runtimes.
See more in `examples/distribution`.

## Javascript FFI
Dalry brings support for natively calling JavaScript functions from Links client
code. You can read more on the [wiki page](https://github.com/links-lang/links/wiki/JavaScript-FFI).

## Standard Library
Links now has support for a standard library. We are hoping to gradually move
things out of the more monolithic prelude, and support more data structures and
algorithms out of the box. For now, we have included a minimal node-indexed
binary tree library, which you can load with `open BinaryTree`.

## Support for Programmatic XML Construction
Links now supports the ability to create XML from variants, and variants from XML.
These are implemented in the `xmlToVariant` and `variantToXml` functions.

## Minor Changes

  * Links now has native readline functionality by default. To turn this off, run with the `-r` flag.
  * Many bugfixes.

# Version 0.7 (Dalry)

Version 0.7 of Links brings many new features (some of which have been in the works for over two years!)

## Handlers for Algebraic Effects
Algebraic effects abstractly specify operations, which are given meaning by
handlers. These provide a powerful mechanism for designing effectful code.

Read more in Hillerström & Lindley's
[Liberating Effects with Rows and Handlers](http://homepages.inf.ed.ac.uk/slindley/papers/links-effect.pdf) and in
`examples/handlers`.

## Distributed Concurrency
Whereas before, communication using session types and actor-style processes was
limited to processes on the same concurrency runtime, Dalry provides the
infrastructure to use these concurrency primitives across concurrency runtimes.
See more in `examples/distribution`.

## Javascript FFI
Dalry brings support for natively calling JavaScript functions from Links client
code. You can read more on the [wiki page](https://github.com/links-lang/links/wiki/JavaScript-FFI).

## Standard Library
Links now has support for a standard library. We are hoping to gradually move
things out of the more monolithic prelude, and support more data structures and
algorithms out of the box. For now, we have included a minimal node-indexed
binary tree library, which you can load with `open BinaryTree`.

## Support for Programmatic XML Construction
Links now supports the ability to create XML from variants, and variants from XML.
These are implemented in the `xmlToVariant` and `variantToXml` functions.

## Minor Changes

  * Links now has native readline functionality by default. To turn this off, run with the `-r` flag.
  * Many bugfixes.

# 0.6.1

Bugfix release:

  * install required JavaScript libraries, examples and a default configuration
    file

  * Automatically locate config file inside the current directory or inside OPAM
    installation

  * Correctly support multiple remote clients

# 0.6 (Gorgie)

Version 0.6 of Links brings many changes.

## Session Types

Session types are a type system for communication channels, allowing
conformance to communication protocols to be checked at
compile-time. Links supports session types natively; for more
information, see the examples/sessions directory.

## Server-side concurrency

Links now implements message-passing concurrency on the server-side
using OCaml's light-weight threading (Lwt) library.

## Application Server

A major addition in the Gorgie release is the ability to run Links
applications using a standalone web server, as opposed to using Links
as a CGI interpreter.  This allows Links to be set up more quickly,
and for the easier development of applications where state must
persist over multiple requests. For more information, see the
examples/webserver directory.

## Modules

Links now supports an experimental simple modules system, allowing the
development of modular code over multiple files. To enable modules,
either run Links with the -m flag, or set the "modules" setting to
true in the configuration file.

To see examples of modules in action, check examples/webserver/examples.links.

## Recursive Type Inlining

Recursive types can now be written without explicitly writing
recursive type variables. For example, whereas before a list could be
defined as:

> typename List(a) = mu X . [| Nil : (), Cons : (a, X) |]

It is now possible to write:

> typename List(a) = [| Nil : (), Cons : (a, List(a)) |]


## Real Pages

Previously Links offered two web modes: server mode and client
mode. In server mode web pages were built on the server and it was not
possible to generate any JavaScript in the web page. In client mode a
stub was generated on the server. The stub contained JavaScript which
generated the actual content of the web page dynamically using the DOM
API.

Now, web pages are always generated on the server, but they may
contain JavaScript. In particular, they may include embedded event
handlers implemented by processes which are spawned on the server,
serialised, and sent to the server along with the body of the web
page.

In order to spawn a process on the server that needs to run on the
client, use the spawnClient keyword in place of spawn. This is the
primary change that needs to be made when adapting old code to work
with the realpages feature.

## Shredding

Links now includes experimental support for query shredding. This
allows queries with nested result types to be written. A nested query
is guaranteed to translate to at most n SQL queries, where n is the
nesting depth of the source query. It currently only works for the
postgresql database back end. The shredding extension is enabled by
configuring the shredding setting to the value true.
