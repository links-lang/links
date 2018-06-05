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
