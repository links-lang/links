# Installing Links

We recommend installing Links using the OPAM tool (version >= 2.1), available from [opam.ocaml.org](https://opam.ocaml.org/).

Links requires at least OCaml 4.08.0 to build and install. Therefore
you should first install the minimum (or greater) required version of
OCaml:

```
$ opam switch install 4.08.0
```

You can then install Links and its database drivers simply by issuing the commands:

```
$ opam install links links-postgresql links-mysql links-sqlite3
```

Alternatively, if you don't have PostgreSQL, MySQL, and Sqlite3 installed then do:

```
$ opam install links
```

This will put Links and its files into your OPAM switch. The
environment variable `$OPAM_SWITCH_PREFIX` points to your current
active switch. Links files install to several locations inside an OPAM
switch:

* The executable is called `linx` and goes into `$OPAM_SWITCH_PREFIX/bin`
* The Links prelude and JavaScript libraries go into `$OPAM_SWITCH_PREFIX/lib/links`
* The example Links programs go into `$OPAM_SWITCH_PREFIX/share/links`
* Links configuration file goes into `$OPAM_SWITCH_PREFIX/etc/links`

# Running Links.

Links supports running applications via a standalone application
server.

## Examples (without a database)

To run the examples that don't use the database with the Links
application server issue the following command:
```
$ linx --path=$OPAM_SWITCH_PREFIX/share/links/examples --path=$OPAM_SWITCH_PREFIX/share/links/examples/games $OPAM_SWITCH_PREFIX/share/links/examples/webserver/examples-nodb.links
```
(This version just displays the source code for examples that use the
database.)

The default port is 8080, so you should now be able to access the
examples by pointing your browser at:

  http://localhost:8080/

## Examples (with a Postgres database)

To run the examples using the Links application server issue the
following command:

```
$ linx --path=$OPAM_SWITCH_PREFIX/share/links/examples --path=$OPAM_SWITCH_PREFIX/share/links/examples/games --path=$OPAM_SWITCH_PREFIX/share/links/examples/dictionary $OPAM_SWITCH_PREFIX/share/links/examples/webserver/examples.links
```

* The `--path` option tells Links to look for source files in the following
  directories under `$OPAM_SWITCH_PREFIX/share/links`:
  - examples
  - examples/games
  - examples/dictionary
* Finally examples/webserver/examples.links is the file to run, which imports
  the example modules and sets up the application server.

But first you need to do a few things.

  1) By default Links uses a config file inside `$OPAM_SWITCH_PREFIX/etc/links` directory.  Use
     `--config=/custom/config/file` option to use a different configuration file.

  2) The default config file should contain paths to JavaScript libraries:

         jsliburl=/lib/js
         jslibdir=$OPAM_SWITCH_PREFIX/lib/links/js

     To use database examples you need to add a database configuration
     to the config file:

         database_driver=postgresql
         database_args=localhost:5432:fred:

     The database user fred should exist. With postgres, perhaps the
     easiest way to do this is to tie fred to your unix username and
     then no separate authentication is required.

  3) Create appropriate databases. If you are using postgres and you
  have tied your username to the database as described in 2) then you
  can use the scripts in examples/dbsetup.

    $ cd examples/dbsetup
    ./createdbs
    ./populatedbs

  Otherwise you can adapt them to your particular database set up.

  Postgres data dumps for the larger databases (citations and
  dictionary) can be obtained from a separate repository: https://github.com/links-lang/links-data.

For convenience you may want to add the `path` to the config file

    path=$OPAM_SWITCH_PREFIX/share/links/examples,$OPAM_SWITCH_PREFIX/share/links/examples/games,$OPAM_SWITCH_PREFIX/share/links/examples/dictionary

Then simply issue the following command to run the database examples application:
```
$ linx $OPAM_SWITCH_PREFIX/share/links/examples/webserver/examples.links
```

# Documentation

Some (outdated) documentation can be built by running the Makefile in
the doc directory.

Some very incomplete documentation is available on the links wiki:

  https://github.com/links-lang/links/wiki
