.. _install:

Installation
============

In this guide, we will get Links installed.
We recommend Links can be installed using the Opam_ tool, which is used to distribute OCaml
packages.

Links is supported on Linux and Mac OSX. On Windows, we recommend using `Windows Subsystem for Linux`_.

Installing OPAM
---------------

We recommend installing OPAM using the script referred to on the `OPAM installation pages`_.
OPAM installations on Linux distribution repositories tend to be out of date,
and Links requires at least OPAM 2.

An OPAM "switch" refers to an environment for a particular compiler.

Once OPAM is installed, be sure to execute ``opam init``.

A note on ``bubblewrap``
~~~~~~~~~~~~~~~~~~~~~~~~

A dependency of OPAM 2 is the `bubblewrap` sandboxing tool.
If you are on Ubuntu 16.04, unfortunately this is not available from the default
repositories. You can, however, add a `custom repository`_.


Installing System Dependencies
------------------------------

You will need the ``m4`` package, and ``gcc``.

Installing Links
----------------

Once the system dependencies are installed, you can install Links from OPAM by
writing::

  opam install links

This will install Links and its files into your OPAM switch. The
environment variable ``$OPAM_SWITCH_PREFIX`` points to your current
active switch. Links files install to several locations inside an OPAM
switch:

* The executable is called ``linx`` and goes into ``$OPAM_SWITCH_PREFIX/bin``
* The Links prelude and JavaScript libraries go into ``$OPAM_SWITCH_PREFIX/lib/links``
* The example Links programs go into ``$OPAM_SWITCH_PREFIX/share/links``
* Links configuration file goes into ``$OPAM_SWITCH_PREFIX/etc/links``

If your ``opam`` switch is set up correctly, you should be able to get to the
Links REPL by typing ``linx`` at a command line. If not, writing ``eval $(opam
env)`` should set the system path correctly.

Installing the Links Database Drivers
-------------------------------------

Links supports interfacing with databases. The database drivers are not
installed by default.

PostgreSQL
~~~~~~~~~~

Running::

  opam install links-postgresql

will install the ``postgresql`` library, the required Links PostgreSQL driver,
and recompile ``links`` to allow it to use the driver.


Where next?
~~~~~~~~~~~

Once you have installed Links, see the pages on :ref:`getting_started`.


.. _Opam: https://opam.ocaml.org
.. _OPAM installation pages: https://opam.ocaml.org/doc/Install.html
.. _custom repository: https://launchpad.net/~ansible/+archive/ubuntu/bubblewrap
.. _Windows Subsystem for Linux: https://docs.microsoft.com/en-us/windows/wsl/faq
