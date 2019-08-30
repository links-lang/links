Modules
=======

Links supports a basic namespacing system, which allows the definition of
modules both inside and outside of a file.

Basic use
---------

A module in a file is introduced using the ``module`` keyword. Suppose the
following is in file ``foo.links``::

  module Foo {
    typename FooInt = Int;

    sig bar : FooInt
    var bar = 5;

    fun baz() {
      print(intToString(bar))
    }
  }

  fun main() {
    Foo.baz();
    open Foo;
    baz()
  }

  main()

A module consists of a series of variable and function declarations and type
aliases, and can also contain further modules. A variable or function
declaration inside a module must be used within the same module or qualified by
the module name. A binding can be used unqualified if the module is brought into
scope by ``open``.

Running ``links foo.links`` would result in ``5`` being printed twice.

Multi-file modules
------------------

We can also import other files as modules. As an example, we can use the
``foo.links`` file as a module in the following code::

  import Foo;
  Foo.baz()

The ``import`` statement tells Links that the file ``foo.links`` should be
included as a module ``Foo``. It is also possible to both import *and* bring a
module into scope by using ``import`` and ``open`` together, either separately
or using the combined ``open import`` statement::

  open import Foo;
  baz()

The paths used to find Links files to be used in module resolution can be
specified using the ``--path`` command line argument::

  linx --path=/home/user/links-files/ bar.links

Additionally, the path can be specified in a configuration file::

  path=/home/user/links-files/

Future plans
------------

The current Links module system is rather basic. We are currently extending the
module system to allow a more ML-like module system, including abstract types,
functors, and module signatures.

