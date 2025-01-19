Settings and compiler flags
===========================

Settings
--------

TODO: Write me

Flags
-----

As well as reading settings from a configuration file, Links supports the
following flags which can be provided to the executable.  Some flags have a
"short option", which allows them to be stacked. Some flags take an argument.
For example, the following invocation::

  linx -dr --path=/home/user/links-files

will invoke Links in debug mode, disable native readline support, and set the
module resolution path to ``/home/user/links-files``.

The full list of flags can be found below.

+--------------+---------------------------+------------------------------------------------------------------+
| Short option | Long option               | Description                                                      |
+--------------+---------------------------+------------------------------------------------------------------+
| ``-d``       | ``--debug``               | Turns debug mode on                                              |
+--------------+---------------------------+------------------------------------------------------------------+
| ``-w``       | ``--web_mode``            | Turns web mode on (deprecated)                                   |
+--------------+---------------------------+------------------------------------------------------------------+
|              | ``--optimise``            | Turns optimisation on                                            |
+--------------+---------------------------+------------------------------------------------------------------+
|              | ``--measure_performance`` | Prints measurements for memory and execution time                |
+--------------+---------------------------+------------------------------------------------------------------+
| ``-n``       | ``--no-types``            | Does not print types when printing expressions                   |
+--------------+---------------------------+------------------------------------------------------------------+
| ``-e <ex>``  | ``--evaluate=<ex>``       | Evaluates the expression ``ex``                                  |
+--------------+---------------------------+------------------------------------------------------------------+
| ``-m``       | ``--modules``             | Turns on modules (removed after v0.9)                            |
+--------------+---------------------------+------------------------------------------------------------------+
|              | ``--print-keywords``      | Prints keywords and quits                                        |
+--------------+---------------------------+------------------------------------------------------------------+
|              | ``--pp=<path>``           | Runs the preprocessor located at ``path``                        |
+--------------+---------------------------+------------------------------------------------------------------+
|              | ``--path=<path>``         | Sets the module resolution path to ``path``                      |
+--------------+---------------------------+------------------------------------------------------------------+
|              | ``--config=<path>``       | Uses the configuration file at ``path``                          |
+--------------+---------------------------+------------------------------------------------------------------+
|              | ``--enable-handlers``     | Enables effect handlers                                          |
+--------------+---------------------------+------------------------------------------------------------------+
| ``-r``       | ``--rlwrap``              | Disables native readline support, allowing ``rlwrap`` to be used |
+--------------+---------------------------+------------------------------------------------------------------+
|              | ``--session-exceptions``  | Enables session exceptions                                       |
+--------------+---------------------------+------------------------------------------------------------------+

