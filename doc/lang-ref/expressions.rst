Expressions
===========

Conditional expressions
-----------------------

A conditional expression has a condition, a consequent, and an else
clause. None of the three may be omitted.

::

  if (x == y) {
    expr1
  } else {
    expr2
  }

Curly braces can be wrapped around either clause, no matter how many
sub-expressions they contain, and *must* be so wrapped if you want
the clause to consist of more than one semicolon-separated expression.

Note that an ``if``-``else`` expression *always* returns a value in
Links; the return values of the two branches must be of the same type,
and both branches are required.


Variable binding
----------------

Variables are single-assignment in Links. The form::

  var x = expr;
  etc

evaluates ``expr`` and binds the name ``x`` to the resulting value,
within the expression ``etc``.

Variable assignments have block scope. The following example::

  var x = 1;
  if (condition) {
    var x = 2;
    ()
  } else {
    var x = 3;
    ()
  };
  print(intToString(x))

prints ``1`` because the assignments to ``c`` within the ``if`` clauses only
bind within those clauses. As conditionals are *expressions* rather than
*statements*, they return a result. Therefore, if you want the value printed to
depend on the condition, you should assign to ``x`` from the result of the whole
``if`` expression::

  var x = if (condition) {
    2
  } else {
    3
  };
  print(intToString(x))


Blocks
------

A *block* is a sequence of variable bindings and function definitions, followed
optionally by an expression.  A sequence of variable bindings separated by
semicolons is evaluated in text order, binding each value to the corresponding
name in what follows. Variables cannot be rebound; later bindings of the same
name shadow preceding ones::

    var x = 1;
    var y = 2;
    var x = 2;
    var z = x + y;    # z is now bound to 4

The scope of a binding is strictly within its immediate block. As a
result, the following code may not do what you expect::

    var x = 0;
    if (a == b) {
        var x = 1;
        ()
    } else {
        var x = 2;
        ()
    }
    alert(x);

The value printed by ``alert`` will be 0, because the other two
bindings only bind x within the corresponding clauses of the
conditional. This may come as a surprise to programmers used to
imperative languages.

It is possible to evaluate a block as the body of a ``var`` binding::

  var result = {
    var x = 1;
    var y = 2;
    x + y;
  }; # result is now bound to 3


Tuples
------

A tuple is a finite sequence of values, possibly of different
types. An *n*-tuple is constructed by enclosing *n* expressions in
parentheses and separating them with commas::

   (5, "OK")
   (true, "a string", 42)

The first tuple has type ``(Int, String)``, and the second tuple has type
``(Bool, String, Int)``.

You can deconstruct a tuple by either projecting a field number::

  links> ("hello", "world").1;
  "hello" : String

or alternatively, by deconstructing the tuple using a ``switch`` expression and
binding each field to an identifier::

  switch (("hello", "world")) {
    case (greeting, place) -> greeting
  }

  "hello" : String

Records
-------

A record is like a tuple, but its fields are indexed by field names rather than
integer indices. A record is written like a tuple but with fieldnames preceeding
the fields::

  (lastname="Bond", firstname="James", license="To kill")

Field names are generally not enclosed in quotes and are not expressions. Field
names can be quoted if the name clashes with a keyword, for example::

  links>   ("for" = 1, "query" = true, notAKeyword = "string");
  ("for" = 1, notAKeyword = "string", "query" = true) : (for:Int, notAKeyword:String, query:Bool)

Note that, whereas the content of a field can be any expression, the field name
must be literally present when constructing a record. For example::

  var item = (drinkname = "latte", price = 2.0 +. 0.5)          # OK
  var item = ("drink" + "name" = "latte", price = 2.0 +. 0.5)   # NOT OK

You can access the fields of a record by *projecting* them, using
dot notation::

  item.drinkname == "latte"
  (lastname="Bond", firstname="James").lastname == "Bond"

As with tuples, it is also possible to deconstruct a record by pattern matching
and binding each field to an identifier::

  var character = (firstname = "James", lastname="Bond");
  switch (character) {
    case (firstname=first, lastname=last) -> first ^^ " " ^^ last
  }

  "James Bond" : String

Record updates
~~~~~~~~~~~~~~

You can *add* a field to an arbitrary record using the *record
extension* operation. This operation works only when the field is not
already present in the record. Recall the ``item`` definition from above. We can
add a ``caffeineContent`` field as follows::

  (caffeineContent = 60 | item)

This would yield a value::

  (caffeineContent = 60, drinkname = "latte", price = 2.5)

To *overwrite* the value in a field, when that field is already
present, use the "with" notation::

  (item with drinkname = "capuccino")

This yields::

  (drinkname="capuccino", price=2.5)

Unlike in Haskell or OCaml, records do not need to be declared. It is acceptable
to use a particular field name with different types. For example::

  var x = (drinkname="capuccino", price=2.5);
  var y = (drinkname="capuccino", price="a lot of money");
  x

is fine (note that ``price`` has type ``Float`` in record ``x``, but not in
``y``).

Field punning
~~~~~~~~~~~~~

We often wish to update the field of a record with the contents of a variable
which has the same name::

  var drink = (drinkname="capuccino", price=2.5);
  var drinkname = "latte";
  (drink with drinkname = drinkname)

Links offers "record field punning" syntax which allows us to omit the field
name in such circumstances::

  var drink = (drinkname="capuccino", price=2.5);
  var drinkname = "latte";
  (drink with =drinkname)

Variants
--------

A "variant type" is one that uses explicit "tags" (or "labels") to
distinguish different sets of possible values as to their meaning. For
example, a mode of transport may be either Automobile or Camel. If it
is Automobile, we want to know what fuel it takes; if it is Camel, we
want to know how many humps it has. In Links, values like these can be
expressed as follows::

  Automobile(Diesel)
  Automobile(Unleaded)
  Camel(2)

The *type* that includes such values is written as follows::

  [| Automobile:
      [|Diesel | Unleaded | Biodiesel |]
   | Camel: Int |]

The box brackets ``[| |]`` delimit a variant type, and variant labels
are separated by a pipe ``|``. After each variant label, separated by a
colon ``:``, is the type of its contents--a Camel has a number of humps
so its content type is ``Int``, whereas the ``Automobile`` content type is
another variant type, ``[|Diesel | Unleaded|]``. The contents may be empty (as in
``Diesel``, ``Unleaded``, and ``Biodiesel``, in which case the ``:`` and content
type may be omitted.

In Links, a *variant tag always begins with a capital letter*. Any
string beginning with a capital letter, used in a value context,
denotes a variant label.

Case analysis
~~~~~~~~~~~~~

To inspect a variant value, use *pattern matching*. Pattern matching is
accomplished using the ``switch`` expression, which has a target expression
and a case for each variant label. The following expression determines
the effective number of humps of a transport (automobiles have no humps)::

  switch (target) {
    case Automobile(fuelType) -> 0
    case Camel(humpCount) -> humpCount
  }

The expression ``expr`` is evaluated to produce a value of variant
type; then the label is examined and one of the cases is chosen. The
lowercase word following the variant label in a case is bound to the
content of the target value (provided that case actually matches the
target). This allows us to use the variable ``humpCount`` within the
body of the ``Camel`` case. The body of a case (everything between the
``->`` and the next case (if any) or the end of the switch) produces
the result of the whole switch expression, and all case bodies of a
switch must have the same type.

Type-checking will ensure that all possible cases are matched by
the ``switch`` expression. To handle arbitrary variant values, you can
add an open case to the end of the switch::

  switch (target) {
    case Automobile(fuelType) -> 0
    case Camel(humpCount) -> humpCount
    case other -> 0
  }

Since ``other`` begins with a lowercase letter, it is a variable, which
matches any value. Unlike the variables in the previous cases, which
are wrapped inside variant labels, ``other`` is used here as the
complete pattern to match for its case, so it will match
anything. Patterns are tried in the order they are given, so the
``other`` case will not by selected unless the previous cases do not
match.

Functions
---------

Functions take arguments and produce a result.  Functions can be named or
anonymous. We can write a named function which sums three integers as follows::

  fun add3(x, y, z) {
    x + y + z
  }

Anonymous functions just omit the name: ``fun (x) { x + 1 }`` is an
expression that evaluates to an anonymous function value.

Function values, whether named or anonymous, are lexical closures; any
variables free in the body must refer to bindings from a surrounding
lexical scope. The smallest surrounding scope is chosen.

A function can be called by using its name, followed by a list of
arguments in parentheses::

    add3(1, 2, 7)

This works whether ``add3`` is a function defined with a name, as
``fun(x, y, z) { x + y + z}``, or a variable bound to a functional value, as::

    var add3 = fun(x, y, z) { x + y + z }
    add3(1, 2, 7)

``add3(1, 2, 7)`` returns 10.

Any expression that evaluates to a function value can be called::

    (if (true) fun (x) { x + 1 }
     else fun (x) { x + 2 })(3)

Recursion
~~~~~~~~~

Functions are treated as non-recursive by default. If a function refers to
itself, then it can call itself recursively. As an example, we can write the
naiive Fibonacci function as follows::

  fun fib(n) {
    if (n < 1) {
      0
    } else if (n == 1) {
      1
    } else {
      fib(n - 2) + fib(n - 1)
    }
  }

Note that we can recursively call ``fib`` in the ``else`` branch.

Mutually-recursive functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To define *mutually* recursive functions, both functoins should be wrapped in
a ``mutual`` block. As an example, consider the following pair of functions
which determines whether a given Peano-encoded number is even::

  typename Nat = [| Z | Succ:Nat |];

  mutual {
    sig isOdd : (Nat) ~> Bool
    fun isOdd(n) {
      switch(n) {
        case Z -> false
        case Succ(n) -> isEven(n)
      }
    }

    sig isEven : (Nat) ~> Bool
    fun isEven(n) {
      switch(n) {
        case Z -> true
        case Succ(n) -> isOdd(n)
      }
    }
  }

  isEven(Succ(Succ(Succ(Z))))

Operators
---------

Links supports the standard arithmetic operators::

   +     : (Int, Int) -> Int
   -     : (Int, Int) -> Int
   *     : (Int, Int) -> Int
   /     : (Int, Int) -> Int
   ^     : (Int, Int) -> Int
   mod   : (Int, Int) -> Int
   *.    : (Float, Float) -> Float
   +.    : (Float, Float) -> Float
   -.    : (Float, Float) -> Float
   /.    : (Float, Float) -> Float
   ^.    : (Float, Float) -> Float

As Links does not yet have any support for overloading, the floating
point versions are distinguished using the "." suffix. The arithmetic
operators can be used infix as is or prefix when enclosed in
parentheses.

As an example::

  1+2*3

returns ``7``, and::

  (*.)(6.0, 7.)

returns ``42.0``.


The ``(^^)`` operator is used for string concatenation::

  "hello" ^^ "world"

results in::

  "helloworld"

Lists
-----

A list is a finite sequence of values, constructed using ``[]``
(pronounced "nil") and ``::`` (pronounced "cons")::

   1 :: 4 :: 9 :: 16 :: []

A list can be created directly by wrapping a series of comma-separated
expressions between brackets::

    [1, 4, 9, 16]

    ["apple", "nectarine", "pear"]

    []

    x = true;
    [true, false, x, true]

Note that *all elements of a list must be of the same type*.

Lists support the "concatenate" operation, denoted by two plus
characters::

    [1, 2] ++ [3, 4, 5] == [1, 2, 3, 4, 5]

Lists are also comparable using the ``==`` operator.

The "cons" operator ``::`` appends an element to the start of a
list::

  links> 1 :: [2,3,4,5];
  [1, 2, 3, 4, 5] : [Int]

The head ``hd`` and tail ``tl`` functions each take a single list as
an argument. The ``hd`` function returns the first element of the list, and the
``tl`` function returns the list consisting of all elements from the original list except the first element::

  links> hd([1,2,3]);
  1 : Int

  links> tl([1,2,3]);
  [2, 3] : [Int]

Both functions are partial in that they can fail at runtime if given an empty
list.

The ``take`` and ``drop`` functions return the first ``n`` elements of
a list, and all *but* the first ``n`` elements of a list, respectively.

::

  links> take(2,[1,2,3]);
  [1, 2] : [Int]
  links> drop(2,[1,2,3]);
  [3] : [Int]

Pattern matching on lists
~~~~~~~~~~~~~~~~~~~~~~~~~

Cons and nil can also be used in patterns, to deconstruct lists. We can
deconstruct a list using a ``switch`` expression::

  switch (s) {
    case []    -> Empty
    case x::xs -> NonEmpty
  }

Integer Ranges
~~~~~~~~~~~~~~

The syntax ``[a .. b]`` constructs a list of all the integers between
``a`` and ``b``, inclusive. The result is empty if ``a`` is greater than
``b``.

As an example::

  links> [1..10];
  [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] : [Int]

Comprehensions
--------------

The main loop construct in Links is the list comprehension::

    for (x <- source)
       body

Both the source and the body should be expressions that evaluate to
lists.

The value of a comprehension is the concatenation of all the lists
produced by evaluating the body, once for each element of *source*, and:
binding that element to the variable ``x``. For example::

    var source_list = [1, 2, 3];
    for (x <- source_list)
        [ x*x ]

constructs a list of the squares of the values in C<source_list>. Note
that more than one value can be included in the body list::

    var source_list = [2, 3, 7, 8, 9, 55];
    for (n <- source_list)
        if (odd(n))
           [n, n+1]
        else
           [n]

This example returns ``[2, 3, 4, 7, 8, 8, 9, 10, 55, 56]``.

Other forms of looping can be implemented using tail recursion.

Filtering
~~~~~~~~~

A comprehension can be filtered using the ``where`` clause::

    var source = [2, 3, 4, 5, 6, 7];
    for (x <- source)
    where (odd(x))
      [x+1]

returns ``[4, 6, 8]``.

A ``where`` clause is equivalent to a condition nested within a
comprehension::

    for (x <- src)
    where (pred)
      expr

is equivalent to::

    for (x <- src)
      if (pred)
        expr
      else []

``where`` is a clause on ``for`` comprehensions: it cannot be used
outside of a ``for``.

Sorting
~~~~~~~

The ``orderby`` clause on ``for`` comprehensions is used to sort the
source before evaluating the body.

For example, suppose "models" is a list declared previously with type
``(release_year:Int,model_number:Int,model_name:String)``, describing
models of an automobile make. Then the following will return a list of
pairs describing the models, ordered by their year of release::

    for (m <- models)
    orderby (m.release_year)
      [(m.model_number, m.model_name)]


Multiple generators
~~~~~~~~~~~~~~~~~~~

A comprehension can draw elements from more than one list.
We say that a clause ``i <- xs`` for some list ``xs`` is a *generator*.
For each element produced by the first generator, Links iterates over all the
items produced by the remaining generators.

For example::

   links>
     for (fruit <- ["apple", "orange", "banana"], i <- [1..4])
       [(i, fruit)];
   [(1, "apple"),  (2, "apple"),  (3, "apple"),  (4, "apple"),
    (1, "banana"), (2, "banana"), (3, "banana"), (4, "banana"),
    (1, "orange"), (2, "orange"), (3, "orange"), (4, "orange")] : [(Int, String)]

You can also impose an order on all the elements produced by the
series of generators in a comprehension header, as in::

   links>
     for (fruit <- ["apple", "orange", "banana"], i <- [1..4])
     orderby (fruit)
       [(i, fruit)];
   [(1, "apple"),  (2, "apple"),  (3, "apple"),  (4, "apple"),
    (1, "banana"), (2, "banana"), (3, "banana"), (4, "banana"),
    (1, "orange"), (2, "orange"), (3, "orange"), (4, "orange")] : [(Int, String)]

Links will produce a list of tuple elements as dictated by the
generators, then sort them, and finally evaluate the body expression
for each element produced. Note that it is the source elements, *not*
the body elements, which are sorted.

The effect of multi-generator comprehensions is much like that of
nested comprehensions: the comprehension::

     for (fruit <- ["apple", "orange", "banana"], i <- [1..4])
       [(i, fruit)];

behaves just like this one::

     for (fruit <- ["apple", "orange", "banana"])
       for (i <- [1..4])
         [(i, fruit)];

But multi-generator comprehensions are different from the nested
counterparts when it comes to clauses such as C<orderby>. This is
because the C<orderby> clause sorts the list of tuples produced by all
the generators in the *most recent* comprehension header. When using
nested single-generator comprehesions, you are sorting one series of
elements which is then collected by another comprehension, for a
result than may not obey the desired ordering. For example::

   links> for (fruit <- ["apple", "orange", "banana"])
            for (i <- [1..4])
            orderby (i)
              [(i, fruit)];
   [(1, "apple"),  (2, "apple"),  (3, "apple"),  (4, "apple"),
    (1, "banana"), (2, "banana"), (3, "banana"), (4, "banana"),
    (1, "orange"), (2, "orange"), (3, "orange"), (4, "orange")] : [(Int, String)]
