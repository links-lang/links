.. _lexical_syntax:

Lexical Syntax
==============

Identifiers
-----------

Links identifiers begin with a lowercase letter and can contain lower- and
uppercase letters, underscores, numbers, and primes ``'``.

**Valid identifiers**: ``hello``, ``helloWorld``, ``hello5``, ``hello'``, ``hello``

**Invalid identifiers**: ``5hello``, ``_hello``, ``Hello``

Literals
--------

Links supports standard base literals:

  * Strings: ``"hello"``
  * Floating point numbers: ``0.``, ``1.0``, ``1.5``
  * Booleans: ``true``, ``false``
  * Characters: ``'c'``, ``'9'``, ``'\012'``

Comments
--------

Comments are introduced by a hash symbol, ``#``, and continue for the
rest of the line.

Keywords
--------

The following keywords are reserved and are unavailable for use as identifiers::

  alien as by case client database default delete delete_left
  determined do else escape false for forall from fun formlet
  handle if in lens lensdrop lensget lensput lensselect lensjoin
  lenscheck yields import insert linfun module mu mutual native
  nu offer on orderby op open otherwise page prefix query raise
  readonly receive returning select server set shallowhandle sig
  spawn spawnClient spawnAt spawnAngel spawnAngelAt spawnWait
  switch table TableHandle true try typename update unsafe
  values var where with tablekeys

XML
---

Links supports parsing XML literal notation. An XML expression is introduced by an
XML start-tag such as ``<foo>``. Inside the XML, you may escape from the XML
mode back into Links syntax by enclosing the Links code within braces ``{ }``.
When the literal is evaluated, all its escaped expressions will be
evaluated, and their values will be embedded within the constructed
XML value. For example::

    <html>
      <ul>
        <li> First item: {item1} </li>
        <li> Second item: {item2} </li>
      </ul>
    </html>

Within an XML literal, a variety of special features can be used to
handle browser events. These are described in the pages on :ref:`web_ref`.

To introduce a *forest* of XML nodes that have no mutually enclosing
XML element, use the ``<#>...</#>`` syntax::

    <#>
      Name: <b>{name}</b>
    </#>

Here, a *sequence* of nodes (first, an XML text node, and then a ``<b>``
element) becomes as a single expression. This value can be included
directly in other XML expressions::

    var nameLine = <#> Name: <b>{name}</b> </#>;
    <div>
      {nameLine} <br />
      The Links Team
    </div>
