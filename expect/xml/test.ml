open Links_expect.Test_common
open Expect_test_common.Expectation
open Expect_test_common.Expectation.Body
open Expect_test_common.File.Location


let%expect_test "Braced XML" =
  run_expr {|<f>{for (i <- []) <br/>}</f>|};
  [%expect {|
    <f/> : Xml
    exit: 0 |}]

let%expect_test "Escaped braces" =
  run_expr {|<p>A left: {{ and a right: }}</p>|};
  [%expect {|
    <p>A left: { and a right: }</p> : Xml
    exit: 0 |}]

let%expect_test "Escaped braces (not doubled)" =
  run_expr {|<p>A left: {{ and a right: }</p>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character : }
      <p>A left: {{ and a right: }</p>
                                  ^ |}]

let%expect_test "Backslashes" =
  run_expr {|<p>A backslash \ </p>|};
  [%expect {|
    <p>A backslash \ </p> : Xml
    exit: 0 |}]

let%expect_test "Top-level-bound XML object" =
  run_expr {|fun main() { var x = for (i <- []) <br/>; <f>{x}</f> } main()|};
  [%expect {|
    <f/> : Xml
    exit: 0 |}]

let%expect_test "Let-bound XML object" =
  run_expr {|{var x = for (i <- []) <br/>; <f>{x}</f>}|};
  [%expect {|
    <f/> : Xml
    exit: 0 |}]

let%expect_test "Whitespace preservation" =
  run_expr {|<a b="c"> <d/> </a>|};
  [%expect {|
    <a b="c"> <d/> </a> : Xml
    exit: 0 |}]

let%expect_test "Element splicing [1]" =
  run_expr {|{var x = "three"; <a b="c">{stringToXml(x)}</a>}|};
  [%expect {|
    <a b="c">three</a> : Xml
    exit: 0 |}]

let%expect_test "Element splicing [2]" =
  run_expr {|{var x = "hre"; <a b="c">t{stringToXml(x)}e</a>}|};
  [%expect {|
    <a b="c">three</a> : Xml
    exit: 0 |}]

let%expect_test "Attribute splicing [1]" =
  run_expr {|{var x = "three"; <a b="{x}"><c/></a>}|};
  [%expect {|
    <a b="three"><c/></a> : Xml
    exit: 0 |}]

let%expect_test "Attribute splicing [2]" =
  run_expr {|{var x = "three"; <a b="a{x}b"><c/></a>}|};
  [%expect {|
    <a b="athreeb"><c/></a> : Xml
    exit: 0 |}]

let%expect_test "Rejection of incorrectly nested elements" =
  run_expr {|<a><b></a></b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Closing tag 'a' does not match start tag 'b'.
      <a><b></a></b>
                ^ |}]

let%expect_test "Rejection of incorrectly typed attributes" =
  run_expr {|{var x = 3; <a b="{x}"><c/></a>}|};
  [%expect {|
    exit: 1
    <string>:1: Type error: XML attributes must have type `String', but the expression
        `{x}'
    has type
        `Int'
    In expression: <a b="{x}"><c/></a>. |}]

let%expect_test "Rejection of incorrectly typed l:attributes" =
  run_expr {|{var x = 3; <a l:href="{x}"><c/></a>}|};
  [%expect {|
    exit: 1
    :0: Type error: The function
        `<dummy>'
    has type
        `(() -a-> Page) ~b~> String'
    while the arguments passed to it have types
        `() -a-> Int'
    and the currently allowed effects are
        `wild'
    In expression: <dummy>. |}]

let%expect_test "Reject nonsense l:name attributes" =
  run_expr {|<form><input l:name="{1+1}" /></form>|};
  [%expect {|
    exit: 1
    <string>:1: Error compiling attributes: Illegal l: attribute in XML node
    In expression: <input l:name="{1+1}" />. |}]

let%expect_test "Accept okay l:name attributes" =
  run_expr {|<form l:action="{page <html><body>{stringToXml(foo)}</body></html>}"><input l:name="foo"/></form>|};
  [%expect {|
    <form action="#"><input value="hJWmvgAAACQAAAAJAAAAGgAAABmgAua3BZagAQS+kKACMoDqsaCgJDEyMTOgAua3BZagAQioQEA=" type="hidden" name="_k"/><input name="lname__g135" id="_lnameid__g134"/></form> : Xml
    exit: 0 |}]

let%expect_test "Looping in XML quasis" =
  run_expr {|var things = [1, 2]; <ul>{for (x <- things) <br/>}</ul>|};
  [%expect {|
    <ul><br/><br/></ul> : Xml
    exit: 0 |}]

let%expect_test "Amp-encoding (OK)" =
  run_expr {|<xml>&lt;</xml>|};
  [%expect {|
    <xml>&lt;</xml> : Xml
    exit: 0 |}]

let%expect_test "Amp-encoding (ill-formed XML)" =
  run_expr {|<xml>this & that</xml>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character : &
      <xml>this & that</xml>
                 ^ |}]

let%expect_test "Amp-encoding (converting from string)" =
  run_expr {|var x = "this & that"; <xml>{stringToXml(x)}</xml>|};
  [%expect {|
    <xml>this &amp; that</xml> : Xml
    exit: 0 |}]

let%expect_test "Looping in XML quasis, with multiple content elements." =
  run_expr {|var things = [1, 2];<ul>{for (x <- things) <#><li>{stringToXml(intToString(x))}</li><li>1</li></#>}</ul>|};
  [%expect {|
    <ul><li>1</li><li>1</li><li>2</li><li>1</li></ul> : Xml
    exit: 0 |}]

let%expect_test "XML forest" =
  run_expr {|<#>foo<a/><b>bar</b>fubar</#>|};
  [%expect {|
    foo<a/><b>bar</b>fubar : Xml
    exit: 0 |}]

let%expect_test "Labelization bug" =
  run_expr {|(escape cc in <form l:action="{exit (cc)}" />) ++ <p/>|};
  [%expect {|
    <form action="#"><input value="hJWmvgAAAD8AAAARAAAAMQAAAC+gAua3BZagAQS+kKACMoDqsaCgJDEyMTOgAua3BZagAQimkKACMoDqsaCgJDIyMTOgAg1zZ9egoAEIqUBAQEA=" type="hidden" name="_k"/></form><p/> : [XmlItem]
    exit: 0 |}]

let%expect_test "XML Construction" =
  run_expr {|<#>{[makeXml("foo", [ ("id", "bar"), ("class", "baz") ], <div/>)]}</#>|};
  [%expect {|
    <foo id="bar" class="baz"><div/></foo> : Xml
    exit: 0 |}]

let%expect_test "XML Construction 2" =
  run_expr {|[ makeXml("foo", [ ("id", "bar"), ("class", "baz") ], <div/>) ]: Xml|};
  [%expect {|
    <foo id="bar" class="baz"><div/></foo> : Xml
    exit: 0 |}]

let%expect_test "XML Construction 3" =
  run_expr {|makeXml("foo", [ ("id", "bar"), ("class", "baz") ], <div/>)|};
  [%expect {|
    <foo id="bar" class="baz"><div/></foo> : XmlItem
    exit: 0 |}]

let%expect_test "Unpretty-printing attributes of XmlItem" =
  run_expr ~args:["--set=print_pretty=false"] {|<foo id="bar" class="baz"><div/></foo>|};
  [%expect {|
    <foo id="bar" class="baz"><div/></foo> : Xml
    exit: 0 |}]

let%expect_test "Unpretty-printing long snippets (#256)" =
  run_expr ~args:["--set=print_pretty=false"] {|<a><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/></a>|};
  [%expect {|
    <a><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/></a> : Xml
    exit: 0 |}]

let%expect_test "XML-Variant-Conversion" =
  run_expr {|variantToXml(xmlToVariant(<div class="i-am-a-div"><ul><li/><li></li><li>A</li></ul></div>))|};
  [%expect {|
    <div class="i-am-a-div"><ul><li/><li/><li>A</li></ul></div> : Xml
    exit: 0 |}]

let%expect_test "XML-Variant-Conversion (namespaced)" =
  run_expr {|variantToXmlItem(NsNode("myns", "mytag", [ Attr("x", "y"), NsAttr("a", "b", "c"), Node("div", [ ]), NsNode("foo", "bar", [ ]) ]))|};
  [%expect {|
    <myns:mytag x="y" a:b="c"><div/><foo:bar/></myns:mytag> : XmlItem
    exit: 0 |}]

let%expect_test "Ill-formed namespace" =
  run_expr {|variantToXmlItem(NsNode("ill:formed", "div", [ ]))|};
  [%expect {|
    exit: 1
    ***: Runtime error: Illegal character in namespace |}]

let%expect_test "Ill-formed tag-name [1]" =
  run_expr {|variantToXmlItem(Node("tag:name", [ ]))|};
  [%expect {|
    exit: 1
    ***: Runtime error: Illegal character in tagname |}]

let%expect_test "Ill-formed tag-name [2]" =
  run_expr {|variantToXmlItem(NsNode("ns", "tag:name", [ ]))|};
  [%expect {|
    exit: 1
    ***: Runtime error: Illegal character in tagname |}]

let%expect_test "Appending a children (single)" =
  run_expr {|appendChild(makeXml("foo", [ ], <div/>), <span/>)|};
  [%expect {|
    <foo><div/><span/></foo> : XmlItem
    exit: 0 |}]

let%expect_test "Appending a children (multiple)" =
  run_expr {|appendChild(makeXml("foo", [ ], <div/>), <#><div><span>Foo</span></div><span/></#>)|};
  [%expect {|
    <foo><div/><div><span>Foo</span></div><span/></foo> : XmlItem
    exit: 0 |}]

let%expect_test "Prepending a children (single)" =
  run_expr {|prependChild(makeXml("foo", [ ], <div/>), <span/>)|};
  [%expect {|
    <foo><span/><div/></foo> : XmlItem
    exit: 0 |}]

let%expect_test "Prepending a children (multiple)" =
  run_expr {|prependChild(makeXml("foo", [ ], <div/>), <#><div><span>Foo</span></div><span/></#>)|};
  [%expect {|
    <foo><div><span>Foo</span></div><span/><div/></foo> : XmlItem
    exit: 0 |}]

let%expect_test "Replace children" =
  run_expr {|replaceChildren(makeXml("foo", [ ], <div/>), <span/>)|};
  [%expect {|
    <foo><span/></foo> : XmlItem
    exit: 0 |}]

let%expect_test "Setting new Attributes" =
  run_expr {|setAttribute(makeXml("foo", [ ], <#/>), "bar", "baz")|};
  [%expect {|
    <foo bar="baz"/> : XmlItem
    exit: 0 |}]

let%expect_test "Setting new Attributes NS" =
  run_expr {|setAttributeNS(makeXml("foo", [ ], <#/>), "attr", "bar", "baz")|};
  [%expect {|
    <foo attr:bar="baz"/> : XmlItem
    exit: 0 |}]

let%expect_test "Setting illformed Attributes [1]" =
  run_expr {|setAttributeNS(makeXml("foo", [ ], <#/>), "attr", "ill:formed", "baz")|};
  [%expect {|
    exit: 1
    ***: Runtime error: Attribute names cannot contain colons. |}]

let%expect_test "Setting illformed Attributes [2]" =
  run_expr {|setAttribute(makeXml("foo", [ ], <#/>), "ill:formed", "baz")|};
  [%expect {|
    exit: 1
    ***: Runtime error: Attribute names cannot contain colons. Use setAttributeNS instead. |}]

let%expect_test "Setting existing Attributes" =
  run_expr {|setAttribute(makeXml("foo", [ ("bar", "baz") ], <#/>), "bar", "oof")|};
  [%expect {|
    <foo bar="oof"/> : XmlItem
    exit: 0 |}]

let%expect_test "Getting namespace [1]" =
  run_expr {|getNamespace([ variantToXmlItem(NsNode("ns", "tagname", [ ])) ])|};
  [%expect {|
    "ns" : String
    exit: 0 |}]

let%expect_test "Getting namespace [2]" =
  run_expr {|getNamespace([ variantToXmlItem(Node("tagname", [ ])) ])|};
  [%expect {|
    "" : String
    exit: 0 |}]

let%expect_test "Get tag name [1]" =
  run_expr {|getTagName(<div />)|};
  [%expect {|
    "div" : String
    exit: 0 |}]

let%expect_test "Get tag name [2]" =
  run_expr {|getTagName([ variantToXmlItem(NsNode("ns", "tagname", [ ])) ])|};
  [%expect {|
    "tagname" : String
    exit: 0 |}]

let%expect_test "Get single attribute [1]" =
  run_expr {|attribute(<div id="myid" />, "id")|};
  [%expect {|
    Just("myid") : [|Just:String|Nothing|]
    exit: 0 |}]

let%expect_test "Get attributes" =
  run_expr {|getAttributes(<div id="myid" ns:foo="bar" />)|};
  [%expect {|
    [("ns:foo", "bar"), ("id", "myid")] : [(String, String)]
    exit: 0 |}]

let%expect_test "Top level comment" =
  run_expr {|<!-- This is an XML comment --> ()|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Body comment" =
  run_expr {|<b><!-- abc -->ab<!-- foobar -->ba</b>|};
  [%expect {|
    <b>abba</b> : Xml
    exit: 0 |}]

let%expect_test "Unterminated top level comment (1)" =
  run_expr {|<!-- this comment never ends -> ()|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character :
      <!-- this comment never ends -> ()
                                        ^ |}]

let%expect_test "Unterminated top level comment (2)" =
  run_expr {|<!-- this comment never ends > ()|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character :
      <!-- this comment never ends > ()
                                       ^ |}]

let%expect_test "Unterminated top level comment (3)" =
  run_expr {|<!-- this comment never ends ()|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character :
      <!-- this comment never ends ()
                                     ^ |}]

let%expect_test "Unterminated body comment (1)" =
  run_expr {|<b>ab<!-- this comment never ends -></b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character :
      <b>ab<!-- this comment never ends -></b>
                                              ^ |}]

let%expect_test "Unterminated body comment (2)" =
  run_expr {|<b>ab<!-- this comment never ends ></b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character :
      <b>ab<!-- this comment never ends ></b>
                                             ^ |}]

let%expect_test "Unterminated body comment (3)" =
  run_expr {|<b>ab<!-- this comment never ends -></b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character :
      <b>ab<!-- this comment never ends -></b>
                                              ^ |}]

let%expect_test "Unterminated body comment (4)" =
  run_expr {|<b>ab<!-- this comment never ends</b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character :
      <b>ab<!-- this comment never ends</b>
                                           ^ |}]

let%expect_test "Nested comments" =
  run_expr {|<b><!-- First <!-- Second --> --></b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character : --
      <b><!-- First <!-- Second --> --></b>
                        ^ |}]

let%expect_test "Double dash in comment (1)" =
  run_expr {|<b><!-- -- --></b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character : --
      <b><!-- -- --></b>
                ^ |}]

let%expect_test "Double dash in comment (2)" =
  run_expr {|<b><!-- ---></b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character : --
      <b><!-- ---></b>
                ^ |}]

let%expect_test "Triple dash in comment" =
  run_expr {|<b><!-- --- --></b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character : --
      <b><!-- --- --></b>
                ^ |}]

let%expect_test "XML comment embodied inside a string literal" =
  run_expr {|"<!-- This is an XML comment -->"|};
  [%expect {|
    "<!-- This is an XML comment -->" : String
    exit: 0 |}]

let%expect_test "Valid XML embodied inside an XML comment" =
  run_expr {|<!-- <b>Hello World</b> --> ()|};
  [%expect {|
    () : ()
    exit: 0 |}]

let%expect_test "Ill-bracketed tag (closing tag hidden in a comment)" =
  run_expr {|<b><!-- </b> -->|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1

      <b><!-- </b> -->
                      ^ |}]

let%expect_test "Ill-bracketed tag (opening tag hidden in a comment)" =
  run_expr {|<!-- <b> --></b>|};
  [%expect {|
    exit: 1
    <string>:1: Type error: Unknown variable </.
    In expression: </b. |}]

let%expect_test "Newlines in comment" =
  run_file {|./tests/xml_comment_newlines.links|};
  [%expect {|
    <b/> : Xml
    exit: 0 |}]

let%expect_test "Mismatched tags" =
  run_expr {|<a></b>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Closing tag 'b' does not match start tag 'a'.
      <a></b>
             ^ |}]

let%expect_test "Attributes with hyphens are OK" =
  run_expr {|<button class="navbar-toggler" type="button" data-toggle="collapse"></button>|};
  [%expect {|
    <button type="button" data-toggle="collapse" class="navbar-toggler"/> : Xml
    exit: 0 |}]

let%expect_test "Tags with hyphens are not OK" =
  run_expr {|<button-monstrosity type="button"></button-monstrosity>|};
  [%expect {|
    exit: 1
    ***: Parse error: <string>:1
    Unexpected character : -
      <button-monstrosity type="button"></button-monstrosity>
              ^ |}]

