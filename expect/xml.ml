open Test_common
open Expect_test_common.File.Location


let%expect_test "Braced XML" =
  run_expr {|<f>{for (i <- []) <br/>}</f>|}

let%expect_test "Escaped braces" =
  run_expr {|<p>A left: {{ and a right: }}</p>|}

let%expect_test "Escaped braces (not doubled)" =
  run_expr {|<p>A left: {{ and a right: }</p>|}

let%expect_test "Backslashes" =
  run_expr {|<p>A backslash \ </p>|}

let%expect_test "Top-level-bound XML object" =
  run_expr {|fun main() { var x = for (i <- []) <br/>; <f>{x}</f> } main()|}

let%expect_test "Let-bound XML object" =
  run_expr {|{var x = for (i <- []) <br/>; <f>{x}</f>}|}

let%expect_test "Whitespace preservation" =
  run_expr {|<a b="c"> <d/> </a>|}

let%expect_test "Element splicing [1]" =
  run_expr {|{var x = "three"; <a b="c">{stringToXml(x)}</a>}|}

let%expect_test "Element splicing [2]" =
  run_expr {|{var x = "hre"; <a b="c">t{stringToXml(x)}e</a>}|}

let%expect_test "Attribute splicing [1]" =
  run_expr {|{var x = "three"; <a b="{x}"><c/></a>}|}

let%expect_test "Attribute splicing [2]" =
  run_expr {|{var x = "three"; <a b="a{x}b"><c/></a>}|}

let%expect_test "Rejection of incorrectly nested elements" =
  run_expr {|<a><b></a></b>|}

let%expect_test "Rejection of incorrectly typed attributes" =
  run_expr {|{var x = 3; <a b="{x}"><c/></a>}|}

let%expect_test "Rejection of incorrectly typed l:attributes" =
  run_expr {|{var x = 3; <a l:href="{x}"><c/></a>}|}

let%expect_test "Reject nonsense l:name attributes" =
  run_expr {|<form><input l:name="{1+1}" /></form>|}

let%expect_test "Accept okay l:name attributes" =
  run_expr {|<form l:action="{page <html><body>{stringToXml(foo)}</body></html>}"><input l:name="foo"/></form>|}

let%expect_test "Looping in XML quasis" =
  run_expr {|var things = [1, 2]; <ul>{for (x <- things) <br/>}</ul>|}

let%expect_test "Amp-encoding (OK)" =
  run_expr {|<xml>&lt;</xml>|}

let%expect_test "Amp-encoding (ill-formed XML)" =
  run_expr {|<xml>this & that</xml>|}

let%expect_test "Amp-encoding (converting from string)" =
  run_expr {|var x = "this & that"; <xml>{stringToXml(x)}</xml>|}

let%expect_test "Looping in XML quasis, with multiple content elements." =
  run_expr {|var things = [1, 2];<ul>{for (x <- things) <#><li>{stringToXml(intToString(x))}</li><li>1</li></#>}</ul>|}

let%expect_test "XML forest" =
  run_expr {|<#>foo<a/><b>bar</b>fubar</#>|}

let%expect_test "Labelization bug" =
  run_expr {|(escape cc in <form l:action="{exit (cc)}" />) ++ <p/>|}

let%expect_test "XML Construction" =
  run_expr {|<#>{[makeXml("foo", [ ("id", "bar"), ("class", "baz") ], <div/>)]}</#>|}

let%expect_test "XML Construction 2" =
  run_expr {|[ makeXml("foo", [ ("id", "bar"), ("class", "baz") ], <div/>) ]: Xml|}

let%expect_test "XML Construction 3" =
  run_expr {|makeXml("foo", [ ("id", "bar"), ("class", "baz") ], <div/>)|}

let%expect_test "Unpretty-printing attributes of XmlItem" =
  run_expr ~args:["--set=print_pretty=false"] {|<foo id="bar" class="baz"><div/></foo>|}

let%expect_test "Unpretty-printing long snippets (#256)" =
  run_expr ~args:["--set=print_pretty=false"] {|<a><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/><b/></a>|}

let%expect_test "XML-Variant-Conversion" =
  run_expr {|variantToXml(xmlToVariant(<div class="i-am-a-div"><ul><li/><li></li><li>A</li></ul></div>))|}

let%expect_test "XML-Variant-Conversion (namespaced)" =
  run_expr {|variantToXmlItem(NsNode("myns", "mytag", [ Attr("x", "y"), NsAttr("a", "b", "c"), Node("div", [ ]), NsNode("foo", "bar", [ ]) ]))|}

let%expect_test "Ill-formed namespace" =
  run_expr {|variantToXmlItem(NsNode("ill:formed", "div", [ ]))|}

let%expect_test "Ill-formed tag-name [1]" =
  run_expr {|variantToXmlItem(Node("tag:name", [ ]))|}

let%expect_test "Ill-formed tag-name [2]" =
  run_expr {|variantToXmlItem(NsNode("ns", "tag:name", [ ]))|}

let%expect_test "Appending a children (single)" =
  run_expr {|appendChild(makeXml("foo", [ ], <div/>), <span/>)|}

let%expect_test "Appending a children (multiple)" =
  run_expr {|appendChild(makeXml("foo", [ ], <div/>), <#><div><span>Foo</span></div><span/></#>)|}

let%expect_test "Prepending a children (single)" =
  run_expr {|prependChild(makeXml("foo", [ ], <div/>), <span/>)|}

let%expect_test "Prepending a children (multiple)" =
  run_expr {|prependChild(makeXml("foo", [ ], <div/>), <#><div><span>Foo</span></div><span/></#>)|}

let%expect_test "Replace children" =
  run_expr {|replaceChildren(makeXml("foo", [ ], <div/>), <span/>)|}

let%expect_test "Setting new Attributes" =
  run_expr {|setAttribute(makeXml("foo", [ ], <#/>), "bar", "baz")|}

let%expect_test "Setting new Attributes NS" =
  run_expr {|setAttributeNS(makeXml("foo", [ ], <#/>), "attr", "bar", "baz")|}

let%expect_test "Setting illformed Attributes [1]" =
  run_expr {|setAttributeNS(makeXml("foo", [ ], <#/>), "attr", "ill:formed", "baz")|}

let%expect_test "Setting illformed Attributes [2]" =
  run_expr {|setAttribute(makeXml("foo", [ ], <#/>), "ill:formed", "baz")|}

let%expect_test "Setting existing Attributes" =
  run_expr {|setAttribute(makeXml("foo", [ ("bar", "baz") ], <#/>), "bar", "oof")|}

let%expect_test "Getting namespace [1]" =
  run_expr {|getNamespace([ variantToXmlItem(NsNode("ns", "tagname", [ ])) ])|}

let%expect_test "Getting namespace [2]" =
  run_expr {|getNamespace([ variantToXmlItem(Node("tagname", [ ])) ])|}

let%expect_test "Get tag name [1]" =
  run_expr {|getTagName(<div />)|}

let%expect_test "Get tag name [2]" =
  run_expr {|getTagName([ variantToXmlItem(NsNode("ns", "tagname", [ ])) ])|}

let%expect_test "Get single attribute [1]" =
  run_expr {|attribute(<div id="myid" />, "id")|}

let%expect_test "Get attributes" =
  run_expr {|getAttributes(<div id="myid" ns:foo="bar" />)|}

let%expect_test "Top level comment" =
  run_expr {|<!-- This is an XML comment --> ()|}

let%expect_test "Body comment" =
  run_expr {|<b><!-- abc -->ab<!-- foobar -->ba</b>|}

let%expect_test "Unterminated top level comment (1)" =
  run_expr {|<!-- this comment never ends -> ()|}

let%expect_test "Unterminated top level comment (2)" =
  run_expr {|<!-- this comment never ends > ()|}

let%expect_test "Unterminated top level comment (3)" =
  run_expr {|<!-- this comment never ends ()|}

let%expect_test "Unterminated body comment (1)" =
  run_expr {|<b>ab<!-- this comment never ends -></b>|}

let%expect_test "Unterminated body comment (2)" =
  run_expr {|<b>ab<!-- this comment never ends ></b>|}

let%expect_test "Unterminated body comment (3)" =
  run_expr {|<b>ab<!-- this comment never ends -></b>|}

let%expect_test "Unterminated body comment (4)" =
  run_expr {|<b>ab<!-- this comment never ends</b>|}

let%expect_test "Nested comments" =
  run_expr {|<b><!-- First <!-- Second --> --></b>|}

let%expect_test "Double dash in comment (1)" =
  run_expr {|<b><!-- -- --></b>|}

let%expect_test "Double dash in comment (2)" =
  run_expr {|<b><!-- ---></b>|}

let%expect_test "Triple dash in comment" =
  run_expr {|<b><!-- --- --></b>|}

let%expect_test "XML comment embodied inside a string literal" =
  run_expr {|"<!-- This is an XML comment -->"|}

let%expect_test "Valid XML embodied inside an XML comment" =
  run_expr {|<!-- <b>Hello World</b> --> ()|}

let%expect_test "Ill-bracketed tag (closing tag hidden in a comment)" =
  run_expr {|<b><!-- </b> -->|}

let%expect_test "Ill-bracketed tag (opening tag hidden in a comment)" =
  run_expr {|<!-- <b> --></b>|}

let%expect_test "Newlines in comment" =
  run_file {|./tests/xml_comment_newlines.links|}

let%expect_test "Mismatched tags" =
  run_expr {|<a></b>|}

let%expect_test "Attributes with hyphens are OK" =
  run_expr {|<button class="navbar-toggler" type="button" data-toggle="collapse"></button>|}

let%expect_test "Tags with hyphens are not OK" =
  run_expr {|<button-monstrosity type="button"></button-monstrosity>|}

