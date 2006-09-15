let symbols = ref []

let define s =
  let i = 
    try String.index s '=' 
    with Not_found -> failwith ("Invalid symbol definition :" ^ s) in
  symbols := 
  (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)) ::
    !symbols

EXTEND
  GLOBAL: Pcaml.str_item;

  Pcaml.str_item: FIRST
    [ [ "ifdef"; c = UIDENT; "then"; e1 = SELF;
        "else"; e2 = SELF ->
          if List.mem_assoc c !symbols then e1 else e2
      | "ifdef"; c = UIDENT; "then"; e1 = SELF ->
          if List.mem_assoc c !symbols then e1 else <:str_item< declare end >>
      | "ifndef"; c = UIDENT; "then"; e1 = SELF;
        "else"; e2 = SELF ->
          if List.mem_assoc c !symbols then e2 else e1
      | "ifndef"; c = UIDENT; "then"; e1 = SELF ->
          if List.mem_assoc c !symbols then <:str_item< declare end >> else e1
      ] ];
END

let expr _ s =
  try List.assoc s !symbols
  with Not_found -> failwith ("No definition for symbol " ^ s)

let _ =
  Quotation.add "symbol" (Quotation.ExStr expr);
  Pcaml.add_option "-symbol" (Arg.String define) 
    "<symbol=value> Define a symbol"
