  type t =
    [ `Ident of string     (* identifier name x *)
    | `Dot of string * t ] (* access to a module component A.B.C.x *)
    [@@deriving show]


  (* A.B.C.D == (A.(B.(C.D))) *)
  let rec of_path = function
    | []  -> assert false
    | [x] -> `Ident x
    | x :: xs -> `Dot (x, of_path xs)

  let of_name x = `Ident x

  let rec unqualify = function
    | `Ident name -> name
    | `Dot (_, path) -> unqualify path

  let rec split = function
    | `Ident x -> [x]
    | `Dot (x, q) -> x :: split q

  let canonical_name q =
    String.concat "\\" (split q)

  let is_qualified = function
    | `Ident _ -> false
    | `Dot _ -> true

  let rec prefix n qname =
    match n, qname with
      | (0, _) -> failwith "Illegal prefix length"
      | _, `Ident _ -> qname
      | 1, `Dot (s, _) -> `Ident s
      | _, `Dot (s, remainder) -> `Dot (s, prefix (n-1) remainder)


  let tail = function
    | `Ident _ -> failwith "Calling tail on Ident qualified name"
    | `Dot (_, rem) -> rem

  let head = function
    | `Ident name -> name
    | `Dot (name, _) -> name

  let rec append prefix_qname qname = match prefix_qname with
    | `Ident s -> `Dot (s, qname)
    | `Dot (s, qname') -> `Dot (s, append qname' qname)
