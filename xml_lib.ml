open Cduce_lib

let env = ref Typer.empty_env

let expr_of_string str =
  let str' = Ident.U.mk str in
  Ast.String (Ident.U.start_index str', Ident.U.end_index str', str', Ast.cst_nil)

let expr_of_int value =
  Ast.Integer (Intervals.V.from_int value)

module Definition = struct
  type t = Cduce_lib.Ast.ppat

  type regexp = Ast.regexp

  let of_internal internal = Location.mknoloc (Ast.Internal internal)

  let of_string string =
    let length = String.length string in
    let rec make_regexp index regexp =
      if index < length then
        let char = string.[index] in
        let char_type = Types.char (Chars.atom (Chars.V.mk_char char)) in
        let char_exp = Ast.Elem (of_internal char_type) in
        let regexp = Ast.Seq (regexp, char_exp) in
        make_regexp (succ index) regexp
      else regexp in
    make_regexp 0 Ast.Epsilon

  let epsilon = Ast.Epsilon

  let latin1 = Ast.Elem (of_internal (Sequence.char_latin1))

  let concat a b = Ast.Seq (a, b)

  let union a b = Ast.Alt (a, b)

  let star t = Ast.Star t

  let question t = Ast.Alt (Ast.Epsilon, t)

  let plus t = Ast.Seq (t, Ast.Star t)

  let of_name name = Location.mknoloc (Ast.PatVar [Ident.U.mk name])

  let of_regexp t = Location.mknoloc (Ast.Regexp t)

  let any = of_internal Types.any

  let any_char = of_internal (Types.char Chars.any)

  let string = of_regexp (star (Ast.Elem any_char))

  let element name attr_list attr_open contents =
    let to_attr_spec (name, optional) =
      let attr_type =
        if optional then Location.mknoloc (Ast.Optional string)
        else string in  
      Ident.U.mk name, (attr_type, None) in
    let attr_spec_list = List.map to_attr_spec attr_list in
    let attr_record_noloc = Ast.Record (attr_open, attr_spec_list) in
    let attr_record = Location.mknoloc attr_record_noloc in
    let name_pat = Location.mknoloc (Ast.Cst (Ast.Atom (Ident.U.mk name))) in
    let prod_pat = Location.mknoloc (Ast.Prod (attr_record, contents)) in
    Ast.Elem (Location.mknoloc (Ast.XmlT (name_pat, prod_pat)))

  let to_pat t = Location.mknoloc (Ast.Regexp t)

  let define list =
    let to_type_def (ident, t) =
      Location.noloc, Ident.U.mk ident, t in
    let type_def_list = List.map to_type_def list in
    env := Typer.type_defs !env type_def_list
end

module Type = struct
  type t = Types.t

  let any = Types.any

  let empty = Types.empty

  let epsilon = Sequence.nil_type

  let string = Sequence.string_latin1

  let of_definition name =
    let pat = Definition.of_name name in
    Types.descr (Typer.typ !env pat)

  let of_regexp regexp =
    let pat = Definition.of_regexp regexp in
    Types.descr (Typer.typ !env pat)

  let of_string string =
    let rec make_type index result =
      if index >= 0 then
        let char = string.[index] in
        let char_type = Types.char (Chars.atom (Chars.V.mk_char char)) in
        let result = Types.times (Types.cons char_type) (Types.cons result) in
        make_type (pred index) result
      else result in
    make_type (String.length string - 1) Sequence.nil_type

  let concat t1 t2 = Sequence.concat t1 t2

  let union t1 t2 = Types.cup t1 t2

  let element name attr_list attr_open contents =
    let add_attr_spec attr_map (name, optional) =
      let attr_type =
        if optional then Types.cons (Sequence.option (Types.cons string))
        else Types.cons string in
      Ident.LabelMap.add (Ns.Label.mk_ascii name) attr_type attr_map in
    let attr_map =
      List.fold_left add_attr_spec Ident.LabelMap.empty attr_list in
    let attr_type = Types.record_fields (attr_open, attr_map) in
    let pair_type = Types.times (Types.cons attr_type) (Types.cons contents) in
    let name_atom = Atoms.atom (Atoms.V.mk_ascii name) in
    let name_type = Types.atom name_atom in
    let xml = Types.xml (Types.cons name_type) (Types.cons pair_type) in
    Sequence.seq_of_list [xml]

  let fprintf ppf t =
    Format.fprintf ppf "XML:";
    Types.Print.print ppf t

  let to_buffer buf t =
    let ppf = Format.formatter_of_buffer buf in
    fprintf ppf t;
    Format.pp_print_flush ppf ()

  let to_string t =
    let buf = Buffer.create 8 in
    to_buffer buf t;
    Buffer.contents buf

  let subtype t1 t2 = Types.subtype t1 t2
end
