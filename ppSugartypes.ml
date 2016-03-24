open Format
open Sugartypes

(* Exact floating point printing.
 * http://blog.frama-c.com/index.php?post/2011/10/29/A-portable-OCaml-function-to-print-floats
 * Changed to use Links floating point operators like this: 1.4387939942446899*.2.0^.19.0
 *)
let print_exact_float ~use_hex fmt f =
  let double_norm = Int64.shift_left 1L 52 in
  let double_mask = Int64.pred double_norm in
  let i = Int64.bits_of_float f in
  let s = 0L <> (Int64.logand Int64.min_int i) in
  let i = Int64.logand Int64.max_int i in
  let exp = Int64.to_int (Int64.shift_right_logical i 52) in
  let man = Int64.logand i double_mask in
  let s = if s then "-" else "" in
  let firstdigit, exp =
    if exp <> 0
    then 1, (exp - 1023)
    else 0, -1022
  in
  if not use_hex
  then begin
      let firstdigit, man, exp =
	if 0 <= exp && exp <= 12
	then begin
	    Int64.to_int
	      (Int64.shift_right_logical
		 (Int64.logor man double_norm)
		 (52 - exp)),
            Int64.logand (Int64.shift_left man exp) double_mask,
            0
	  end
	else firstdigit, man, exp
      in
      let d =
	Int64.float_of_bits
	  (Int64.logor 0x3ff0000000000000L man)
      in
      let d, re =
	if d >= 1.5
	then d -. 1.5, 5000000000000000L
	else d -. 1.0, 0L
      in
      let d = d *. 1e16 in
      let decdigits = Int64.add re (Int64.of_float d) in
      if exp = 0
      then
	Format.fprintf fmt "%s%d.%016Ld"
	               s
	               firstdigit
	               decdigits
      else
	Format.fprintf fmt "%s%d.%016Ld*.2.0^.%d.0"
	               s
	               firstdigit
	               decdigits
	               exp
    end
  else
    Format.fprintf fmt "%s0x%d.%013Lxp%d"
                   s
                   firstdigit
                   man
                   exp

let keyword (ppf : formatter) (k : string) : 'a =
  fprintf ppf "@{<keyword>%s@}" k

let comments : (int * string) list ref = ref []

let flush_comments ppf (s, pos) =
  let pos = (Utility.fst3 pos).Lexing.pos_cnum in
  let rec go = function
    | (col, s) :: comments when col < pos ->
       fprintf ppf "%s@\n" s;
       go comments
    | comments -> comments
  in fprintf ppf "@[";
     comments := go !comments;
     fprintf ppf "@]"

let maybe sep element_printer =
  fun ppf ->
  let printer ppf = function
    | None -> fprintf ppf ""
    | Some e -> fprintf ppf sep
                        element_printer e
  in printer ppf

let separated_list sep element_printer =
  fun ppf ->
  let rec printer ppf = function
    | [] -> fprintf ppf ""
    | [x] -> fprintf ppf "%a"
                     element_printer x
    | x :: xs -> fprintf ppf sep
                         element_printer x;
                 printer ppf xs
  in printer ppf

let comma_separated_list element_printer =
  fun ppf ->
  let rec printer ppf = function
    | [] -> fprintf ppf ""
    | [x] -> fprintf ppf "%a"
                     element_printer x
    | x :: xs -> fprintf ppf "%a,@ "
                         element_printer x;
                 printer ppf xs
  in printer ppf

(** Comma separated list with outer parens, empty string if list is empty. *)
let argument_list element_printer =
  fun ppf ->
  function
  | [] -> fprintf ppf ""
  | l -> fprintf ppf "(@[<hv>%a@])"
                 (comma_separated_list element_printer) l

let constant (ppf : formatter) : constant -> 'a = function
  | `Int i -> fprintf ppf "@{<constant>%i@}" i
  | `Bool true -> fprintf ppf "@{<constant>true@}"
  | `Bool false -> fprintf ppf "@{<constant>false@}"
  (* NOTE Char.escaped uses OCaml rules for special characters. Not sure Links' are the same. *)
  | `Char c -> fprintf ppf "@{<constant>'%s'@}" (Char.escaped c)
  (* NOTE String.escaped uses OCaml rules for special characters in strings. Not sure Links' are the same. *)
  | `String s -> fprintf ppf "@{<constant>\"%s\"@}" (String.escaped s)
  (* TODO Better printing for floats. Ideally, we would like to keep
     the input, but currently Links only gives us the value. Printing
     floating point numbers is hard. See, e.g. Guy L. Steele Jr. and
     Jon L. White. How to print floating-point numbers accurately.
     PLDI 1990 *)
  | `Float f -> fprintf ppf "@{@{<constant>%a@}"
                        (print_exact_float ~use_hex:false) f

let rec patternnode ppf : patternnode -> 'a = function
  | `Variable (n, t, p) -> fprintf ppf "%s" n
  | `Any -> fprintf ppf "_"
  | `Nil -> fprintf ppf "[]"
  | `Constant c -> constant ppf c
  | `Cons (h, t) -> fprintf ppf "%a::%a"
                            pattern h
                            pattern t
  | `Tuple l ->
     fprintf ppf "(@[%a@])"
             (comma_separated_list pattern) l
  | `List l ->
     fprintf ppf "[%a]"
             (comma_separated_list pattern) l
  | `Variant (n, p) ->
     fprintf ppf "%s%a"
             n
             (maybe "(%a)" pattern) p
  | p -> failwith ("patternode " ^ Show_patternnode.show p)

and pattern (ppf : formatter) : pattern -> 'a = function
  | e, pos ->
     fprintf ppf "%a%a"
             flush_comments ("pattern", pos) (* This pos seems to be wrong... *)
             patternnode e

let binop (ppf : formatter) : binop -> 'a = function
  | x -> fprintf ppf "%s" (string_of_binop x)

let unary_op (ppf : formatter) : unary_op -> 'a = function
  | x -> fprintf ppf "%s" (string_of_unary_op x)

let record_label (ppf : formatter) : name -> 'a = function
  (* TODO Better logic for determining when to use strings for record_labels. *)
  (* NOTE This uses OCaml rules for characters that need escaping in strings. Not sure Links' are the same. *)
  | s -> let s' = String.escaped s in
         try ignore (List.find (fun (k, _) -> s = k) Lexer.keywords);
             fprintf ppf "@{<record_label>\"%s\"@}" s'
         with _ ->
           if s = s' then
             fprintf ppf "@{<record_label>%s@}" s
           else
             fprintf ppf "@{<record_label>\"%s\"@}" s'

let rec string_fieldspec (ppf : formatter) : string * fieldspec -> 'a = fun (l, dt) ->
  match dt with
  | `Present `Unit ->
     fprintf ppf "%s"
             l
  | `Present dt ->
     fprintf ppf "%s: %a"
             l
             datatype dt

and type_arg (ppf : formatter) : type_arg -> 'a = function
  | `Type t -> datatype ppf t

and row_var (ppf : formatter) : row_var -> 'a = function
  | `Closed -> fprintf ppf ""
  | `Open (n, sk, `Rigid) -> fprintf ppf "|_%s::TODO" n
  | `Open (n, sk, `Flexible) -> fprintf ppf "|?%s::TODO" n

and row (ppf : formatter) : row -> 'a = function
  (* TODO pretty printing of row variables depends on how often they are used! We should print _ for
     a rigid variable that is only used once (similar: ? for flexible tyvar). Not sure how to do
     that without traversing the datatype beforehand and counting, which is not something I
     necessarily want to do... *)
  | fsl, `Closed ->
     fprintf ppf "{%a}"
             (comma_separated_list string_fieldspec) (List.sort (fun (a,_) (b,_) -> compare a b) fsl)
  | r -> fprintf ppf "%s"
                 (Show_row.show r)

and effect_arrow (ppf : formatter) : row -> 'a = function
  (* TODO not sure these are entirely correct *)
  | [], `Open (n, (`Unl, `Any), `Rigid) -> fprintf ppf "->"
  | [("wild", `Present `Unit)], `Closed -> fprintf ppf "{}~>"
  | [("wild", `Present `Unit)], `Open (n, (`Unl, `Any), `Rigid) -> fprintf ppf "~%s~>" n
  | r -> fprintf ppf "%a->"
                 row r

and datatype (ppf : formatter) : datatype -> 'a = function
  | `Primitive `String -> fprintf ppf "@{<primitive_type>String@}"
  | `Primitive `Int -> fprintf ppf "@{<primitive_type>Int@}"
  | `Primitive `Bool -> fprintf ppf "@{<primitive_type>Bool@}"
  | `Primitive `Float -> fprintf ppf "@{<primitive_type>Float@}"
  | `Record (fsl, rv) ->
     fprintf ppf "(@[<hv>%a%a)@]"
             row_var rv
             (comma_separated_list string_fieldspec) (List.sort (fun (a,_) (b,_) -> compare a b) fsl)
  | `Variant (fsl, `Closed) ->
     fprintf ppf
             "@[<hov 1>[|@ %a |]@]"
             (separated_list "%a@ | " string_fieldspec) (List.sort (fun (a,_) (b,_) -> compare a b) fsl)
  | `Function (args, effrow, res) ->
     fprintf ppf "%a %a@ %a"
             (argument_list datatype) args
             effect_arrow effrow
             datatype res
  | `TypeVar (n, (`Unl, `Any), `Rigid) ->
     fprintf ppf "%s" n
  | `TypeApplication (name, args) ->
     fprintf ppf "%s%a"
             name
             (argument_list type_arg) args
  | `Unit -> fprintf ppf "()"
  | `List dt -> fprintf ppf "[@[%a@]]" datatype dt
  | `Tuple ts ->
     fprintf ppf "(@[<hv>%a)@]"
             (comma_separated_list datatype) ts
  (* | _dbg -> failwith ("Cannot pprint datatype " ^ Show_datatype.show _dbg) *)
  | _dbg -> fprintf ppf "%s" (Show_datatype.show _dbg)

let fieldconstraint (ppf : formatter) : fieldconstraint -> 'a = function
  | `Readonly -> keyword ppf "readonly"
  | `Default ->  keyword ppf "default"

let fieldconstraintlist (ppf : formatter) : (name * fieldconstraint list) -> 'a = function
  | n, fcs -> fprintf ppf "%a @[%a@]"
                      record_label n
                      (separated_list "%a@ " fieldconstraint) fcs

let name_fieldconstraintlist_list (ppf : formatter) : (name * fieldconstraint list) list -> 'a = function
  | [] -> fprintf ppf ""
  | l -> fprintf ppf "@ %a @[<hv>%a@]"
                 keyword "where"
                 (comma_separated_list fieldconstraintlist) l

let rec phrasenode (ppf : formatter) : phrasenode -> 'a = function
  | `Constant c -> constant ppf c
  | `Regex _ -> fprintf ppf "REGEX TODO"
  | `Block ([], p) ->
     fprintf ppf "{@[<hov>@ %a@ @]}"
             phrase p
  | `Block (bl, p) ->
     fprintf ppf "@[{@;<1 2>@[<v>%a@]@[<hov>%a@]@]@ }"
             binding_list bl
             phrase p
  | `DatabaseLit (name, (mdriver, mparams)) ->
     fprintf ppf "@[<hov 2>%a@ %a%a%a@]"
             keyword "database"
             phrase name
             (maybe "@ %a" phrase) mdriver
             (maybe "@ %a" phrase) mparams
  | `TableLit (name, (withdt, None), nfcll, tk, db) as _dbg ->
     (* fprintf ppf "%s" (Show_phrasenode.show _dbg) *)
     fprintf ppf "@[<hv>%a %a@ %a %a%a%a@ %a %a@]"
             keyword "table"
             phrase name
             keyword "with"
             datatype withdt
             name_fieldconstraintlist_list nfcll
             tablekeys tk
             keyword "from"
             phrase db
  | `Query (limits, (`Block ([], p), _), dt) ->
     fprintf ppf "%a {@;<1 2>@[<hov>%a@]@\n}"
             keyword "query"
             phrase p
  | `Iteration (ips, b, mw, mo) ->
     fprintf ppf "@[<hov>%a (@[<hv>%a@])%a%a@\n  %a@]"
             keyword "for"
             (comma_separated_list iterpatt) ips (* iterpatt_list ips *)
             maybe_where mw
             maybe_orderby mo
             phrase b
  | `Switch (p, ps, dt) ->
     fprintf ppf "%a (%a) {@\n  @[<v>%a@]@\n}"
             keyword "switch"
             phrase p
             (separated_list "%a@ " switch_case) ps
  | `ListLit (ps, dt) -> fprintf ppf "[@[<hv>%a@]]"
                                 phrase_list ps
  (* maybe_datatype_annotation dt *)
  | `ConstructorLit (n, p, None) ->
     fprintf ppf "%s%a"
             n
             (maybe "(%a)" phrase) p
  | `RecordLit (r, None) -> fprintf ppf "(@[<hv>%a)@]"
                                    (* TODO Maybe make sorting optional? *)
                                    (* TODO Could optionally print more compact using <hov>? *)
                                    (comma_separated_list record_element) (List.sort (fun (a,_) (b,_) -> compare a b) r)
  | `TupleLit l -> fprintf ppf "(@[<hv>%a)@]"
                           (* TODO Could optionally print more compact using <hov>? *)
                           (comma_separated_list phrase) l
  | `Var n -> fprintf ppf "%s" n
  | `Projection (p, n) -> fprintf ppf "%a.%a"
                                  phrase p
                                  record_label n
  | `UnaryAppl ((tyargs, op), e) ->
     fprintf ppf "%a%a"
             unary_op op
             phrase e
  | `InfixAppl ((tyargs, op), left, right) ->
     (* TODO parenthesis, operator precedence? *)
     fprintf ppf "@[<hov 2>%a@ %a@ %a@]"
             phrase left
             binop op
             phrase right
  | `FnAppl (f, args) -> fprintf ppf "%a(@[%a@])"
                                 phrase f
                                 (comma_separated_list phrase) args

  (* TODO Deduplicate with toplevel `Fun.
          Unlike the toplevel fun, this supports printing on one line for short functions without binding lists *)
  | `FunLit (None, `Unl, (pss, (`Block ([], bod), _)), `Unknown) as _dbg ->
     fprintf ppf "@[<hv>%a @[<hv>(@[<hov>%a@])@] {@;<1 2>@[%a@]@ @]}"
             keyword "fun"
             (separated_list "%a@])@ (@[<hov>" pattern_list) pss
             phrase bod
  | `FunLit (None, `Unl, (pss, (`Block (bl, bod), _)), `Unknown) as _dbg ->
     fprintf ppf "@[<v>%a @[<hv>(@[<hov>%a@])@] {@;<1 2>@[%a%a@]@ @]}"
             keyword "fun"
             (separated_list "%a@])@ (@[<hov>" pattern_list) pss
             binding_list bl
             phrase bod

  | `Conditional (b, t, f) -> fprintf ppf "@[<hov>@[<hv>%a (%a)@;<1 2>%a@]@ @[<hv>%a@;<1 2>%a@]@]"
                                      (* TODO this probably needs some tweaking *)
                                      keyword "if"
                                      phrase b
                                      phrase t
                                      keyword "else"
                                      phrase f
  | `Section _ -> fprintf ppf "SECTION"
  (* | p -> failwith ("Can't pretty print " ^ Sugartypes.Show_phrasenode.show p) *)
  | p -> fprintf ppf "%s" (Show_phrasenode.show p) (* TODO *)

and record_element (ppf : formatter) : (name * phrase) -> 'a = fun (n, p) ->
  fprintf ppf "%a = %a"
          record_label n
          phrase p
and switch_case (ppf : formatter) : pattern * phrase -> 'a = function
  (* Phrases like the below have a nested outer block for some reason. Special case this to put
     the opening brace on the same line as the case and print the block indented, like functions
     case _ -> {
       fun abc(i) { i }
       var cde = 5;
       abc(cde)
     } *)
  | pat, (`Block ([], (`Block (bl, bod), _)), _) ->
     fprintf ppf "@[%a %a -> {@;<1 2>@[%a%a@]@ }@]"
             keyword "case"
             pattern pat
             binding_list bl
             phrase bod
  | pat, (`Block ([], p), _) ->
     fprintf ppf "@[%a %a ->@ %a@]"
             keyword "case"
             pattern pat
             phrase p
  (* Turns out, the right hand side is parsed as a block, even if there are no curly braces.
     We "fix" this.
     Example: case _ -> var a = 5; a
     We turn this into: case _ -> { var a = 5; a }
     TODO revisit this choice. *)
  | pat, (phr, pos) -> switch_case ppf (pat, (`Block ([], (phr, pos)), pos))

and phrase (ppf : formatter) : phrase -> 'a = function
  | e, pos -> fprintf ppf "%a@[%a@]"
                      flush_comments ("phrase", pos)
                      phrasenode e
and phrase_list ppf = comma_separated_list phrase ppf
and maybe_where (ppf : formatter) : phrase option -> 'a = function
  | None -> fprintf ppf ""
  | Some p -> fprintf ppf "@\n%a (%a)"
                      keyword "where"
                      phrase p
and maybe_orderby (ppf : formatter) : phrase option -> 'a = function
  | None -> fprintf ppf ""
  | Some p -> fprintf ppf "@\n%a (%a)"
                      keyword "orderby"
                      phrase p (* TODO 1-tuples here are `RecordLits for some reason. *)
and iterpatt (ppf : formatter) : iterpatt -> 'a = function
  | `List (p, ph) -> fprintf ppf "%a <- %a"
                             pattern p
                             phrase ph
  | `Table (p, ph) -> fprintf ppf "%a <-- %a"
                              pattern p
                              phrase ph
and iterpatt_list ppf = comma_separated_list iterpatt ppf

and tablekeys (ppf : formatter) : phrase -> 'a = function
  | `ListLit ([], _), _ -> fprintf ppf ""
  | keys -> fprintf ppf "@ %a %a"
                    keyword "tablekeys"
                    phrase keys

and maybesig (ppf : formatter) : datatype' option -> 'a = function
  | None -> fprintf ppf ""
  | Some s -> fprintf ppf "%a TODO@\n" keyword "sig"

and funtypesig (b : binder) (ppf : formatter) : datatype' -> 'a = function
  | dt, None ->
     fprintf ppf "%a %a: @[%a@]"
             keyword "sig"
             binder b
             datatype dt

and name (ppf : formatter) : name -> 'a = function
  | s -> fprintf ppf "%s" s

and binder (ppf : formatter) : binder -> 'a = function
  | e, t, pos -> fprintf ppf "%a%a"
                         flush_comments ("binder", pos)
                         name e

and pattern_list = comma_separated_list pattern

and binding_list ppf = function
  (* Print newline at end of list, but only if not empty *)
  | [] -> fprintf ppf ""
  | l -> fprintf ppf "%a@\n"
                 (separated_list "%a@ " binding) l

and toplevel_binding_list ppf = function
  | [] -> fprintf ppf "@\n"
  | l -> fprintf ppf "%a@\n@\n"
                 (separated_list "%a@\n@\n" binding) l

and bindingnode (ppf : formatter) : Sugartypes.bindingnode -> 'a = function
  | `Val (t, p, e, l, d) ->
     fprintf ppf "@[<2>%a@ %a@ =@ @[%a;@]@]"
             keyword "var"
             pattern p
             phrase e
  | `Fun (b, lin, (tyvar, (pss, (`Block (bl, bod), _))), loc, s) ->
     (* binding_list could be <hv> instead of <v> for more compact printing, or even <hov>? *)
     fprintf ppf "%a%a %a@[<hv>(@[<hov>%a@])@] {@\n  @[<v>%a@]%a@\n}"
             (maybe "%a@\n" (funtypesig b)) s
             keyword "fun"
             binder b
             (* This is massive hackery to support multiple argument lists, like
               `fun validate(s, t, u, ff) (env) {...` *)
             (separated_list "%a@])@ (@[<hov>" pattern_list) pss
             binding_list bl
             phrase bod
  | `Exp p -> fprintf ppf "%a;"
                      phrase p
  | `Type (n, q_mtyvar_s, dt') ->
     fprintf ppf "@[<2>%a %s%a =@ @[%a;@]@]"
             keyword "typename"
             n
             quantifier_mtyvar_list q_mtyvar_s
             datatype' dt'
  (* | bn -> failwith ("bindingnode "^Show_bindingnode.show bn) *)
  | bn -> fprintf ppf "%s" (Show_bindingnode.show bn) (* TODO *)

and binding (ppf : formatter) : binding -> 'a = function
  | e, pos ->
     fprintf ppf "%a@[%a@]"
             flush_comments ("binding", pos)
             bindingnode e

and quantifier_mtyvar_list (ppf : formatter) : (quantifier * tyvar option) list -> 'a = function
  | [] -> fprintf ppf ""
  | l -> fprintf ppf "(%a)"
                 (comma_separated_list quantifier_mtyvar) l

and quantifier_mtyvar (ppf : formatter) : quantifier * tyvar option -> 'a = function
  | q, None -> quantifier ppf q

and quantifier (ppf : formatter) : quantifier -> 'a = function
  | q -> type_variable ppf q

and type_variable (ppf : formatter) : type_variable -> 'a = function
  | (n, k, f) -> fprintf ppf "%s"
                         n
and datatype' (ppf : formatter) : datatype' -> 'a = function
  | dt, None -> datatype ppf dt
