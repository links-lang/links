open Ir
open Utility

let find_primitive_binding s = Env.String.lookup Lib.nenv s

let rec value_to_ir : Value.t -> Ir.computation =
  let rec convert : Value.t -> Ir.binding list * Ir.value = function
  | `Bool b -> [], (`Constant (`Bool b))
  | `Char c -> [], (`Constant (`Char c))
  | `Float f -> [], (`Constant  (`Float f))
  | `Int i -> [], (`Constant (`Int i))
  | `XML (Value.Node (s, xs)) ->
     let rec build (name, attrs, bodies) = function
       | Value.Text s -> (name, attrs, bodies @ [`Constant (`String s)])
       | Value.Attr (k, v) -> (name, StringMap.add k (`Constant (`String v)) attrs, bodies)
       | Value.Node (s, xs) -> (name, attrs, bodies @ [xml_to_ir s xs])
     and xml_to_ir s xs = `XmlNode (List.fold_left build (s, StringMap.empty, []) xs) in
     [], xml_to_ir s xs
  | `XML (Value.Text s) -> [], `Constant (`String s)
  | `XML _ -> assert false
  | `String s -> [], `Constant (`String s)
  | `List vs ->
     let bss, vs' = List.split (List.map convert vs) in
     List.concat bss,
     List.fold_right (fun v lst -> `ApplyPure (`Variable (find_primitive_binding "Cons"), [v; lst]))
                     vs' (`ApplyPure (`Variable (find_primitive_binding "Nil"), []))
  | `Record fs ->
     let (bss, fs') = List.split (List.map (fun (k, v) -> let (vs, v) = convert v in vs, (k, v)) fs) in
     List.concat bss, `Extend (StringMap.from_alist fs', None)
  | `Variant (tag, v) ->
       let bs, v' = convert v in
       [], `Inject (tag, v', `Not_typed)
  | `FunctionPtr (v, env) -> assert false
  | `PrimitiveFunction (name, None) -> assert false
  | `PrimitiveFunction (_, Some v) -> [], `Variable v
  | `ClientFunction name -> assert false
  | `Continuation c -> assert false
  | `Socket p -> assert false in
  fun v -> let bs, v' = convert v in bs, `Return v'
