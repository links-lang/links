module InContext (C : Base.Context) =
struct
  open C
  open Base
  open Util
  open Types
  open Camlp4.PreCast
  include Base.InContext(C)

  let classname = "Pickle"

  let rec expr t =
  object (self)
    inherit make_module_expr ~classname
    method variant = variant
    method record = record
    method sum = sum
  end # expr t
    
  and polycase : Types.tagspec * int -> Ast.match_case * Ast.match_case = 
    function
      | Tag (name, args), n -> 
          let tag_patt, pickle_args,unpickle_args = match args with 
            | None   -> <:patt< `$name$ >>,
                        <:expr< >>,
                        <:expr< `$name$ >>
            | Some e -> <:patt< `$name$ x >>,
                        <:expr< let module M = $expr e$ 
                                 in M.pickle buffer x >>,
                        <:expr< let module M = $expr e$ in 
                                let x = M.unpickle stream in
                               `$name$ x >> in
            <:match_case< $tag_patt$ -> 
              Pickle_int.pickle buffer $`int:n$;
              $pickle_args$ >>,
            <:match_case< $`int:n$ -> $unpickle_args$ >>
      | Extends t, n -> 
          let patt, guard, cast = cast_pattern t in
            <:match_case< $patt$ when $guard$ -> 
                          let module M = $expr t$ in 
                            Pickle_int.pickle buffer $`int:n$;
                            M.pickle buffer $cast$ >>,
            <:match_case< $`int:n$ -> let module M = $expr t$ 
                                       in (M.unpickle stream :> a) >>

  and case : Types.summand * int -> Ast.match_case * Ast.match_case = fun ((name,args),n) ->
    let patt, exp = tuple (List.length args) in (* Does this work correctly for zero-arg constructors? *)
    <:match_case< $uid:name$ $patt$ -> 
                  Pickle_int.pickle buffer $`int:n$;
                  let module M = $expr (Tuple args)$ 
                   in M.pickle $exp$ >>,
    <:match_case< $`int:n$ -> 
                  let module M = $expr (Tuple args)$ 
                   in $uid:name$ (M.unpickle stream) >>

  and field : Types.field -> Ast.expr * Ast.expr = function
    | (name, ([], t), _) -> 
        <:expr< let module M = $expr t$ in M.pickle buffer $lid:name$ >>,
        <:expr< let module M = $expr t$ in M.unpickle stream >>
    | f -> raise (Underivable (classname, context.atype)) (* Can't handle "higher-rank" types *)

  and sum summands = 
    let picklers, unpicklers = 
      List.split (List.map2 (F.uncurry case) 
                    summands
                    (List.range 0 (List.length summands))) in
      <:module_expr< struct
        let pickle buffer = function $list:picklers$
        let unpickle stream = function $list:unpicklers$
      end >>

  and record fields = 
    let picklers, unpicklers = 
      List.split (List.map field fields) in
    let unpickle = 
      List.fold_right2
        (fun (field,_,_) unpickler e -> 
           <:expr< let $lid:field$ = $unpickler$ in $e$ >>)
        fields
        unpicklers
        (record_expression fields) in
      <:module_expr< struct
        let pickle buffer $record_pattern fields$ = $List.fold_left1 seq picklers$
        let unpickle stream = $List.fold_left1 seq unpicklers$
      end >>

  and variant (_, tags) = 
    let picklers, unpicklers = 
      List.split (List.map2 (F.uncurry polycase) 
                    tags
                    (List.range 0 (List.length tags))) in
      <:module_expr< struct
        let pickle buffer = function $list:picklers$
        let unpickle stream = function $list:unpicklers$
      end >>
end

let generate context csts = 
  let module M = InContext(struct let context = context end) in
    M.generate ~csts ~make_module_expr:M.expr
      ~classname:M.classname ~default_module:(Some "Pickle_defaults")
