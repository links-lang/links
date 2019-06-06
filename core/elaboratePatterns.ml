(* open Utility *)

type state = int
let empty = 0
let in_handler_mask = 0b1
let toplevel_mask = 0b10
let allow_effect_patterns_mask = in_handler_mask lor toplevel_mask
let in_handler st = st land in_handler_mask == in_handler_mask
let toplevel st = st land toplevel_mask == toplevel_mask
let allow_effect_patterns st = st land allow_effect_patterns_mask == allow_effect_patterns_mask
let apply mask st = st lor mask
let remove mask st =
  if st land mask <> 0 then st lxor mask
  else st


let elaborate =
  let open Sugartypes in
  object(self : 'self_type)
    inherit SugarTraversals.map as super

    val mutable state = empty

    method backup = state
    method restore state' = state <- state'

    method allow_effect_patterns =
      allow_effect_patterns state

    method enter_handler =
      state <- apply in_handler_mask state

    method leave_handler =
      state <- remove in_handler_mask state

    method below_toplevel =
      state <- remove toplevel_mask state

    method at_toplevel =
      state <- apply toplevel_mask state

    method! phrasenode = function
      | Handle { expressions; cases; descriptor } ->
         let st' = self#backup in
         let expressions =
           self#list
             (fun o expression ->
               let expression = o#phrase expression in
               o#restore st';
               expression)
             expressions
         in
         let params =
           self#option (fun o -> o#handle_params) descriptor.shd_params
         in
         self#restore st';
         self#enter_handler;
         self#at_toplevel;
         let st'' = self#backup in
         let cases =
           self#list
             (fun o { patterns; resumption; body } ->
               let patterns =
                 o#list
                   (fun o -> o#restore st''; o#pattern)
                   patterns
               in
               let resumption =
                 o#option
                   (fun o (p, dt) ->
                     o#restore st'';
                     (o#pattern p, dt))
                   resumption
               in
               self#restore st'';
               let body = o#phrase body in
               { patterns; resumption; body })
             cases
         in
         self#restore st';
         Handle { expressions; cases; descriptor = { descriptor with shd_params = params } }
      | p -> super#phrasenode p

    method! pattern pat =
      let open SourceCode.WithPos in
      let open Pattern in
      match pat.node with
      | Any -> pat
      | Nil -> pat
      | Constant _ -> pat
      | Negative _ -> pat
      | Operation ({ parameters; resumption; _ } as d) ->
         if self#allow_effect_patterns then
           let () = self#below_toplevel in
           make
             ~pos:pat.pos
             (Operation { d with parameters = self#list (fun o -> o#pattern) parameters;
                                 resumption = self#option (fun o (p, dt) -> o#pattern p, dt) resumption })
         else raise (Errors.effect_pattern_below_toplevel pat.pos)
      | pat' ->
         self#below_toplevel;
         make ~pos:pat.pos (self#patternnode pat')
  end

let program : Sugartypes.program -> Sugartypes.program
  = fun program ->
  elaborate#program program

let sentence : Sugartypes.sentence -> Sugartypes.sentence
  = fun sentence ->
  elaborate#sentence sentence
