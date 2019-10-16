open SourceCode

let resolve_positions source =
object
  inherit SugarTraversals.map

  method !position =
    Position.map_code ~f:(
      function
      | None -> Some source
      | _ -> assert false
    )
end

module Untyped = struct
  open Transform.Untyped

  let name = "resolve_positions"

  let program state program =
    let pos_context = Context.source_code (context state) in
    let program' = (resolve_positions pos_context)#program program in
    return state program'

  let sentence state sentence =
    let pos_context = Context.source_code (context state) in
    let sentence' = (resolve_positions pos_context)#sentence sentence in
    return state sentence'
end
