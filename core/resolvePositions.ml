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
