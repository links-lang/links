let resolve_positions source =
object
  inherit SugarTraversals.map

  method !position =
    function
      | (start, finish, None) ->
          (start, finish, Some source)
      | _ -> assert false
end
