module String = struct
  include String

  let pp = Format.pp_print_string

  let show s = s
end

include String

module Set = struct
  include Lens_set.Make (String)
end

module Map = struct
  include Lens_map.Make (String)
end
