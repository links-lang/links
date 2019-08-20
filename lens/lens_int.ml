module Int = struct
  include Int

  let pp = Format.pp_print_int

  let show s = string_of_int s
end

include Int

module Set = struct
  include Lens_set.Make (Int)
end

module Map = struct
  include Lens_map.Make (Int)
end
