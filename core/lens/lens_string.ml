open Utility

include String

module Set = struct
  include Lens_set.Make (String)
end

module Map = struct
  include Utility.StringMap
end
