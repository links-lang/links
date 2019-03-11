module String = Lens_string
module Option = Lens_option
module Format = Lens_format
module List = Lens_list
module Seq = Lens_seq
module Set = Lens_set
module Result = Lens_result

module O = struct
  let ( << ) f g x = f (g x)

  let ( >> ) f g x = g (f x)
end

include Common
