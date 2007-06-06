type 'a m = 'a Shelvehelper.m
type 'a n = 'a Shelvehelper.Input.m
type id = Shelvehelper.id

module type Shelve =
sig
  type a
  module Typeable : Typeable.Typeable with type a = a
  module Eq : Eq.Eq with type a = a
  val shelve : a -> id m
  val unshelve : id -> a n
end

module Shelve_unit  : Shelve with type a = unit
module Shelve_bool  : Shelve with type a = bool
module Shelve_int   : Shelve with type a = int
module Shelve_char  : Shelve with type a = char
module Shelve_float : Shelve with type a = float
module Shelve_num   : Shelve with type a = Num.num
module Shelve_string : Shelve with type a = string
module Shelve_option (V0 : Shelve) : Shelve with type a = V0.a option
module Shelve_list (V0 : Shelve)  : Shelve with type a = V0.a list
module Shelve_1 (S1 : Shelve) : Shelve with type a = S1.a
module Shelve_2 (S1 : Shelve) (S2 : Shelve) 
  : Shelve with type a = S1.a * S2.a
module Shelve_3 (S1 : Shelve) (S2 : Shelve) (S3 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a
module Shelve_4 (S1 : Shelve) (S2 : Shelve) (S3 : Shelve) (S4 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a
module Shelve_5 (S1 : Shelve) (S2 : Shelve) (S3 : Shelve) (S4 : Shelve) (S5 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a
module Shelve_6 (S1 : Shelve) (S2 : Shelve) (S3 : Shelve) (S4 : Shelve) (S5 : Shelve) 
  (S6 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a
module Shelve_7 (S1 : Shelve) (S2 : Shelve) (S3 : Shelve) (S4 : Shelve) (S5 : Shelve)
  (S6 : Shelve) (S7 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a
module Shelve_8 (S1 : Shelve) (S2 : Shelve) (S3 : Shelve) (S4 : Shelve) (S5 : Shelve) 
  (S6 : Shelve) (S7 : Shelve) (S8 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a
module Shelve_9 (S1 : Shelve) (S2 : Shelve) (S3 : Shelve) (S4 : Shelve) (S5 : Shelve)
  (S6 : Shelve) (S7 : Shelve) (S8 : Shelve) (S9 : Shelve)
  : Shelve with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a * S9.a
