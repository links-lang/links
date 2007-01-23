
module type Show =
  sig
    type a
    val format : Format.formatter -> a -> unit
    val formatList : Format.formatter -> a list -> unit
    val show : a -> string
    val showList : a list -> string
  end

module ShowDefaults (S : 
  sig
    type a
    val format : Format.formatter -> a -> unit 
  end)
  : Show with type a = S.a
module Show_unprintable (S : sig type a end)
  : Show with type a = S.a
module Show_list (S : Show)
  : Show with type a = S.a list
module Show_ref (S : Show)
  : Show with type a = S.a ref
module Show_option (S : Show)
  : Show with type a = S.a option
module Show_array (S : Show)
  : Show with type a = S.a array
module Show_0
  : Show with type a = unit
module Show_1 (S1 : Show)
  : Show with type a = S1.a
module Show_2 (S1 : Show) (S2 : Show)
  : Show with type a = S1.a * S2.a
module Show_3 (S1 : Show) (S2 : Show) (S3 : Show)
  : Show with type a = S1.a * S2.a * S3.a
module Show_4 (S1 : Show) (S2 : Show) (S3 : Show) (S4 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a
module Show_5 (S1 : Show) (S2 : Show) (S3 : Show) (S4 : Show) (S5 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a
module Show_6 (S1 : Show) (S2 : Show) (S3 : Show) (S4 : Show) (S5 : Show) (S6 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a
module Show_7 (S1 : Show) (S2 : Show) (S3 : Show) (S4 : Show) (S5 : Show) (S6 : Show) (S7 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a
module Show_8 (S1 : Show) (S2 : Show) (S3 : Show) (S4 : Show) (S5 : Show) (S6 : Show) (S7 : Show) (S8 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a
module Show_9 (S1 : Show) (S2 : Show) (S3 : Show) (S4 : Show) (S5 : Show) (S6 : Show) (S7 : Show) (S8 : Show) (S9 : Show)
  : Show with type a = S1.a * S2.a * S3.a * S4.a * S5.a * S6.a * S7.a * S8.a * S9.a
