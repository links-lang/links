(* Alias variable names must be distinct from parameter names *)
type 'a x = [`Foo] as 'a
    deriving (Eq)
