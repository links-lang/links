module Internal = struct
  exception E of string

  let raise pp v =
    let s =
      Format.asprintf
        "Unhandled internal exception in relational lenses module:\n\n%a" pp v
    in
    raise (E s)
end
