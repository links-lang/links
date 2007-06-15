(*pp deriving *)

let _ = 
  Eq.eq<bool> true false


let _ = 
  Show.show<(bool * string) list option> 
    (Some ([true, "yes";
            false, "no"]))
