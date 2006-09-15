(*
let state = ref []
let complete = ref false

let close () =
  complete := true

let register name r =
  if !complete then failwith "State.register: state already closed";
  state := (name,Obj.magic r) :: !state 

let ref name v =
  let r = ref v in
  register name r;
  r

let get () =
  if not !complete then failwith "State.get: need to close the state";
  Obj.magic (List.map (fun (name,r) -> (name, !r)) !state)

let set s =
  if not !complete then failwith "State.set: need to close the state";
  let rec aux = function
    | [],[] -> ()
    | (n1,v)::l1, (n2,r)::l2 when n1 = n2 -> r := v; aux (l1,l2)
    | _ -> failwith "State.set_state: failed"
  in
  aux (Obj.magic s,!state)
*)
