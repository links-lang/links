
let rec count n =
  if n == 0 then n
  else count (n - 1)

let b n =
  let x = count n in
  x

let _ = print_endline (string_of_int (b 10000000))
