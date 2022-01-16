open Links_unit_lens_utility

open Lens.Utility

let avg_ints a b = float (a + b) /. 2.0

let skip_median l =
  let sorted = List.drop ~n:5 l |> List.sort Int.compare in
  let n = List.length sorted in
  if n mod 2 == 0 then
    avg_ints (List.nth sorted (n / 2)) (List.nth sorted ((n / 2) - 1))
  else List.nth sorted (n / 2) |> float

let csv_name str =
  let dir = Filename.dirname Sys.argv.(0) in
  Format.sprintf "%s/%s.csv" dir str

let tex_name str =
  let dir = Filename.dirname Sys.argv.(0) in
  Format.sprintf "%s/%s.tex" dir str

let print_csv_4 ~file data =
  let channel = open_out (csv_name file) in
  Printf.fprintf channel "n, iqtime, ittime, cqtime, cttime\n";
  List.iter
    ~f:(fun (a, b, c, d, e) ->
      Printf.fprintf channel "%i, %f, %f, %f, %f\n" a b c d e)
    data;
  close_out channel

let print_csv_2 ~file data =
  let channel = open_out (csv_name file) in
  Printf.fprintf channel "n, qtime, ttime\n";
  List.iter
    ~f:(fun (a, b, c) -> Printf.fprintf channel "%i, %f, %f\n" a b c)
    data;
  close_out channel
