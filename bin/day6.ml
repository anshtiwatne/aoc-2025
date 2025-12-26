open Lib.Utils

let rec transpose = function
  | [] | [] :: _ -> []
  | m -> List.map List.hd m :: transpose (List.map List.tl m)
;;

let rec part1 vals ops =
  match vals, ops with
  | v :: vs, op :: t ->
    let f, init = if op = "*" then ( * ), 1 else ( + ), 0 in
    List.fold_left (fun a s -> f a (int_of_string s)) init v + part1 vs t
  | _ -> 0
;;

let () =
  let parsed =
    read_input '\n' 6
    |> List.map (fun s -> String.split_on_char ' ' s |> List.filter (( <> ) ""))
    |> List.rev
  in
  let ops, vals = List.hd parsed, transpose (List.tl parsed) in
  Printf.printf "Part 1: %d\n" (part1 vals ops)
;;
(* Printf.printf "Part 2: %d\n" TODO *)
