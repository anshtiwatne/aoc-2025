open Lib.Utils

let is_invalid_p1 n =
  let s = string_of_int n in
  let l = String.length s in
  l mod 2 = 0 && String.sub s 0 (l / 2) = String.sub s (l / 2) (l / 2)
;;

let is_invalid_p2 n =
  let s = string_of_int n in
  let l = String.length s in
  List.init (l / 2) (fun i -> i + 1)
  |> List.exists (fun len ->
    l mod len = 0
    && String.concat "" (List.init (l / len) (fun _ -> String.sub s 0 len)) = s)
;;

let solve invalid_f ranges =
  ranges
  |> List.concat_map (fun r ->
    Scanf.sscanf r "%d-%d" (fun a b -> List.init (b - a + 1) (( + ) a)))
  |> List.filter invalid_f
  |> List.sort_uniq compare
  |> List.fold_left ( + ) 0
;;

let () =
  let input = read_input ',' 2 in
  Printf.printf "Part 1: %d\n" (solve is_invalid_p1 input);
  Printf.printf "Part 2: %d\n" (solve is_invalid_p2 input)
;;
