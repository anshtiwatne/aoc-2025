open Lib.Utils

let part1 ranges ids =
  let bounds = List.map (fun r -> Scanf.sscanf r "%d-%d" (fun a b -> a, b)) ranges in
  ids
  |> List.map int_of_string
  |> List.filter (fun n -> List.exists (fun (a, b) -> a <= n && n <= b) bounds)
  |> List.length
;;

(* very inefficient, quadratic time *)
(* let part2 ranges =
  ranges
  |> List.concat_map (fun s ->
    Scanf.sscanf s "%d-%d" (fun a b -> List.init (b - a + 1) (( + ) a)))
  |> List.fold_left (fun acc x -> if List.mem x acc then acc else x :: acc) []
  |> List.length
;; *)

let part2 ranges =
  let r =
    List.map (fun s -> Scanf.sscanf s "%d-%d" (fun a b -> a, b)) ranges
    |> List.sort compare
  in
  let rec aux count a b = function
    | (x, y) :: rest when x <= b + 1 -> aux count a (max b y) rest
    | (x, y) :: rest -> aux (count + (b - a + 1)) x y rest
    | [] -> count + (b - a + 1)
  in
  match r with
  | (a, b) :: rest -> aux 0 a b rest
  | [] -> 0
;;

let () =
  let input = read_input '\n' 5 in
  let ranges, ids =
    let rec aux acc = function
      | "" :: t -> List.rev acc, t
      | h :: t -> aux (h :: acc) t
      | [] -> List.rev acc, []
    in
    aux [] input
  in
  Printf.printf "Part 1: %d\n" (part1 ranges ids);
  Printf.printf "Part 2: %d\n" (part2 ranges)
;;
