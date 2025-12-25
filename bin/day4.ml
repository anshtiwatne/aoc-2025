open Lib.Utils

let part1 inp =
  let h = List.length inp in
  let w = String.length (List.hd inp) in
  let adj = [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ] in
  let is_roll x y =
    try (List.nth inp y).[x] = '@' with
    | _ -> false
  in
  List.init (h * w) (fun i -> i / w, i mod w)
  |> List.filter (fun (x, y) ->
    is_roll x y
    && List.length (List.filter (fun (ax, ay) -> is_roll (x + ax) (y + ay)) adj) < 4)
  |> List.length
;;

let () =
  let input = read_input '\n' 4 in
  Printf.printf "Part 1: %d\n" (part1 input)
;;
