open Lib.Utils

(* let part1 single_pass inp =
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
    && List.length (List.filter (fun (dx, dy) -> is_roll (x + dx) (y + dy)) adj) < 4)
  |> List.length
;; *)

let solve single_pass inp =
  let w = String.length (List.hd inp) in
  let h = List.length inp in
  let adj = [ -1, -1; -1, 0; -1, 1; 0, -1; 0, 1; 1, -1; 1, 0; 1, 1 ] in
  let char_at g x y =
    try (List.nth g y).[x] with
    | _ -> '.'
  in
  let rec aux grid =
    let next =
      List.init h (fun y ->
        String.init w (fun x ->
          let c = char_at grid x y in
          if
            c = '@'
            && List.length
                 (List.filter (fun (dx, dy) -> char_at grid (x + dx) (y + dy) = '@') adj)
               < 4
          then 'X'
          else c))
    in
    if single_pass || next = grid then next else aux next
  in
  List.fold_left
    (fun acc row ->
       acc + String.fold_left (fun acc c -> if c = 'X' then acc + 1 else acc) 0 row)
    0
    (aux inp)
;;

let () =
  let input = read_input '\n' 4 in
  Printf.printf "Part 1: %d\n" (solve true input);
  Printf.printf "Part 2: %d\n" (solve false input)
;;
