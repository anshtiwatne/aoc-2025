open Lib.Utils

let part1 inp =
  let step (pos, count) turn =
    Scanf.sscanf turn "%c%d" (fun dir mag ->
      let diff = if dir = 'R' then mag else -mag in
      let next = (pos + (diff mod 100) + 100) mod 100 in
      let count' = if next = 0 then count + 1 else count in
      next, count')
  in
  snd (List.fold_left step (50, 0) inp)
;;

let part2 inp =
  let step (pos, count) turn =
    Scanf.sscanf turn "%c%d" (fun dir mag ->
      let zeros =
        if dir = 'R' then (pos + mag) / 100 else (mag + ((100 - pos) mod 100)) / 100
      in
      let next = (pos + ((if dir = 'R' then mag else -mag) mod 100) + 100) mod 100 in
      next, count + zeros)
  in
  snd (List.fold_left step (50, 0) inp)
;;

let () =
  let input = read_input '\n' 1 in
  Printf.printf "Part 1: %d\n" (part1 input);
  Printf.printf "Part 2: %d\n" (part2 input)
;;
