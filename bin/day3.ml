open Lib.Utils

(* let part1 inp =
  let max_c s = String.fold_left max '0' s in
  let max_joltage bat =
    let m1 = max_c (String.sub bat 0 (String.length bat - 1)) in
    let i = String.index bat m1 + 1 in
    let m2 = max_c (String.sub bat i (String.length bat - i)) in
    int_of_string (String.make 1 m1 ^ String.make 1 m2)
  in
  List.fold_left (fun acc s -> acc + max_joltage s) 0 inp
;; *)

let solve n inp =
  let rec bat_seq k s =
    if k = 0
    then ""
    else (
      let lim = String.length s - k + 1 in
      let m = String.fold_left max '0' (String.sub s 0 lim) in
      let i = String.index s m + 1 in
      String.make 1 m ^ bat_seq (k - 1) (String.sub s i (String.length s - i)))
  in
  List.fold_left (fun acc s -> acc + int_of_string (bat_seq n s)) 0 inp
;;

let () =
  let input = read_input '\n' 3 in
  Printf.printf "Part 1: %d\n" (solve 2 input);
  Printf.printf "Part 2: %d\n" (solve 12 input)
;;
