let read_input sep day =
  let filename = Printf.sprintf "input/day%d.txt" day in
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let rec drop_while pred = function
    | [] -> []
    | x :: xs when pred x -> drop_while pred xs
    | lst -> lst
  in
  String.split_on_char sep content
  |> List.map String.trim
  |> List.rev
  |> drop_while (fun s -> String.length s = 0)
  |> List.rev
;;
