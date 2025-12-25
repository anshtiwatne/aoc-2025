open Base
open Stdio

let read_input sep day =
  let content = In_channel.read_all (Printf.sprintf "input/day%d.txt" day) in
  String.split content ~on:sep
  |> List.map ~f:String.strip
  |> List.rev
  |> List.drop_while ~f:String.is_empty
  |> List.rev
;;
