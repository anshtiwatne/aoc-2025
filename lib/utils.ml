open Base
open Stdio

let read_input sep day =
  let content = In_channel.read_all (Printf.sprintf "input/day%d.txt" day) in
  String.split content ~on:sep
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))
;;
