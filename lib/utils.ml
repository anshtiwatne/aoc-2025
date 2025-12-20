open Base
open Stdio

let read_lines day = In_channel.read_lines (Printf.sprintf "input/day%d.txt" day)
