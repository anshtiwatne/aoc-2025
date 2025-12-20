open Base
open Stdio
open Lib

let () =
  let lines = Utils.read_lines 1 in
  printf "Input has %d lines\n" (List.length lines)
;;
