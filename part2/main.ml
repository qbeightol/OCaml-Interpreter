open Ast
open Printf
open Repl

let rec chars c n = if n=0 then "" else Char.escaped c^chars c (n-1)
let spaces = chars ' '
let stars  = chars '*'

let greet () =
  let line1 = sprintf "%s%s%s" (spaces 28) (stars 24) (spaces 28) in
  let line2 = sprintf "%s%s%s" (stars 29) " Welcome to 3110Caml  " (stars 29) in
  let line3 = sprintf
    "%s%s%s%s%s" (stars 2) (spaces 26) (stars 24) (spaces 26) (stars 2) in
  let blank = stars 2^spaces 76^stars 2 in
  let format_commands = List.map (fun s ->
    sprintf "%s %s%s%s" (stars 2) s (spaces (75-String.length s)) (stars 2)) in
  let greeting =
    [line1;line2;line3;blank]@format_commands commands@[blank;(stars 80)] in
  List.iter print_endline greeting

let main () =
  let n = Array.length Sys.argv in
  if n=1 then begin
    greet ();
    repl IdMap.empty (Hashtbl.create 16)
  end
  else if n=2 then handle_file Sys.argv.(1)
  else print_endline "usage: ./interpreter.exe [file]"; exit 1

let _ = main ()
