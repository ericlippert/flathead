open Type
open Utility

let () = 
  let story = Story.load "minizork.z3" in
  let zstring = Zstring 0xb106 in
  let text = Zstring.read story zstring in
  Printf.printf "%s\n" text;
