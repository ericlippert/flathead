open Type
open Utility

let () = 
  let story = Story.load "minizork.z3" in
  let dict = Dictionary.display story in
  Printf.printf "%s\n" dict
