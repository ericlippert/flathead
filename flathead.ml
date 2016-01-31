open Type
open Utility

let () = 
  let story = Story.load "minizork.z3" in
  let tree = Object.display_object_tree story in
  Printf.printf "%s\n" tree
