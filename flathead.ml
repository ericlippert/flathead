open Type
open Utility

let () = 
  let story = Story.load "minizork.z3" in
  let table = Object.display_object_table story in
  Printf.printf "%s\n" table
