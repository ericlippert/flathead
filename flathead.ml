open Type
open Utility

let () = 
  let story = Story.load "minizork.z3" in
  let locals = Local_store.create_default_locals story (Routine 0x3b36) in
  let text = Local_store.display locals in
  Printf.printf "%s\n" text
