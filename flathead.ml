open Type
open Utility

let () = 
  let story = Story.load "minizork.z3" in
  let zstring = Zstring.abbreviation_zstring story (Abbreviation 0) in
  let text = Zstring.display_bytes story zstring in
  Printf.printf "%s\n" text;
  let zstring = Zstring.abbreviation_zstring story (Abbreviation 4) in
  let text = Zstring.display_bytes story zstring in
  Printf.printf "%s\n" text
