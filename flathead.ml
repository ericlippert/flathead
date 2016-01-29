open Type
open Utility;;

let word = 0xBEEF

let fetch_bits_original high length word =
  let mask = lnot (-1 lsl length) in
  (word lsr (high - length + 1)) land mask

let () =
  Printf.printf "\n\n%0x\n\n" ((word lsr 12) land (lnot (-1 lsl 15)));
  Printf.printf "\n\n%0x\n\n" (fetch_bits_original 15 4 word);
  Printf.printf "\n\n%0x\n\n" (fetch_bits bit15 size4 word)
