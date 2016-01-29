open Type
open Utility

let maximum_local = 15

let locals_count story (Routine routine_address) =
  let count = Story.read_byte story (Byte_address routine_address) in
  if count > maximum_local then failwith "routine must have fewer than 16 locals"
  else count

let first_instruction story (Routine routine_address) =
  (* Spec:
  * A routine begins with one byte indicating the number of local
    variables it has (between 0 and 15 inclusive).
  * In Versions 1 to 4, that number of 2-byte words follows, giving initial
    values for these local variables.
  * In Versions 5 and later, the initial values are all zero.
  * Execution of instructions begins from the byte after this header
    information *)
  if Story.v4_or_lower (Story.version story) then
    let count = locals_count story (Routine routine_address) in
    Instruction (routine_address + 1 + count * 2)
  else
    Instruction (routine_address + 1)

(* Note that here the locals are indexed from 1 to 15, not 0 to 14 *)
let local_default_value story (Routine routine_address) n =
  if n < 1 || n > maximum_local then
    failwith "invalid local"
  else if Story.v4_or_lower (Story.version story) then
    Story.read_word story (Word_address (routine_address + 1 + 2 * (n - 1)))
  else
    0
