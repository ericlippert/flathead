open Story
open Instruction
open Utility
open Type

(* Any given instruction in a routine either goes on to the next instruction,
when it is done, or branches to another instruction when it is done, or terminates
the routine. Given the address of an instruction, what are all the reachable instructions
in this routine? Note that this could miss instructions if a jump is made to a location
read from a variable. *)
let all_reachable_addresses_in_routine story instr_address =
  let immediately_reachable_addresses address =
    let instr = decode_instruction story address in
    let next =
      if continues_to_following instr.opcode then [Instruction.following instr]
      else [] in
    match (branch_target instr) with
    | Some address -> address :: next
    | _ -> next in
  reflexive_closure instr_address immediately_reachable_addresses

let display_reachable_instructions story address =
  let reachable = all_reachable_addresses_in_routine story address in
  let sorted = List.sort compare reachable in
  let to_string addr =
    let instr = decode_instruction story addr in
    Instruction.display instr (version story)  in
  accumulate_strings to_string sorted

let display_routine story routine_address =
  let first = first_instruction story routine_address in
  display_reachable_instructions story first

(* Takes an instruction; if it is a call, produces the unpacked address
of the called routine, if possible. *)
let call_address story instr =
  if is_call (version story) instr.opcode then
    match instr.operands with
    | (Large packed_address) :: _ ->
      let packed_address = Packed_routine packed_address in
      let unpacked_address = decode_routine_packed_address story packed_address in
      Some unpacked_address
    | _ -> None
  else
    None

(* Takes the address of the first instruction in a routine, produces
   a list of addresses of all routines called in the routine. *)
(* Again, this can miss routines that are called with a variable as the address. *)
let reachable_routines_in_routine story instr_address =
  let reachable_instrs = all_reachable_addresses_in_routine story instr_address in
  let option_fold routines instr_addr =
    let instr = decode_instruction story instr_addr in
    match call_address story instr with
    | None -> routines
    | Some routine_address -> routine_address :: routines in
  List.fold_left option_fold [] reachable_instrs

let all_routines story =
  let ipc = initial_program_counter story in
  let called_by_main = reachable_routines_in_routine story ipc in
  let relation routine =
      reachable_routines_in_routine story (first_instruction story routine) in
  let all_routines = reflexive_closure_many called_by_main relation in
  List.sort compare all_routines

let display_all_routines story =
  let routines = all_routines story in
  let to_string r =
    (display_routine story r) ^ "\n\n" in
  accumulate_strings to_string routines
