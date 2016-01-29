open Utility
open Type

(* Any given instruction in a routine either goes on to the next instruction,
when it is done, or branches to another instruction when it is done, or terminates
the routine. Given the address of an instruction, what are all the reachable instructions
in this routine? Note that this could miss instructions if a jump is made to a location
read from a variable. *)

(* Suppose an instruction either has a branch portion to an address,
or a jump to an address. What is that address? *)
let branch_target instr =
  let br_target =
    match Instruction.branch instr with
    | None -> None
    | Some (_, Return_false) -> None
    | Some (_, Return_true) -> None
    | Some (_, Branch_address address) -> Some address in
  let jump_target =
    match (Instruction.opcode instr, Instruction.operands instr) with
    | (OP1_140, [Large offset]) ->
      let offset = signed_word offset in
      Some (Instruction.jump_address instr offset)
    | _ -> None in
  match (br_target, jump_target) with
  | (Some b, _) -> Some b
  | (_, Some j) -> Some j
  | _ -> None

let all_reachable_addresses_in_routine story instr_address =
  let immediately_reachable_addresses address =
    let instr = Instruction.decode story address in
    let next =
      if Instruction.continues_to_following (Instruction.opcode instr) then
        [Instruction.following instr]
      else
        [] in
    match (branch_target instr) with
    | Some address -> address :: next
    | _ -> next in
  reflexive_closure instr_address immediately_reachable_addresses

let display_reachable_instructions story address =
  let reachable = all_reachable_addresses_in_routine story address in
  let sorted = List.sort compare reachable in
  let to_string addr =
    let instr = Instruction.decode story addr in
    Instruction.display instr (Story.version story)  in
  accumulate_strings to_string sorted

let display_routine story routine_address =
  let first = Routine.first_instruction story routine_address in
  display_reachable_instructions story first

(* Takes the address of the first instruction in a routine, produces
   a list of addresses of all routines called in the routine. *)
(* Again, this can miss routines that are called with a variable as the address. *)
let reachable_routines_in_routine story instr_address =
  let reachable_instrs = all_reachable_addresses_in_routine story instr_address in
  let option_fold routines instr_addr =
    let instr = Instruction.decode story instr_addr in
    match Instruction.call_address instr story with
    | None -> routines
    | Some routine_address -> routine_address :: routines in
  List.fold_left option_fold [] reachable_instrs

let all_routines story =
  let ipc = Story.initial_program_counter story in
  let called_by_main = reachable_routines_in_routine story ipc in
  let relation routine =
      reachable_routines_in_routine story (Routine.first_instruction story routine) in
  let all_routines = reflexive_closure_many called_by_main relation in
  List.sort compare all_routines

let display_all_routines story =
  let routines = all_routines story in
  let to_string r =
    (display_routine story r) ^ "\n\n" in
  accumulate_strings to_string routines
