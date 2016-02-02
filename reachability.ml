open Utility
open Type

(* Any given instruction in a routine either goes on to the next instruction,
when it is done, or branches to another instruction when it is done, or terminates
the routine. Given the address of an instruction, what are all the reachable instructions
in this routine? Note that this could miss instructions if a jump is made to a location
read from a variable. *)

let following_instruction instr =
  if Instruction.continues_to_following (Instruction.opcode instr) then
    let (Instruction addr) = (Instruction.address instr) in
    let length = (Instruction.length instr) in
    [Instruction (addr + length)]
  else
    []

let branch_target_instruction instr =
  match Instruction.branch instr with
  | None
  | Some (_, Return_false)
  | Some (_, Return_true) -> []
  | Some (_, Branch_address address) -> [address]

let jump_target_instruction instr =
  match (Instruction.opcode instr, Instruction.operands instr) with
  | (OP1_140, [Large offset]) ->
    let offset = signed_word offset in
    [ Instruction.jump_address instr offset ]
  | _ -> []

let all_reachable_addresses_in_routine story instr_address =
  let immediately_reachable_addresses address =
    let instr = Instruction.decode story address in
    let following = following_instruction instr in
    let branch = branch_target_instruction instr in
    let jump = jump_target_instruction instr in
    following @ branch @ jump in
  reflexive_closure instr_address immediately_reachable_addresses

let display_reachable_instructions story address =
  let reachable = all_reachable_addresses_in_routine story address in
  let sorted = List.sort compare reachable in
  let to_string addr =
    let instr = Instruction.decode story addr in
    Instruction.display instr (Story.version story)  in
  accumulate_strings to_string sorted
