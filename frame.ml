open Utility
open Type

type t =
{
  stack : Evaluation_stack.t;
  local_store : Local_store.t;
  resume_at : instruction_address;
  store : variable_location option
}

let resume_at frame =
  frame.resume_at

let store frame =
  frame.store

let peek_stack frame =
  Evaluation_stack.peek frame.stack

let pop_stack frame =
  { frame with stack = Evaluation_stack.pop frame.stack }

let push_stack frame value =
  { frame with stack = Evaluation_stack.push frame.stack value }

let write_local frame local value =
  { frame with local_store = Local_store.write_local frame.local_store local value }

let read_local frame local =
  Local_store.read_local frame.local_store local

let display frame =
  let (Instruction resume_at) = frame.resume_at in
  let locals = Local_store.display frame.local_store in
  let stack = Evaluation_stack.display frame.stack in
  Printf.sprintf "Locals %s\nStack %s\nResume at:%04x\n"
    locals stack resume_at
