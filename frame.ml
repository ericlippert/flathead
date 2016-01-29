open Utility
open Iff
open Type

type t =
{
  stack : Evaluation_stack.t;
  local_store : Local_store.t;
  called : instruction_address;
  resume_at : instruction_address;
  store : variable_location option
}

let make pc =
{
  stack = Evaluation_stack.empty;
  local_store = Local_store.empty;
  called = pc;
  resume_at = Instruction 0;
  store = None
}

let make_call_frame story arguments routine_address resume_at store =
  let default_store = Local_store.create_default_locals story routine_address in
  let local_store = Local_store.write_arguments default_store arguments in
  let called = Routine.first_instruction story routine_address in
  {
    stack = Evaluation_stack.empty;
    local_store;
    called;
    resume_at;
    store
  }

let called frame =
  frame.called

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

(* Handy debugging methods *)

let display_frame frame =
  let (Instruction called) = frame.called in
  let (Instruction resume_at) = frame.resume_at in
  Printf.sprintf "Locals %s\nStack %s\nResume at:%04x\nCurrent Routine: %04x\n"
    (Local_store.display_locals frame.local_store)
    (Evaluation_stack.display_stack frame.stack)
    resume_at
    called

let make_frame_record frame =
  let locals = Local_store.make_locals_record frame.local_store in
  let stack = Evaluation_stack.make_stack_records frame.stack in
  let arguments_byte = (1 lsl frame.local_store.Local_store.arguments_supplied) - 1 in
  let (Instruction resume_at) = frame.resume_at in
(* TODO Move this into the Quetzal module *)
  let (discard_value, target_variable) =
    match frame.store with
    | None -> (true, 0)
    | Some Stack -> (false, 0)
    | Some Local_variable Local n -> (false, n)
    | Some Global_variable Global n -> (false, n) in

(* TODO: Bit_number could take a bit number, not an integer *)
  Record [
    Integer24 (Some resume_at);
    BitField [
      Integer4 (Some (List.length locals));
      Bit (4, Some discard_value)];
    Integer8 (Some target_variable);
    BitField [
      Bit (0, Some (fetch_bit (Bit_number 0) arguments_byte));
      Bit (1, Some (fetch_bit (Bit_number 1) arguments_byte));
      Bit (2, Some (fetch_bit (Bit_number 2) arguments_byte));
      Bit (3, Some (fetch_bit (Bit_number 3) arguments_byte));
      Bit (4, Some (fetch_bit (Bit_number 4) arguments_byte));
      Bit (5, Some (fetch_bit (Bit_number 5) arguments_byte));
      Bit (6, Some (fetch_bit (Bit_number 6) arguments_byte))];
    Integer16 (Some (Evaluation_stack.length frame.stack));
    SizedList (Integer8 (Some (List.length locals)), locals );
    SizedList (Integer8 (Some (Evaluation_stack.length frame.stack)) , stack)]

let make_frame_from_record frame_record =
  let (ret_addr, locals_list, eval_stack,
      store, arg_count, locals_count) =
    match frame_record with
    | Record [
      Integer24 (Some ret_addr);
      BitField [
        Integer4 (Some locals_count);
        Bit (4, Some discard_value)];
      Integer8 (Some target_variable);
      BitField [
        Bit (0, Some a0);
        Bit (1, Some a1);
        Bit (2, Some a2);
        Bit (3, Some a3);
        Bit (4, Some a4);
        Bit (5, Some a5);
        Bit (6, Some a6)];
      Integer16 (Some _); (* size of evaluation stack in words *)
      SizedList (_, locals_list);
      SizedList (_, eval_stack)] ->
      let rec find_false n items =
        match items with
        | false :: _ -> n
        | true :: tail -> find_false (n + 1) tail
        | [] -> failwith "impossible" in
      let arg_count =
        find_false 0 [a0; a1; a2; a3; a4; a5; a6; false] in
      let store = (* TODO: Use decode_variable *)
        match (discard_value, target_variable) with
        | (true, _) -> None
        | (false, n) -> Some (Instruction.decode_variable n) in

      (Instruction ret_addr, locals_list, eval_stack,
        store, arg_count, locals_count)
    | _ -> failwith "TODO handle failure reading frame" in
  let stack = Evaluation_stack.make_stack_from_record eval_stack in
  let local_store = Local_store.make_locals_from_record arg_count locals_list in
  { stack;
    local_store;
    called = Instruction 0;
    resume_at = ret_addr ;
    store
    }
