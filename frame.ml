open Utility
open Iff
open Story

type t =
{
  stack : int list;
  local_store : Local_store.t;
  called : int;
  resume_at : int;
  store : variable_location option
}

let make pc =
{
  stack = [];
  local_store = Local_store.make IntMap.empty 0 0;
  called = pc;
  resume_at = 0;
  store = None
}

let peek_stack frame =
  match frame.stack with
  | [] -> failwith "peek_stack peeking an empty stack"
  | h :: _ -> h

let pop_stack frame =
  match frame.stack with
  | [] -> failwith "pop_stack popping empty stack"
  | _ :: t -> { frame with stack = t }

let push_stack frame value =
  let value = unsigned_word value in
  { frame with stack = value :: frame.stack }

let write_local frame local value =
  { frame with local_store = Local_store.write_local frame.local_store local value }

let read_local frame local =
  Local_store.read_local frame.local_store local

(* Handy debugging methods *)

let display_stack frame =
  let to_string stack_value =
    Printf.sprintf " %04x" stack_value in
  let folder acc stack_value =
    acc ^ (to_string stack_value) in
  let stack =  frame.stack in
  List.fold_left folder "" stack

let display_frame frame =
  Printf.sprintf "Locals %s\nStack %s\nResume at:%04x\nCurrent Routine: %04x\n"
    (Local_store.display_locals frame.local_store) (display_stack frame) frame.resume_at frame.called

let make_frame_record frame =
  let locals = Local_store.make_locals_record frame.local_store in
  let rec make_stack acc st =
    match st with
    | [] -> acc
    | h :: t ->
      make_stack ((Integer16 (Some (h))) :: acc) t in
  let stack = List.rev (make_stack [] frame.stack) in (* TODO: Be smarter *)
  let arguments_byte = (1 lsl frame.local_store.Local_store.arguments_supplied) - 1 in
  let (discard_value, target_variable) =
    match frame.store with
    | None -> (true, 0)
    | Some Stack -> (false, 0)
    | Some (Local n) -> (false, n)
    | Some (Global n) -> (false, n) in

  Record [
    Integer24 (Some frame.resume_at);
    BitField [
      Integer4 (Some (List.length locals));
      Bit (4, Some discard_value)];
    Integer8 (Some target_variable);
    BitField [
      Bit (0, Some (fetch_bit 0 arguments_byte));
      Bit (1, Some (fetch_bit 1 arguments_byte));
      Bit (2, Some (fetch_bit 2 arguments_byte));
      Bit (3, Some (fetch_bit 3 arguments_byte));
      Bit (4, Some (fetch_bit 4 arguments_byte));
      Bit (5, Some (fetch_bit 5 arguments_byte));
      Bit (6, Some (fetch_bit 6 arguments_byte))];
    Integer16 (Some (List.length frame.stack));
    SizedList (Integer8 (Some (List.length locals)), locals );
    SizedList (Integer8 (Some (List.length frame.stack)) , stack)]

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
      let maximum_local = 15 in (* TODO: Put this all somewhere more sensible *)
      let store = (* TODO: Use decode_variable *)
        match (discard_value, target_variable) with
        | (true, _) -> None
        | (false, 0) -> Some Stack
        | (false, n) -> if n <= maximum_local then Some (Local n) else Some (Global n) in

      (ret_addr, locals_list, eval_stack,
        store, arg_count, locals_count)
    | _ -> failwith "TODO handle failure reading frame" in
  let decode_int16 form =
    match form with
    | (Integer16 (Some v)) -> v
    | _ -> failwith "TODO handle failure reading evaluation stack / locals" in
  let stack = List.rev (List.map decode_int16 eval_stack) in
  let local_store = Local_store.make_locals_from_record arg_count locals_list in
  { stack;
    local_store;
    called = 0;
    resume_at = ret_addr ;
    store
    }
