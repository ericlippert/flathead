open Utility
open Iff
open Story

type t =
{
  stack : int list;
  locals : int IntMap.t;
  locals_count : int;
  called : int;
  resume_at : int;
  arguments_supplied : int;
  store : variable_location option
}

let make pc =
{
  stack = [];
  locals = IntMap.empty;
  locals_count = 0;
  called = pc;
  resume_at = 0;
  arguments_supplied = 0;
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
  let value = unsigned_word value in
  let new_locals = IntMap.add local value frame.locals in
  { frame with locals = new_locals }

let read_local frame local =
  IntMap.find local frame.locals

(* Handy debugging methods *)
let display_locals frame =
  let to_string local value =
    Printf.sprintf "local%01x=%04x " (local - 1) value in
  let folder local value acc =
    acc ^ (to_string local value) in
  let locals = frame.locals in
  IntMap.fold folder locals ""

let display_stack frame =
  let to_string stack_value =
    Printf.sprintf " %04x" stack_value in
  let folder acc stack_value =
    acc ^ (to_string stack_value) in
  let stack =  frame.stack in
  List.fold_left folder "" stack

let display_frame frame =
  Printf.sprintf "Locals %s\nStack %s\nResume at:%04x\nCurrent Routine: %04x\n"
    (display_locals frame) (display_stack frame) frame.resume_at frame.called

let make_frame_record frame =
  let rec make_locals acc n =
    if n = 0 then
      acc
    else
      make_locals ((Integer16 (Some (IntMap.find n frame.locals))) :: acc) (n - 1) in
  let locals = make_locals [] frame.locals_count in
  let rec make_stack acc st =
    match st with
    | [] -> acc
    | h :: t ->
      make_stack ((Integer16 (Some (h))) :: acc) t in
  let stack = List.rev (make_stack [] frame.stack) in
  let arguments_byte = (1 lsl frame.arguments_supplied) - 1 in
  let (discard_value, target_variable) =
    match frame.store with
    | None -> (true, 0)
    | Some Stack -> (false, 0)
    | Some (Local n) -> (false, n)
    | Some (Global n) -> (false, n) in

  Record [
    Integer24 (Some frame.resume_at);
    BitField [
      Integer4 (Some frame.locals_count);
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
    SizedList (Integer8 (Some frame.locals_count), locals );
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
  let rec make_locals map i locs =
    match locs with
    | [] -> map
    | h :: t ->
      let v = decode_int16 h in
      let new_map = IntMap.add i v map in
      make_locals new_map (i + 1) t in
  let locals = make_locals IntMap.empty 1 locals_list in
  { stack;
    locals;
    locals_count;
    called = 0;
    resume_at = ret_addr ;
    arguments_supplied = arg_count;
    store
    }
