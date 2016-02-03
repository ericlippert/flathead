(* The frame set is the stack of activation frames; each activation frame
has a local variable storage, evaluation stack, and information about
the call site that created this activation. *)

open Type
open Utility

type t =
{
  initial_frame : Frame.t;
  frames : Frame.t list
}

let make initial_frame =
  { initial_frame ; frames = []}

let current_frame frameset =
  match frameset.frames with
  | [] -> frameset.initial_frame
  | h :: _ -> h

let set_current_frame frameset frame =
  match frameset.frames with
  | [] -> { frameset with initial_frame = frame }
  | _ :: t -> { frameset with frames = frame :: t }

let add_frame frameset frame =
  { frameset with frames = frame :: frameset.frames }

let remove_frame frameset =
  match frameset.frames with
  | [] -> failwith "Attempting to remove initial frame"
  | _ :: t -> { frameset with frames = t }

let peek_stack frameset =
  Frame.peek_stack (current_frame frameset)

let pop_stack frameset =
  set_current_frame frameset (Frame.pop_stack (current_frame frameset))

let push_stack frameset value =
  set_current_frame frameset (Frame.push_stack (current_frame frameset) value)

let read_local frameset local =
  Frame.read_local (current_frame frameset) local

let write_local frameset local value =
  set_current_frame frameset (Frame.write_local (current_frame frameset) local value)

let display frameset =
  let to_string frame = 
    Frame.display frame in
  (accumulate_strings to_string frameset.frames) ^ (Frame.display frameset.initial_frame) 
