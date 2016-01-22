(* The frame set is the stack of activation frames; each activation frame
has a local variable storage, evaluation stack, and information about
the call site that created this activation. *)

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

let display_frames frameset =
  let folder acc f =acc ^ (Frame.display_frame f) in
  (List.fold_left folder "" frameset.frames) ^ (Frame.display_frame frameset.initial_frame)

let make_frameset_record frameset =
  let head = Frame.make_frame_record frameset.initial_frame in
  let tail = List.rev (List.map Frame.make_frame_record frameset.frames) in
  List.rev (head :: tail)

let make_frameset_from_records frame_records =
  (* TODO: Handle the error case where there is no initial record. *)
  let oldest_first = List.rev frame_records in
  let initial_frame = Frame.make_frame_from_record (List.hd oldest_first) in
  let newest_first = List.rev (List.tl oldest_first) in
  let frames = List.map Frame.make_frame_from_record newest_first in
  { initial_frame; frames }
