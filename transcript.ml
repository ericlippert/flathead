open Utility

type t =
{
  lines : string list;
  width : int
}

let empty = { lines = []; width = 80 }

(* Word-wraps the last line in a list of lines. Assumes that
the tail of the list is already word-wrapped. Returns the
new list. *)

let rec wrap_lines lines line_length =
  match lines with
  | [] -> []
  | unwrapped_line :: wrapped_lines ->
    if String.contains unwrapped_line '\n' then
      (* Recursive case 1: there is a return in the last string.
       Split the string, solve the wrapping problem with no return,
       and then recurse on the remainder of the string. *)
      let (left, right) = break_string unwrapped_line '\n' in
      let w1 = wrap_lines (left :: wrapped_lines) line_length in
      wrap_lines (right :: w1) line_length
    else
      let len = String.length unwrapped_line in
      if len > line_length then
        (* Recursive case 2: there are no returns but the line is too long.
           Try to find a space to break on, break it, and recurse. *)
        let space_location = reverse_index_from unwrapped_line ' ' line_length in
        let break_point =
          match space_location with
          | None -> line_length
          | Some location -> location in
        let right = right_string unwrapped_line (break_point + 1)  in
        let left = left_string unwrapped_line break_point in
        (* The left portion is short enough but the right might not be. *)
        wrap_lines (right :: left :: wrapped_lines) line_length
      else
        (* Base case: the line has no breaks and is short enough. Do nothing. *)
        lines

let append transcript text =
  let unwrapped_lines =
    match transcript.lines with
    | [] -> [text]
    | h :: t -> (h ^ text) :: t in
  let wrapped_lines = wrap_lines unwrapped_lines transcript.width in
  { transcript with lines = wrapped_lines }
