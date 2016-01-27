open Utility
open Type

let empty = Status None

let make story =
  let current_object_global = Global 16 in
  let current_score_global = Global 17 in (* also hours *)
  let turn_count_global = Global 18 in (* also minutes *)

  let current_object () =
    Object (Globals.read story current_object_global) in
  let current_object_name () =
    let c = current_object () in
    if c = Object.invalid_object then ""
    else Object.name story c in
  let status_globals () =
    let score = signed_word (Globals.read story current_score_global) in
    let turn = Globals.read story turn_count_global in
    (score, turn) in
  let build_status_line right =
    let right_length = String.length right in
    let left = current_object_name () in
    let left_length = String.length left in
    let (Character_width width) = Story.screen_width story in
    let left_trimmed =
      if left_length + right_length < width then left
      else left_string left (width - right_length - 1) in (* TODO: Assumes that width >= right_length *)
    let space_count = width - right_length - (String.length left_trimmed) in
    left_trimmed ^ (spaces space_count) ^ right in
  let time_status () =
    let (hours, minutes) = status_globals () in
    let suffix = if hours >= 12 then "PM" else "AM" in
    let adjusted_hours = (hours mod 12) + 12 in
    let text = Printf.sprintf "%d:%02d%s" adjusted_hours minutes suffix in
    build_status_line text in
  let score_status () =
    let (score, turns) = status_globals () in
    let text = Printf.sprintf "%d/%d" score turns in
    build_status_line text in
  match Story.status_line_kind story with
  | NoStatus -> empty
  | TimeStatus -> Status (Some (time_status ()) )
  | ScoreStatus -> Status (Some (score_status ()) )
