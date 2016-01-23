open Story
open Utility

type t =
{
  line : string option
}

let emtpt = { line = None }

let make story =
  let build_status_line right =
    let right_length = String.length right in
    let left = current_object_name story in
    let left_length = String.length left in
    let width = screen_width story in
    let left_trimmed =
      if left_length + right_length < width then left
      else left_string left (width - right_length - 1) in (* TODO: Assumes that width >= right_length *)
    let space_count = width - right_length - (String.length left_trimmed) in
    left_trimmed ^ (spaces space_count) ^ right in
  let time_status () =
    let (hours, minutes) = status_globals story in
    let suffix = if hours >= 12 then "PM" else "AM" in
    let adjusted_hours = (hours mod 12) + 12 in
    let text = Printf.sprintf "%d:%02d%s" adjusted_hours minutes suffix in
    build_status_line text in
  let score_status () =
    let (score, turns) = status_globals story in
    let text = Printf.sprintf "%d/%d" score turns in
    build_status_line text in
  match status_line_kind story with
  | NoStatus -> { line = None }
  | TimeStatus -> { line = Some (time_status()) }
  | ScoreStatus -> { line = Some (score_status()) }
