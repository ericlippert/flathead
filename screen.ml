open Utility
open Type

type t =
{
  status : status_line;
  upper_window : Window.t;
  lower_window : Window.t;
  height : character_height;
  width : character_width;
  selected_window : window_selection;
}

let make height width =
  let (Character_height h) = height in
  let y = (Character_y h) in
  {
    status = Status None;
    upper_window = Window.make
      (Character_height 0)
      width
      Window.top_left
      Word_wrap_disabled
      Scroll_disabled
      More_disabled;
    lower_window = Window.make
      height
      width
      (Cursor (Window.left_column, y))
      Word_wrap_enabled
      Scroll_enabled
      More_enabled;
    height;
    width;
    selected_window = Lower_window
  }

let width screen =
  screen.width

let height screen =
  screen.height

let status screen =
  screen.status

let set_status screen status =
  { screen with status }

let selected_window screen =
  screen.selected_window

let lines screen =
  Window.merge screen.upper_window screen.lower_window

let get_line_at screen (Character_y y) =
  Deque.peek_back_at (lines screen) (y - 1)

let erase_upper screen =
  { screen with upper_window = Window.erase screen.upper_window }

let erase_lower screen =
  { screen with lower_window = Window.erase screen.lower_window }

let erase_all screen =
  erase_upper (erase_lower screen)

let upper_cursor screen =
  Window.cursor screen.upper_window

let lower_cursor screen =
  (* Obtain the location of the lower window cursor in screen coordinates *)
  let c = Window.cursor screen.lower_window in
  let h = Window.height screen.upper_window in
  Window.move_cursor_down c h

let set_upper_cursor screen cursor =
  { screen with upper_window = Window.set_cursor screen.upper_window cursor }

let set_lower_cursor screen cursor =
  (* The cursor here is in screen coordinates; translate to lower window coordinates. *)
  let h = Window.height screen.upper_window in
  let cursor = Window.move_cursor_up cursor h  in
  { screen with lower_window = Window.set_cursor screen.lower_window cursor }

let set_cursor screen cursor =
  match screen.selected_window with
  | Lower_window -> set_lower_cursor screen cursor
  | Upper_window -> set_upper_cursor screen cursor

let split_window screen new_upper_height =
  (* Splitting does not change the contents of the screen, but it can
  change what window is selected and where the cursor is. *)

  (* First figure out what lines go in which window. *)
  let (Character_height h) = new_upper_height in
  let (new_upper_lines, new_lower_lines) = Deque.split (lines screen) h in
  let upper_window = Window.set_lines screen.upper_window new_upper_lines in
  let lower_window = Window.set_lines screen.lower_window new_lower_lines in
  let new_screen = { screen with upper_window; lower_window } in

  (* We have a new screen with the right text in the windows, but the cursors
  are possibly wrong. Fix the upper cursor first. *)
  let new_screen =
    if screen.selected_window = Lower_window then
      (* If the lower window is selected when the screen is split then the
        upper window cursor is reset to 1, 1. *)
      set_upper_cursor new_screen Window.top_left
    else
      (* Spec 8.7.2.1.1
      It is unclear exactly what split_window should do if the upper window is
      currently selected. The author suggests that it should work as usual,
      leaving the cursor where it is if the cursor is still inside the new upper
      window, and otherwise moving the cursor back to the top left. *)
      let upper_cursor = upper_cursor screen in
      if Window.cursor_in_bounds upper_window upper_cursor then
        new_screen
      else
        set_upper_cursor new_screen Window.top_left in

  (* The upper cursor is now correct, but the lower one is still wrong. *)

  (* Spec 8.7.2.2
  If a split takes place which would cause the upper window to swallow the
  lower window's cursor position, the interpreter should move the lower
  window's cursor down to the line just below the upper window's new size. *)
  let new_screen = { new_screen with upper_window; lower_window } in
  let original_lower_cursor = lower_cursor screen in
  let new_screen =
    if Window.cursor_in_bounds upper_window original_lower_cursor then
      let new_cursor = Window.set_cursor_y original_lower_cursor (Character_y (h + 1)) in
      set_lower_cursor new_screen new_cursor
    else
      set_lower_cursor new_screen original_lower_cursor in
  (* The lower cursor is now in place. Did the window selection change? *)
  if h = 0 && screen.selected_window = Upper_window then
    { new_screen with selected_window = Lower_window }
  else
    new_screen

let set_window screen w =
  (* SPEC 8.7.2 Whenever the upper window is selected, its
    cursor position is reset to the top left. *)
  let s = { screen with selected_window = w } in
  if w = Upper_window then set_upper_cursor s Window.top_left
  else s

let print screen text =
  match screen.selected_window with
  | Lower_window ->
    { screen with lower_window = Window.print screen.lower_window text }
  | Upper_window ->
    { screen with upper_window = Window.print screen.upper_window text }

let scroll screen =
  { screen with lower_window = Window.scroll screen.lower_window }

let fully_scroll screen =
  { screen with lower_window = Window.fully_scroll screen.lower_window }

let needs_scroll screen =
  Window.has_pending screen.lower_window

let needs_more screen =
  Window.needs_more screen.lower_window

let more screen =
  { screen with lower_window = Window.more screen.lower_window }

let clear_more screen =
  { screen with lower_window = Window.clear_more screen.lower_window }

let erase_line screen =
match screen.selected_window with
| Lower_window ->
  { screen with lower_window = Window.erase_line screen.lower_window }
| Upper_window ->
  { screen with upper_window = Window.erase_line screen.upper_window }

let get_active_cursor screen =
  match screen.selected_window with
  | Lower_window -> Window.cursor screen.lower_window
  | Upper_window -> Window.cursor screen.upper_window

let set_word_wrap screen can_wrap =
  let lower_window = Window.set_can_wrap screen.lower_window can_wrap in
  { screen with lower_window }

let set_lower_cursor_bottom_left screen =
  let (Character_height h) = screen.height in
  let cursor = Cursor (Window.left_column, (Character_y h)) in
  set_lower_cursor screen cursor
