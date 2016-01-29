open Type
open My_graphics

type t =
{
  x : pixel_x;
  y : pixel_y;
  width : pixel_width;
  height : pixel_height;
  text : string
}

let width button =
  button.width

let height button =
  button.height

let x button =
  button.x

let y button =
  button.y

let text button =
  button.text

let make x y margin_w margin_h text =
  let (text_width, text_height) = text_size text in
  {
    x = x;
    y = y;
    width = add_margin_w text_width margin_w;
    height = add_margin_h text_height margin_h;
    text = text
  }

let draw button =
  let (text_width, text_height) = text_size button.text in
  let text_x = add_pixels_x button.x (margin_w button.width text_width) in
  let text_y = add_pixels_y button.y (margin_h button.height text_height) in
  fill_rect Graphics.blue button.x button.y button.width button.height;
  draw_string_at button.text Graphics.white text_x text_y

let was_clicked button x y =
  (contains_x button.x button.width x) && (contains_y button.y button.height y)
