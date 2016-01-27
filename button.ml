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

let make x y (Pixel_width margin) text =
  let ((Pixel_width text_width), (Pixel_height text_height)) = text_size text in
  {
    x = x;
    y = y;
    width = Pixel_width (text_width + 2 * margin);
    height = Pixel_height (text_height + 2 * margin);
    text = text
  }

let draw button =
  let ((Pixel_width text_width), (Pixel_height text_height)) = text_size button.text in
  let (Pixel_x x) = button.x in
  let (Pixel_y y) = button.y in
  let (Pixel_height h) = button.height in
  let (Pixel_width w) = button.width in
  let text_x = Pixel_x (x + (w - text_width) / 2) in
  let text_y = Pixel_y (y + (h - text_height) / 2) in
  fill_rect Graphics.blue button.x button.y button.width button.height;
  draw_string_at button.text Graphics.white text_x text_y

let was_clicked button (Pixel_x x) (Pixel_y y) =
  let (Pixel_x bx) = button.x in
  let (Pixel_y by) = button.y in
  let (Pixel_width bw) = button.width in
  let (Pixel_height bh) = button.height in
  (bx <= x) &&
  (x <= (bx + bw)) &&
  (by <= y) &&
  (y <= by + bh)
