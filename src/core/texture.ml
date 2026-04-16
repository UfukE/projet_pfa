type t =
    Image of Gfx.surface
  | Color of Gfx.color


let draw ctx dst pos box src =
  let x = int_of_float pos.Vector.x in
  let y = int_of_float pos.Vector.y in
  let Rect.{width;height} = box in
  match src with
    Image img -> Gfx.blit_scale ctx dst img x y width height
  | Color c ->
    Gfx.set_color ctx c;
    Gfx.fill_rect ctx dst x y width height