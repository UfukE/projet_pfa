let window_width = 1200
let window_height = 700

let black   = Gfx.color 0 0 0 255
let white   = Gfx.color 255 255 255 255
let red a   = Gfx.color 255 0 0 a
let green a = Gfx.color 0 255 0 a
let blue a  = Gfx.color 0 0 255 a
let gold    = Gfx.color 239 191 4 255
let transparent = Gfx.color 0 0 0 0


let core_x = float (window_width/2)
let core_y = float (window_height/2)

type buildings = Archer | Bomber | Freezer

let building_cost = function
  | Archer  -> 20
  | Bomber  -> 40
  | Freezer -> 30

let building_color a = function
  | Archer  -> green a
  | Bomber  -> red  a
  | Freezer -> blue a