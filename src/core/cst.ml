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

type buildings = Archer | Bomber of Vector.t | Freezer

type state = Normal | Freeze

let bs = 40 (*building size*)


let building_cost = function
  | Archer    -> 20
  | Bomber _  -> 40
  | Freezer   -> 30

let building_color a = function
  | Archer    -> green a
  | Bomber _  -> red  a
  | Freezer   -> blue a

let archer_radius   = 150.0
let archer_cooldown = 60.0
let archer_damage   = 15

let bomber_cooldown   = 180.0
let bomber_radius = 50
let bomber_damage     = 30.0
let bomber_atk_duration = 20.0

let freezer_radius          = 120
let freezer_cooldown        = 120.0
let freezer_slow_factor     = 0.3

let soldier_attack_cooldown        = 60.0
let soldier_frozen_attack_cooldown = 120.0

let projectile_life = 120.0