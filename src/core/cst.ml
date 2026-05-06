let window_width = 1200
let window_height = 700

let black   = Gfx.color 0 0 0 255
let white   = Gfx.color 255 255 255 255
let red a   = Gfx.color 255 0 0 a
let green a = Gfx.color 0 255 0 a
let blue a  = Gfx.color 0 0 255 a
let gray a  = Gfx.color 120 120 120 a
let orange a = Gfx.color 225 145 40 a
let gold    = Gfx.color 239 191 4 255
let transparent = Gfx.color 0 0 0 0


let core_x = float (window_width/2)
let core_y = float (window_height/2)

type direction = North | East | South | West

type buildings =
  | Archer
  | Bomber of Vector.t
  | Freezer
  | Wall
  | Laser of direction
  | Spawner

type state = Normal | Freeze

let bs = 40 (*building size*)

let starting_money = 80
let soldier_kill_reward = 8

let archer_cost = 30
let bomber_cost = 50
let freezer_cost = 40
let wall_cost = 15
let laser_cost = 55
let spawner_cost = 60

let next_direction = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let direction_vector = function
  | North -> Vector.{ x = 0.0; y = -1.0 }
  | East -> Vector.{ x = 1.0; y = 0.0 }
  | South -> Vector.{ x = 0.0; y = 1.0 }
  | West -> Vector.{ x = -1.0; y = 0.0 }


let building_cost = function
  | Archer    -> archer_cost
  | Bomber _  -> bomber_cost
  | Freezer   -> freezer_cost
  | Wall      -> wall_cost
  | Laser _   -> laser_cost
  | Spawner   -> spawner_cost

let building_color a = function
  | Archer    -> green a
  | Bomber _  -> red  a
  | Freezer   -> blue a
  | Wall      -> gray a
  | Laser _   -> red  a
  | Spawner   -> orange a

let wall_health = 180.0
let laser_health = 100.0
let spawner_health = 110.0

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

let laser_length        = 280
let laser_thickness     = 14
let laser_damage        = 25.0
let laser_cooldown      = 100.0
let laser_atk_duration  = 8.0

let spawner_range_w        = 180
let spawner_range_h        = 120
let spawner_cooldown       = 90.0
let spawner_max_allies     = 4

let enemy_soldier_health = 40.0
let enemy_soldier_damage = 10.0

let ally_soldier_health = 28.0
let ally_soldier_damage = 12.0
let ally_soldier_speed = 2.2
let ally_soldier_attack_cooldown = 40.0

let soldier_attack_cooldown        = 60.0
let soldier_frozen_attack_cooldown = 120.0

let projectile_life = 120.0

let inactive_pos = Vector.{ x = -1000.0; y = -1000.0 }

let safe_direction from_pos to_pos =
  let delta = Vector.sub to_pos from_pos in
  if Vector.is_zero delta then Vector.zero else Vector.normalize delta

let center_of pos box =
  let Vector.{ x; y } = pos in
  let Rect.{ width; height } = box in
  Vector.{
    x = x +. float width /. 2.0;
    y = y +. float height /. 2.0;
  }

let laser_rect pos box dir =
  let center = center_of pos box in
  let thickness_f = float laser_thickness in
  let length_f = float laser_length in
  match dir with
  | North ->
    Vector.{ x = center.x -. thickness_f /. 2.0; y = center.y -. length_f },
    Rect.{ width = laser_thickness; height = laser_length }
  | East ->
    Vector.{ x = center.x; y = center.y -. thickness_f /. 2.0 },
    Rect.{ width = laser_length; height = laser_thickness }
  | South ->
    Vector.{ x = center.x -. thickness_f /. 2.0; y = center.y },
    Rect.{ width = laser_thickness; height = laser_length }
  | West ->
    Vector.{ x = center.x -. length_f; y = center.y -. thickness_f /. 2.0 },
    Rect.{ width = laser_length; height = laser_thickness }

let spawner_rect pos box =
  let center = center_of pos box in
  Vector.{
    x = center.x -. float spawner_range_w;
    y = center.y -. float spawner_range_h;
  },
  Rect.{ width = spawner_range_w * 2; height = spawner_range_h * 2 }