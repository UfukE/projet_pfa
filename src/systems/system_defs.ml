
open Ecs

module Draw_system = System.Make(Draw)

module Move_system = System.Make(Move)

module Collision_system = System.Make(Collision)