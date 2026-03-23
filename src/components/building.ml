open Ecs
open Component_defs
open System_defs

type tag += Core | Tower | Wall

let create x y hp txt width height tg =
  let e = new building () in
  e#tag#set tg;
  e#texture#set txt;
  e#position#set Vector.{x=x;y =y};
  e#box#set Rect.{width;height};
  e#health#set hp;
  Collision_system.(register (e:>t));
  Draw_system.(register (e:>t));
  e


let add_core () = create 
  Cst.core_x
  Cst.core_y
  500.0
  Texture.gold
  20 20
  Core

let add_archer x y = create
  x y
  120.0
  Texture.green
  20 20
  Tower





