open Ecs
open Component_defs
open System_defs


let create x y hp txt width height t =
  let e = new building t in
  e#texture#set txt;
  e#position#set Vector.{x=float x;y = float y};
  e#box#set Rect.{width;height};
  e#health#set hp;
  (*Collision_system.(register (e:>t));*)
  Draw_system.(register (e:>t));
  e


let core_bld () = create 
  (Cst.window_width/2)
  (Cst.window_height/2)
  500.0
  Texture.gold
  20 20
  Core




