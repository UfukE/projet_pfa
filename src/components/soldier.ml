open Ecs
open Component_defs
open System_defs

type tag += Enemy | Ally

let create_s x y v hp txt width height tg =
  let e = new soldier () in
  e#tag#set tg;
  e#texture#set txt;
  e#position#set Vector.{x=x;y=y};
  e#box#set Rect.{width;height};
  e#velocity#set v;
  e#health#set hp;
  Collision_system.(register (e:>t));
  Move_system.(register (e:>t));
  Draw_system.(register (e:>t));
  
  e#resolve#set (fun n tg -> 
    
    match tg#tag#get with
    | Building.Tower | Building.Core -> 
      let cv = e#velocity#get in
      if cv = Vector.zero then begin
        if e#timer#get <= 0.0 then begin
          tg#health#set (tg#health#get -. 10.0);
          Gfx.debug "Dealt 10 damage! Core HP: %f\n%!" (tg#health#get);
          e#timer#set 60.0
        end else
          e#timer#set (e#timer#get -. 1.0)
      end else
        e#velocity#set Vector.zero
    | _ -> ()
  );
  e
let add_random_soldier dir =
  let w = float (Cst.window_width-20) in
  let x = if dir = 1 then w
  else    if dir = 3 then 20.0
  else 20.0 +. Random.float (w-.20.0) in
  let h = float (Cst.window_height-20) in
  let y = if dir = 0 then 20.0
  else    if dir = 2 then h
  else 20.0 +. Random.float (h-.20.0) in
  let v = Vector.sub 
  (Vector.{x=Cst.core_x; y=Cst.core_y}) 
  (Vector.{x=x;y=y}) in
  let v = Vector.normalize v in
  let v = Vector.mult (0.2 +. Random.float 2.5) v in
  create_s x y v 40.0 Texture.red 20 20 Enemy