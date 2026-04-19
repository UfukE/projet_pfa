open Ecs
open Component_defs
open System_defs

let get_v pos =
  pos
  |> Vector.sub (Vector.{x=Cst.core_x; y=Cst.core_y}) 
  |> Vector.normalize
  |> Vector.mult (0.2 +. Random.float 2.5)

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
  Attack_system.(register (e:>t));
  Wave.enemy_spawned ();
  
  e#resolve#set (fun n tg -> 
    
    match tg#tag#get with
    | (Tower _) | Core -> 
      let cv = e#velocity#get in
      if cv = Vector.zero then begin
        if e#timer#get <= 0.0 then begin
          tg#health#set (tg#health#get -. 10.0);
          let target_name = match tg#tag#get with
            | Core -> "Core"
            | Tower Archer -> "Archer tower"
            | Tower (Bomber _) -> "Bomber tower"
            | Tower Freezer -> "Freezer tower"
            | _ -> "unknown"
          in
          Gfx.debug "Soldier dealt 10 damage to %s! HP: %f\n%!" target_name (tg#health#get);
          if tg#health#get <= 0.0 then
            e#velocity#set (get_v e#position#get)
          ;
          let cooldown = match e#tag#get with
            | Enemy Freeze -> Cst.soldier_frozen_attack_cooldown
            | _ -> Cst.soldier_attack_cooldown
          in
          e#timer#set cooldown
        end else
          e#timer#set (e#timer#get -. 1.0)
      end else
        e#velocity#set Vector.zero
    | _ -> ()
  );
  e
let add_random_soldier dir =
  let bs = float Cst.bs in
  let w = float (Cst.window_width-Cst.bs) in
  let x = if dir = 1 then w
  else    if dir = 3 then bs
  else bs +. Random.float (w-.bs) in
  let h = float (Cst.window_height-Cst.bs) in
  let y = if dir = 0 then bs
  else    if dir = 2 then h
  else bs +. Random.float (h-.bs) in
  let v = get_v Vector.{x;y} in
  let img = (Texture.Image (Global.get()).enemy_img) in
  create_s x y v 40.0 img Cst.bs Cst.bs (Enemy Normal)