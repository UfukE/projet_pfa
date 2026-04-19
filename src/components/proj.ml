open Ecs
open Component_defs
open System_defs

let create pos dir damage =
  let p = new projectile () in
  p#tag#set Projectile;
  p#health#set (float damage);
  p#position#set pos;
  p#velocity#set (Vector.mult 3.5 dir);
  p#box#set Rect.{width = 6; height = 6};
  p#texture#set (Texture.Color (Cst.green 255));
  p#timer#set Cst.projectile_life;
  p#resolve#set (fun _n target ->
    match target#tag#get with
    | Enemy _ ->
      let dmg = p#health#get in
      target#health#set (target#health#get -. dmg);
      Gfx.debug "Archer dealt %.0f damage! Enemy HP: %.1f\n%!" dmg (target#health#get);
      Entity.delete (p :> Entity.t)
    | _ -> ()
  );
  Move_system.(register (p :> t));
  Draw_system.(register (p :> t));
  Collision_system.(register (p :> t));
  p
