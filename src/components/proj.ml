open Ecs
open Component_defs
open System_defs

let pool_size = 256

let pool : projectile array option ref = ref None
let rr_idx = ref 0

let deactivate (p : projectile) =
  p#tag#set No_tag;
  p#velocity#set Vector.zero;
  p#timer#set 0.0;
  p#position#set Cst.inactive_pos

let init_projectile (p : projectile) =
  p#box#set Rect.{ width = 6; height = 6 };
  p#texture#set (Texture.Color (Cst.green 255));
  deactivate p;
  p#resolve#set (fun _n target ->
    if p#tag#get = Projectile then
      match target#tag#get with
      | Enemy _ ->
        let dmg = p#health#get in
        target#health#set (target#health#get -. dmg);
        Gfx.debug "Archer dealt %.0f damage! Enemy HP: %.1f\n%!" dmg (target#health#get);
        deactivate p
      | _ -> ()
  );
  Move_system.(register (p :> t));
  Draw_system.(register (p :> t));
  Collision_system.(register (p :> t))

let get_pool () =
  match !pool with
  | Some p -> p
  | None ->
    let p = Array.init pool_size (fun _ -> new projectile ()) in
    Array.iter init_projectile p;
    pool := Some p;
    p

let take_slot p =
  let n = Array.length p in
  let rec find i =
    if i >= n then None
    else
      let idx = (!rr_idx + i) mod n in
      if p.(idx)#tag#get <> Projectile then Some idx else find (i + 1)
  in
  let idx =
    match find 0 with
    | Some i -> i
    | None -> !rr_idx
  in
  rr_idx := (idx + 1) mod n;
  p.(idx)

let create pos dir damage =
  let p = get_pool () |> take_slot in
  p#tag#set Projectile;
  p#health#set (float damage);
  p#position#set pos;
  p#velocity#set (Vector.mult 3.5 dir);
  p#timer#set Cst.projectile_life;
  p
