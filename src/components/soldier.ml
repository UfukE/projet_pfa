open Ecs
open Component_defs
open System_defs

let ally_soldiers : soldier list ref = ref []
let ally_owner : (int, int) Hashtbl.t = Hashtbl.create 32
let ally_pool : soldier list ref = ref []
let enemy_pool : soldier list ref = ref []

let retire_ally (e : #attackable) =
  let id = Oo.id e in
  Hashtbl.remove ally_owner id;
  ally_soldiers := List.filter (fun ally -> Oo.id ally <> id) !ally_soldiers;
  e#tag#set No_tag;
  e#velocity#set Vector.zero;
  e#timer#set 0.0;
  e#health#set 0.0;
  e#position#set Cst.inactive_pos

let prune_allies () =
  ally_soldiers := List.filter (fun ally -> ally#tag#get = Ally) !ally_soldiers

let get_enemy_velocity pos =
  let speed = 0.2 +. Random.float 2.5 in
  Vector.mult speed (Cst.safe_direction pos Vector.{x=Cst.core_x; y=Cst.core_y})

let get_ally_velocity pos target_pos =
  Vector.mult Cst.ally_soldier_speed (Cst.safe_direction pos target_pos)

let attack_cooldown = function
  | Enemy Freeze -> Cst.soldier_frozen_attack_cooldown
  | Enemy _ -> Cst.soldier_attack_cooldown
  | Ally -> Cst.ally_soldier_attack_cooldown
  | _ -> Cst.soldier_attack_cooldown

let attack_damage = function
  | Ally -> Cst.ally_soldier_damage
  | Enemy _ -> Cst.enemy_soldier_damage
  | _ -> Cst.enemy_soldier_damage

let target_name = function
  | Core -> "Core"
  | Component_defs.Wall -> "Wall"
  | Tower Cst.Archer -> "Archer tower"
  | Tower (Cst.Bomber _) -> "Bomber tower"
  | Tower Cst.Freezer -> "Freezer tower"
  | Tower (Cst.Laser _) -> "Laser tower"
  | Tower Cst.Spawner -> "Spawner"
  | Ally -> "Ally soldier"
  | Enemy _ -> "Enemy soldier"
  | _ -> "unknown"

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
  (match tg with
  | Enemy _ -> Wave.enemy_spawned ()
  | Ally -> ally_soldiers := e :: !ally_soldiers
  | _ -> ());
  
  e#resolve#set (fun _ tg ->
    let perform_attack () =
      let current_velocity = e#velocity#get in
      if current_velocity = Vector.zero then begin
        if e#timer#get <= 0.0 then begin
          let dmg = attack_damage e#tag#get in
          tg#health#set (tg#health#get -. dmg);
          Gfx.debug "Soldier dealt %.0f damage to %s! HP: %f\n%!" dmg (target_name tg#tag#get) (tg#health#get);
          if tg#health#get <= 0.0 then begin
            match e#tag#get with
            | Enemy _ -> e#velocity#set (get_enemy_velocity e#position#get)
            | Ally -> e#velocity#set Vector.zero
            | _ -> ()
          end;
          e#timer#set (attack_cooldown e#tag#get)
        end else
          e#timer#set (e#timer#get -. 1.0)
      end else
        e#velocity#set Vector.zero
    in
    match e#tag#get, tg#tag#get with
    | Enemy _, ((Tower _) | Core | Component_defs.Wall | Ally) -> perform_attack ()
    | Ally, Enemy _ -> perform_attack ()
    | _ -> ()
  );
  e

let count_allies owner_id =
  prune_allies ();
  List.fold_left (fun acc ally ->
    match Hashtbl.find_opt ally_owner (Oo.id ally) with
    | Some id when id = owner_id -> acc + 1
    | _ -> acc
  ) 0 !ally_soldiers

let clear_allies () =
  List.iter (fun (e : soldier) ->
    Hashtbl.remove ally_owner (Oo.id e);
    e#tag#set No_tag; e#velocity#set Vector.zero;
    e#timer#set 0.0; e#health#set 0.0;
    e#position#set Cst.inactive_pos
  ) !ally_soldiers;
  ally_soldiers := []

let activate_ally (ally : soldier) owner_id pos target_pos =
  ally#tag#set Ally;
  ally#texture#set (Texture.Image (Global.get ()).ally_img);
  ally#position#set pos;
  ally#velocity#set (get_ally_velocity pos target_pos);
  ally#health#set Cst.ally_soldier_health;
  ally#timer#set 0.0;
  Hashtbl.replace ally_owner (Oo.id ally) owner_id;
  ally_soldiers := ally :: !ally_soldiers

let spawn_pos_from_center spawner_center dir =
  let offset = Vector.mult (float Cst.bs +. 2.0) dir in
  let spawn_center = Vector.add spawner_center offset in
  Vector.{
    x = spawn_center.x -. float Cst.bs /. 2.0;
    y = spawn_center.y -. float Cst.bs /. 2.0;
  }

let diagonal_dir x y = Cst.safe_direction Vector.zero Vector.{ x; y }

let rec first_valid_spawn spawner_center = function
  | [] -> None
  | dir :: rest ->
    let pos = spawn_pos_from_center spawner_center dir in
    if Building.can_place pos.x pos.y then Some pos
    else first_valid_spawn spawner_center rest

let add_ally owner_id spawner_center target_pos =
  let preferred_dir = Cst.safe_direction spawner_center target_pos in
  let preferred_dir =
    if Vector.is_zero preferred_dir then Cst.direction_vector Cst.East else preferred_dir
  in
  let candidate_dirs = [
    preferred_dir;
    Cst.direction_vector Cst.East;
    Cst.direction_vector Cst.South;
    Cst.direction_vector Cst.West;
    Cst.direction_vector Cst.North;
    diagonal_dir 1.0 1.0;
    diagonal_dir 1.0 (-1.0);
    diagonal_dir (-1.0) 1.0;
    diagonal_dir (-1.0) (-1.0);
  ] in
  match first_valid_spawn spawner_center candidate_dirs with
  | None -> false
  | Some pos ->
    let ally =
      match List.find_opt (fun a -> a#tag#get = No_tag) !ally_pool with
      | Some a -> a
      | None ->
        let Vector.{ x; y } = pos in
        let a = create_s
          x
          y
          Vector.zero
          Cst.ally_soldier_health
          (Texture.Image (Global.get ()).ally_img)
          Cst.bs Cst.bs
          Ally
        in
        ally_pool := a :: !ally_pool;
        a
    in
    activate_ally ally owner_id pos target_pos;
    true

let () =
  Wave.ally_count_fn := count_allies;
  Wave.spawn_ally_fn := (fun owner_id pos target_pos ->
    add_ally owner_id pos target_pos
  )

let activate_enemy (e : soldier) x y =
  let v = get_enemy_velocity Vector.{ x; y } in
  e#tag#set (Enemy Normal);
  e#texture#set (Texture.Image (Global.get ()).enemy_img);
  e#position#set Vector.{ x; y };
  e#velocity#set v;
  e#health#set Cst.enemy_soldier_health;
  e#timer#set 0.0;
  Wave.enemy_spawned ()

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
  let e =
    match List.find_opt (fun e -> e#tag#get = No_tag) !enemy_pool with
    | Some e -> e
    | None ->
      let img = Texture.Image (Global.get ()).enemy_img in
      let a = create_s x y Vector.zero Cst.enemy_soldier_health img Cst.bs Cst.bs No_tag in
      enemy_pool := a :: !enemy_pool;
      a
  in
  activate_enemy e x y;
  e