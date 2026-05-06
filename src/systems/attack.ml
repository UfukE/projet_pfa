open Ecs
open Component_defs

type t = attackable

let init _ = ()

let get_enemies = Seq.filter (fun (e : t) -> match e#tag#get with Enemy _ -> true | _ -> false)

let center_of (e : t) = Cst.center_of e#position#get e#box#get

let safe_direction a b = Cst.safe_direction a b

let find_nearest_enemy center enemies =
  Seq.fold_left (fun acc enemy ->
    let d = Vector.norm (Vector.sub (center_of enemy) center) in
    match acc with
    | None -> Some (enemy, d)
    | Some (_, best_d) when d < best_d -> Some (enemy, d)
    | _ -> acc
  ) None enemies

let find_enemy_in_rect zone_pos zone_box enemies =
  Seq.fold_left (fun acc enemy ->
    if Rect.intersect zone_pos zone_box enemy#position#get enemy#box#get then
      let d = Vector.norm (Vector.sub (center_of enemy) (Cst.center_of zone_pos zone_box)) in
      match acc with
      | None -> Some (enemy, d)
      | Some (_, best_d) when d < best_d -> Some (enemy, d)
      | _ -> acc
    else acc
  ) None enemies

let has_enemy_contact ally enemies =
  Seq.fold_left (fun acc enemy ->
    acc || Rect.intersect ally#position#get ally#box#get enemy#position#get enemy#box#get
  ) false enemies

let update _dt el =
  Seq.iter (fun (e:t) -> 
    match e#tag#get with
    | No_tag -> ()
    | Ally when e#health#get <= 0.0 ->
      e#tag#set No_tag;
      e#velocity#set Vector.zero;
      e#timer#set 0.0;
      e#health#set 0.0;
      e#position#set Cst.inactive_pos
    | _ when e#health#get <= 0.0 -> begin
      match e#tag#get with
      | Enemy _ ->
        Wave.enemy_died ();
        let g = Global.get () in
        Global.set { g with money = g.money + Cst.soldier_kill_reward };
        e#tag#set No_tag;
        e#velocity#set Vector.zero;
        e#health#set 0.0;
        e#timer#set 0.0;
        e#position#set Cst.inactive_pos
      | Core -> Gfx.debug "GAME OVER!!\n%!"; exit 0
      | _ -> Entity.delete e
    end
    | _ -> ()
  ) el;

  Seq.iter (fun (e:t) -> match e#tag#get with
  | Tower Cst.Archer -> begin
    let center = center_of e in
    let nearest =
      match find_nearest_enemy center (get_enemies el) with
      | Some (enemy, d) when d <= Cst.archer_radius -> Some (enemy, d)
      | _ -> None
    in
    match nearest with
    | None -> ()
    | Some (enemy, _) ->
      if e#timer#get <= 0.0 then begin
        let dir = safe_direction center (center_of enemy) in
        if not (Vector.is_zero dir) then begin
          let _ = Proj.create center dir Cst.archer_damage in
          e#timer#set Cst.archer_cooldown
        end
      end else
        e#timer#set (e#timer#get -. 1.0)
    ;
  end
  | Tower (Cst.Bomber tgt) -> begin 
    if e#timer#get <= 0.0 then begin
      let aoe_pos = Vector.{
        x = tgt.x -. (float Cst.bomber_radius);
        y = tgt.y -. (float Cst.bomber_radius) }
      in
      let aoe_box = Rect.{width = Cst.bomber_radius * 2; height = Cst.bomber_radius * 2} in
      Seq.iter (fun enemy ->
        if Rect.intersect aoe_pos aoe_box enemy#position#get enemy#box#get then begin
          enemy#health#set (enemy#health#get -. Cst.bomber_damage);
          Gfx.debug "Bomber dealt %.0f damage! Enemy HP: %.1f\n%!" Cst.bomber_damage (enemy#health#get)
        end
      ) (get_enemies el);
      e#timer#set Cst.bomber_cooldown
    end else
      e#timer#set (e#timer#get -. 1.0)
  end
  | Tower Cst.Freezer -> begin
    let pos = e#position#get in
    let aoe_pos = Vector.{
      x = pos.x +. float (Cst.bs/2) -. float Cst.freezer_radius;
      y = pos.y +. float (Cst.bs/2) -. float Cst.freezer_radius } in
    let aoe_box = Rect.{width = Cst.freezer_radius * 2; height = Cst.freezer_radius * 2} in
    Seq.iter (fun enemy ->
      let state =
        if Rect.intersect aoe_pos aoe_box enemy#position#get enemy#box#get
        then Cst.Freeze else Cst.Normal
      in
      enemy#tag#set (Enemy state)
    ) (get_enemies el)
  end
  | Tower (Cst.Laser dir) -> begin
    if e#timer#get <= 0.0 then begin
      let beam_pos, beam_box = Cst.laser_rect e#position#get e#box#get dir in
      Seq.iter (fun enemy ->
        if Rect.intersect beam_pos beam_box enemy#position#get enemy#box#get then begin
          enemy#health#set (enemy#health#get -. Cst.laser_damage);
          Gfx.debug "Laser dealt %.0f damage! Enemy HP: %.1f\n%!" Cst.laser_damage (enemy#health#get)
        end
      ) (get_enemies el);
      e#timer#set Cst.laser_cooldown
    end else
      e#timer#set (e#timer#get -. 1.0)
  end
  | Tower Cst.Spawner -> begin
    let owner_id = Oo.id e in
    if (!Wave.ally_count_fn) owner_id < Cst.spawner_max_allies then begin
      if e#timer#get <= 0.0 then begin
        let zone_pos, zone_box = Cst.spawner_rect e#position#get e#box#get in
        match find_enemy_in_rect zone_pos zone_box (get_enemies el) with
        | Some (enemy, _) ->
          let center = center_of e in
          if (!Wave.spawn_ally_fn) owner_id center (center_of enemy) then
            e#timer#set Cst.spawner_cooldown
        | None -> ()
      end else
        e#timer#set (e#timer#get -. 1.0)
    end
  end
  | Ally -> begin
    if not (has_enemy_contact e (get_enemies el)) then begin
      match find_nearest_enemy (center_of e) (get_enemies el) with
      | Some (enemy, _) ->
        let dir = safe_direction (center_of e) (center_of enemy) in
        e#velocity#set (Vector.mult Cst.ally_soldier_speed dir)
      | None -> e#velocity#set Vector.zero
    end
  end
  | Enemy _ -> () (*in soldier.ml*)
  | Component_defs.Wall -> ()
  | Core -> ()
  | _ -> ()
  ) el;