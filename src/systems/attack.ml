open Ecs
open Component_defs

type t = attackable

let init _ = ()

let get_enemies = Seq.filter (fun (e : t) -> match e#tag#get with Enemy _ -> true | _ -> false)

let update _dt el =
  Seq.iter (fun (e:t) -> 
    if e#health#get <= 0.0 then begin
      (match e#tag#get with
      | Enemy _ -> Wave.enemy_died ()
      | Core -> Gfx.debug "GAME OVER!!\n%!"; exit 0
      | _ -> ());
      Entity.delete e
    end
  ) el;

  Seq.iter (fun (e:t) -> match e#tag#get with
  | Tower Archer -> begin
    let pos = e#position#get in
    let center = Vector.{
      x = pos.x +. float Cst.bs /. 2.0;
      y = pos.y +. float Cst.bs /. 2.0 } in
    let nearest = Seq.fold_left (fun acc enemy ->
      let d = Vector.norm (Vector.sub enemy#position#get center) in
      if d <= Cst.archer_radius then
        match acc with
        | None -> Some (enemy, d)
        | Some (_, bd) -> if d < bd then Some (enemy, d) else acc
      else acc
    ) None (get_enemies el)
    in
    match nearest with
    | None -> ()
    | Some (enemy, _) ->
      if e#timer#get <= 0.0 then begin
        let dir = Vector.normalize (Vector.sub enemy#position#get center) in
        let _ = Proj.create center dir Cst.archer_damage in
        e#timer#set Cst.archer_cooldown
      end else
        e#timer#set (e#timer#get -. 1.0)
    ;
  end
  | Tower (Bomber tgt) -> begin 
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
  | Tower Freezer -> begin
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
  | Enemy _ -> () (*in soldier.ml*)
  | _ -> ()
  ) el;