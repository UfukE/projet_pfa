open Ecs
open Component_defs

type t = drawable

let init _ = ()

let fill_box ctx surface pos box color =
  Gfx.set_color ctx color;
  Gfx.fill_rect
    ctx
    surface
    (int_of_float pos.Vector.x)
    (int_of_float pos.Vector.y)
    box.Rect.width
    box.Rect.height

let preview_texture = function
  | Cst.Archer -> Texture.Image (Global.get ()).archer_img
  | Cst.Bomber _ -> Texture.Image (Global.get ()).bomber_img
  | Cst.Freezer -> Texture.Image (Global.get ()).freezer_img
  | Cst.Wall -> Texture.Image (Global.get ()).wall_img
  | Cst.Laser _ -> Texture.Image (Global.get ()).laser_img
  | Cst.Spawner -> Texture.Image (Global.get ()).spawn_img

let draw_preview surface bld =
  let Global.{ctx; mouse_pos; _} = Global.get () in
  let mx, my = mouse_pos in
  let size = Cst.bs in
  let pos = Vector.{ x = float (mx - size / 2); y = float (my - size / 2) } in
  let box = Rect.{ width = size; height = size } in
  Texture.draw ctx surface pos box (preview_texture bld);
  match bld with
  | Cst.Laser dir ->
    let beam_pos, beam_box = Cst.laser_rect pos box dir in
    fill_box ctx surface beam_pos beam_box (Gfx.color 255 0 0 60)
  | _ -> ()

let draw_ui surface ww wh =
  (*bottom panel ui*)
  let Global.{ctx; money; wave; font} = Global.get () in
  let panel_h = 140 in
  let shop_row_1 =
    Printf.sprintf
      "[1] Archer (%d$)  [2] Bomber (%d$)  [3] Freezer (%d$)  [0] Clear"
      Cst.archer_cost
      Cst.bomber_cost
      Cst.freezer_cost
  in
  let shop_row_2 =
    Printf.sprintf
      "[4] Wall (%d$)  [5] Laser (%d$)  [6] Spawner (%d$)"
      Cst.wall_cost
      Cst.laser_cost
      Cst.spawner_cost
  in
  Gfx.set_color ctx (Gfx.color 235 233 224 255);
  Gfx.fill_rect ctx surface 0 (wh - panel_h) ww panel_h;
  Gfx.set_color ctx (Gfx.color 80 80 80 255);
  Gfx.fill_rect ctx surface 0 (wh - panel_h) ww 2;

  Gfx.set_color ctx (Gfx.color 0 0 0 255);
  let money_s = Gfx.render_text ctx (Printf.sprintf "Money: $%d" money) font in
  let wave_s = Gfx.render_text ctx (Printf.sprintf "Wave: %d" wave) font in
  let shop_1_s = Gfx.render_text ctx shop_row_1 font in
  let shop_2_s = Gfx.render_text ctx shop_row_2 font in
  let start_s = Gfx.render_text ctx "[5] Rotate selected laser  [SPACE] Start next wave" font in

  Gfx.blit ctx surface money_s 12 (wh - panel_h + 10);
  Gfx.blit ctx surface wave_s 210 (wh - panel_h + 10);
  Gfx.blit ctx surface shop_1_s 12 (wh - panel_h + 40);
  Gfx.blit ctx surface shop_2_s 12 (wh - panel_h + 68);
  Gfx.blit ctx surface start_s 12 (wh - panel_h + 98)

let update _dt el =
  let Global.{window; ctx; mode; grass; _} = Global.get () in
  let surface = Gfx.get_surface window in
  let ww, wh = Gfx.get_context_logical_size ctx in
  let tile = 40 in
  let cols = (ww + tile - 1) / tile in
  let rows = (wh + tile - 1) / tile in
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      Gfx.blit_scale ctx surface grass (col * tile) (row * tile) tile tile
    done
  done;
  
  Seq.iter (fun (e:t) ->
      match e#tag#get with
      | No_tag -> ()
      | tag ->
        let pos = e#position#get in
        let box = e#box#get in
        let txt = e#texture#get in
        Texture.draw ctx surface pos box txt;
        match tag with
      | Tower (Cst.Bomber tgt) 
      when e#timer#get +. Cst.bomber_atk_duration >= Cst.bomber_cooldown ->
        let r = Cst.bomber_radius in
        let tx = int_of_float tgt.x in
        let ty = int_of_float tgt.y in
        Gfx.set_color ctx (Gfx.color 255 0 0 80);
        Gfx.fill_rect ctx surface (tx - r) (ty - r) (r * 2) (r * 2)
      | Tower Cst.Freezer ->
        let pos = e#position#get in
        let r = Cst.freezer_radius in
        let cx = int_of_float pos.Vector.x + Cst.bs / 2 in
        let cy = int_of_float pos.Vector.y + Cst.bs / 2 in
        Gfx.set_color ctx (Gfx.color 100 180 255 50);
        Gfx.fill_rect ctx surface (cx - r) (cy - r) (r * 2) (r * 2)
      | Tower (Cst.Laser dir)
      when e#timer#get +. Cst.laser_atk_duration >= Cst.laser_cooldown ->
        let beam_pos, beam_box = Cst.laser_rect pos box dir in
        fill_box ctx surface beam_pos beam_box (Gfx.color 255 0 0 80)
      | Projectile -> 
        let t = e#timer#get -. 1.0 in
        if t <= 0.0 then begin
          e#tag#set No_tag;
          e#timer#set 0.0;
          e#position#set Cst.inactive_pos
        end
        else e#timer#set t
      | _ -> ()
    ) el;

  (match mode with
  | Build (Some bld) -> 
    draw_preview surface bld;
    draw_ui surface ww wh
  | Build _ -> draw_ui surface ww wh;
  | _ -> ());

  Gfx.commit ctx