open Ecs
open Component_defs

type t = drawable

let init _ = ()

let draw_preview surface bld =
  let Global.{ctx; mouse_pos; archer_img; bomber_img; freezer_img; _} = Global.get () in
  let mx, my = mouse_pos in
  let size = Cst.bs in
  let img = match bld with
    | Cst.Archer  -> archer_img
    | Cst.Bomber _  -> bomber_img
    | Cst.Freezer -> freezer_img
  in
  Gfx.blit_scale ctx surface img (mx - size/2) (my - size/2) size size

let draw_ui surface ww wh =
  (*bottom panel ui*)
  let Global.{ctx; money; wave; font} = Global.get () in
  let panel_h = 110 in
  Gfx.set_color ctx (Gfx.color 235 233 224 255);
  Gfx.fill_rect ctx surface 0 (wh - panel_h) ww panel_h;
  Gfx.set_color ctx (Gfx.color 80 80 80 255);
  Gfx.fill_rect ctx surface 0 (wh - panel_h) ww 2;

  Gfx.set_color ctx (Gfx.color 0 0 0 255);
  let money_s = Gfx.render_text ctx (Printf.sprintf "Money: $%d" money) font in
  let wave_s = Gfx.render_text ctx (Printf.sprintf "Wave: %d" wave) font in
  let shop_s = Gfx.render_text ctx "[1] Archer (20$)  [2] Bomber (40$)  [3] Freezer (30$)  [0] Clear" font in
  let start_s = Gfx.render_text ctx "[SPACE] Start next wave" font in

  Gfx.blit ctx surface money_s 12 (wh - panel_h + 12);
  Gfx.blit ctx surface wave_s 210 (wh - panel_h + 12);
  Gfx.blit ctx surface shop_s 12 (wh - panel_h + 44);
  Gfx.blit ctx surface start_s 12 (wh - panel_h + 74)

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
      let pos = e#position#get in
      let box = e#box#get in
      let txt = e#texture#get in
      Texture.draw ctx surface pos box txt;
      match e#tag#get with
      | Tower (Bomber tgt) 
      when e#timer#get +. Cst.bomber_atk_duration >= Cst.bomber_cooldown ->
        let r = Cst.bomber_radius in
        let tx = int_of_float tgt.x in
        let ty = int_of_float tgt.y in
        Gfx.set_color ctx (Gfx.color 255 0 0 80);
        Gfx.fill_rect ctx surface (tx - r) (ty - r) (r * 2) (r * 2)
      | Tower Freezer ->
        let pos = e#position#get in
        let r = Cst.freezer_radius in
        let cx = int_of_float pos.Vector.x + Cst.bs / 2 in
        let cy = int_of_float pos.Vector.y + Cst.bs / 2 in
        Gfx.set_color ctx (Gfx.color 100 180 255 50);
        Gfx.fill_rect ctx surface (cx - r) (cy - r) (r * 2) (r * 2)
      | Projectile -> 
        let t = e#timer#get -. 1.0 in
        if t <= 0.0 then Entity.delete e
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