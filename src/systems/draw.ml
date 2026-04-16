open Ecs
open Component_defs


type t = drawable

let init _ = ()

let white = Gfx.color 255 255 255 255
(*
let update _dt el =
  let Global.{window;ctx;_} = Global.get () in
  let surface = Gfx.get_surface window in
  let ww, wh = Gfx.get_context_logical_size ctx in
  Gfx.set_color ctx white;
  Gfx.fill_rect ctx surface 0 0 ww wh;
  Seq.iter (fun (e:t) ->
      let pos = e#position#get in
      let box = e#box#get in
      let txt = e#texture#get in
      Texture.draw ctx surface pos box txt
    ) el;
  Gfx.commit ctx
*)

let preview_color = function
  | Cst.Archer  -> Cst.green 160
  | Cst.Bomber  -> Cst.red  160
  | Cst.Freezer -> Cst.blue 160

let draw_preview surface bld =
  let Global.{ctx; mouse_pos} = Global.get () in
  let mx, my = mouse_pos in
  let size = 20 in
  Gfx.set_color ctx (Cst.building_color 160 bld);
  Gfx.fill_rect ctx surface (mx - size/2) (my - size/2) size size

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
  let Global.{window; ctx; mode} = Global.get () in
  let surface = Gfx.get_surface window in
  let ww, wh = Gfx.get_context_logical_size ctx in
  
  Gfx.set_color ctx white;
  Gfx.fill_rect ctx surface 0 0 ww wh;
  
  Seq.iter (fun (e:t) ->
      let pos = e#position#get in
      let box = e#box#get in
      let txt = e#texture#get in
      Texture.draw ctx surface pos box txt
    ) el;

  (match mode with
  | Build (Some bld) -> 
    draw_preview surface bld;
    draw_ui surface ww wh
  | Build _ -> draw_ui surface ww wh;
  | _ -> ());

  Gfx.commit ctx