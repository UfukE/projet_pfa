open System_defs
open Component_defs
open Ecs


let init dt =
  Wave.spawn_fn := (fun dir -> ignore (Soldier.add_random_soldier dir));
  Wave.cleanup_fn := Soldier.clear_allies;
  let _ = Building.add_core () in
  Ecs.System.init_all dt;
  Some ()


let update dt =
  let () = Input.handle_input () in  
  Collision_system.update dt;
  Move_system.update dt;
  Attack_system.update dt;
  Draw_system.update dt;
  let g = Global.get () in
  (match g.mode with Defense -> Wave.update () | _ -> ());
  None

let (let@) f k = f k


let run fn =
  let window_spec = 
    Format.sprintf "game_canvas:%dx%d:"
      Cst.window_width Cst.window_height
  in
  let window = Gfx.create  window_spec in
  let ctx = Gfx.get_context window in

  let grass_res = Gfx.load_image ctx "resources/images/grass.png" in
  let@ grass = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt grass_res) in
  let archer_res = Gfx.load_image ctx "resources/images/archer.png" in
  let@ archer_img = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt archer_res) in
  let bomber_res = Gfx.load_image ctx "resources/images/bomber.png" in
  let@ bomber_img = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt bomber_res) in
  let freezer_res = Gfx.load_image ctx "resources/images/freezer.png" in
  let@ freezer_img = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt freezer_res) in
  let wall_res = Gfx.load_image ctx "resources/images/wall.png" in
  let@ wall_img = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt wall_res) in
  let laser_res = Gfx.load_image ctx "resources/images/laser.png" in
  let@ laser_img = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt laser_res) in
  let spawn_res = Gfx.load_image ctx "resources/images/spawn.png" in
  let@ spawn_img = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt spawn_res) in
  let ally_res = Gfx.load_image ctx "resources/images/ally.png" in
  let@ ally_img = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt ally_res) in
  let core_res = Gfx.load_image ctx "resources/images/core.png" in
  let@ core_img = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt core_res) in
  let enemy_res = Gfx.load_image ctx "resources/images/enemy.png" in
  let@ enemy_img = Gfx.main_loop (fun _dt -> Gfx.get_resource_opt enemy_res) in
  
  let global = Global.{ 
    window; 
    ctx;
    font= Gfx.load_font fn "" 16;
    money = Cst.starting_money;
    wave=0; 
    mode=Build(None);
    mouse_pos=(0,0);
    grass;
    archer_img;
    bomber_img;
    freezer_img;
    wall_img;
    laser_img;
    spawn_img;
    ally_img;
    core_img;
    enemy_img;
  } in
  Global.set global;
  let@ () = Gfx.main_loop ~limit:false init in
  let@ () = Gfx.main_loop update in ()








