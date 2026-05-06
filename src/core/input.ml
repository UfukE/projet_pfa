let action_table = Hashtbl.create 16
let register key action = Hashtbl.replace action_table key action

let on_left_click x y =
  let g = Global.get () in
  match g.mode with
  | Build (Some bld) ->
    let cost = Cst.building_cost bld in
    if g.money >= cost && Building.can_place x y then
      let _ = match bld with
      | Cst.Archer -> Building.add_archer x y
      | Cst.Bomber _ -> Building.add_bomber x y
      | Cst.Freezer -> Building.add_freezer x y
      | Cst.Wall -> Building.add_wall x y
      | Cst.Laser dir -> Building.add_laser x y dir
      | Cst.Spawner -> Building.add_spawner x y
      in
      Global.set {g with money=g.money-cost}
  | _ -> ()

let on_right_click x y = Building.set_bomber_target x y

let handle_input () =
  match Gfx.poll_event () with
    KeyDown s -> ()
  | KeyUp s -> (match Hashtbl.find_opt action_table s with
    |None -> ()
    |Some f -> f ()
  )
  | Quit -> exit 0
  | MouseMove (x, y) -> 
    let g = Global.get () in 
    Global.set {g with mouse_pos = (x,y);}
  | MouseButton (0, true, x, y) (*JS*)
  | MouseButton (1, true, x, y) (*SDL*)
    -> on_left_click (float (x-Cst.bs/2)) (float (y-Cst.bs/2))
  | MouseButton (2, true, x, y) -> on_right_click (float (x-Cst.bs/2)) (float (y-Cst.bs/2))
  | _ -> ()

let set_building b =
  let g = Global.get () in
  match g.mode, b with
  | Defense, _ -> ()
  | Build (Some (Cst.Laser dir)), Some (Cst.Laser _) ->
    Global.set {g with mode = Build (Some (Cst.Laser (Cst.next_direction dir)))}
  | _, None -> Global.set {g with mode=Build None}
  | _, Some(bld) when g.money >= Cst.building_cost bld ->
    Global.set {g with mode=Build (Some bld)}
  | _ -> ()

let start_wave () =
  let g = Global.get () in
  match g.mode with
  | Build _ ->
    let wave = g.wave + 1 in
    Global.set {g with mode=Defense; wave};
    Wave.start wave
  | Defense -> ()
  

let () =
  register "1" (fun () -> set_building (Some Cst.Archer));
  register "2" (fun () -> set_building (Some (Cst.Bomber Vector.zero)));
  register "3" (fun () -> set_building (Some Cst.Freezer));
  register "4" (fun () -> set_building (Some Cst.Wall));
  register "5" (fun () -> set_building (Some (Cst.Laser Cst.North)));
  register "6" (fun () -> set_building (Some Cst.Spawner));
  register "0" (fun () -> set_building None);
  register "space" start_wave; (* SDL *)
  register " " start_wave;     (* JS *)
  (* SDL / numpad *)
  register "keypad 1" (fun () -> set_building (Some Cst.Archer));
  register "keypad 2" (fun () -> set_building (Some (Cst.Bomber Vector.zero)));
  register "keypad 3" (fun () -> set_building (Some Cst.Freezer));
  register "keypad 4" (fun () -> set_building (Some Cst.Wall));
  register "keypad 5" (fun () -> set_building (Some (Cst.Laser Cst.North)));
  register "keypad 6" (fun () -> set_building (Some Cst.Spawner));
  register "keypad 0" (fun () -> set_building None);