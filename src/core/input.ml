let key_table = Hashtbl.create 16
let has_key s = Hashtbl.mem key_table s
let set_key s= Hashtbl.replace key_table s ()
let unset_key s = Hashtbl.remove key_table s

let action_table = Hashtbl.create 16
let register key action = Hashtbl.replace action_table key action

let on_left_click x y =
  let g = Global.get () in
  match g.mode with
  | Build (Some bld) ->
    let cost = Cst.building_cost bld in
    if g.money >= cost then
      let _ = match bld with
      | Archer  -> Building.add_archer  x y
      | Bomber  -> Building.add_bomber  x y
      | Freezer -> Building.add_freezer x y
      in
      Global.set {g with money=g.money-cost}
  | _ -> ()

let handle_input () =
  let () =
    match Gfx.poll_event () with
      KeyDown s -> set_key s
    | KeyUp s -> unset_key s
    | Quit -> exit 0
    | MouseMove (x, y) -> 
      let g = Global.get () in 
      Global.set {g with mouse_pos = (x,y);}
    | MouseButton (0, true, x, y) -> on_left_click (float (x-10)) (float (y-10))
    | _ -> ()
  in
  Hashtbl.iter (fun key action ->
      if has_key key then action ()) action_table

let set_building b =
  let g = Global.get () in
  match b with
  | None -> Global.set {g with mode=Build None}
  | Some(bld) when g.money >= Cst.building_cost bld -> begin
      match g.mode with
      | Defense -> ()
      | Build _ -> Global.set {g with mode= Build(b)}
  end
  | _ -> ()

let start_wave () =
  let g = Global.get () in
  match g.mode with
  | Build _ -> Global.set {g with mode=Defense; wave=g.wave+1}
  | Defense -> ()
  

let () =
  register "1" (fun () -> set_building (Some Cst.Archer));
  register "2" (fun () -> set_building (Some Cst.Bomber));
  register "3" (fun () -> set_building (Some Cst.Freezer));
  register "0" (fun () -> set_building None);
  register "space" start_wave; (* SDL *)
  register " " start_wave;     (* JS *)