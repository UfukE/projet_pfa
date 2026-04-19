(*open Component_defs*)

type mode = Build of Cst.buildings option | Defense 

type t = {
  window : Gfx.window;
  ctx : Gfx.context;
  font: Gfx.font;
  money : int;
  wave : int;
  mode: mode;
  mouse_pos: int*int;
  grass: Gfx.surface;
  archer_img: Gfx.surface;
  bomber_img: Gfx.surface;
  freezer_img: Gfx.surface;
  core_img: Gfx.surface;
  enemy_img: Gfx.surface;
}

let get, set = 
  let state = ref None in

  let get () : t =
    match !state with
      None -> failwith "Uninitialized global state"
    | Some s -> s
  in

  let set s = state := Some s in
  get, set
