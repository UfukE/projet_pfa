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
