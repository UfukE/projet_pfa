(*open Component_defs*)

type mode = Build of Cst.buildings option | Defense 

type t = {
  window : Gfx.window;
  ctx : Gfx.context;
  mutable money : int;
  mutable wave : int;
  mutable mode: mode;
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
