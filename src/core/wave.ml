let pending      = ref 0   (* enemies left to spawn *)
let alive        = ref 0   (* enemies currently alive *)
let spawn_timer  = ref 0.0 (* frames until next spawn *)
let spawn_interval = 60.0  (* frames between spawns *)

let spawn_fn : (int -> unit) ref = ref (fun _ -> ())

let enemy_spawned () = incr alive
let enemy_died    () = decr alive

let start wave_num =
  pending     := 5 + wave_num * 3;
  alive       := 0;
  spawn_timer := 0.0

let update () =
  if !pending > 0 then begin
    if !spawn_timer <= 0.0 then begin
      let dir = Random.int 4 in
      !spawn_fn dir;
      decr pending;
      spawn_timer := spawn_interval
    end else
      spawn_timer := !spawn_timer -. 1.0
  end else if !alive = 0 then begin
    let g = Global.get () in
    match g.mode with
    | Defense ->
      Global.set { g with mode = Build None }
    | _ -> ()
  end
