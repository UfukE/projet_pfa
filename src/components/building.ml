open Ecs
open Component_defs
open System_defs

let buildings = ref []

let can_place x y =
  let p = Vector.{x; y} in
  let b = Rect.{width = Cst.bs; height = Cst.bs} in
  List.exists (fun t ->Rect.intersect p b t#position#get b) !buildings
  |> not

let set_bomber_target =
  let get_bombers () = List.filter (fun t ->
    match t#tag#get with 
    | Tower (Bomber _) -> true
    | _ -> false
    ) !buildings
  in
  let bombers = Hashtbl.create 10 in

  let read_bomber () =
    let rec aux first = function
    | [] -> 
      Hashtbl.clear bombers;
      Hashtbl.add bombers first ();
      first 
    | x::r ->
      if Hashtbl.mem bombers x then 
        aux first r 
      else begin
        Hashtbl.add bombers x ();
        x
      end
    in 
    match get_bombers () with
    | [] -> Hashtbl.clear bombers; None
    | x::r as b -> Some(aux x b)
  in
  fun x y ->
    match read_bomber () with
    |None -> ()
    |Some(next) -> 
      let next = List.find (fun b -> b = next) !buildings in
      next#tag#set (Tower (Cst.Bomber Vector.{x;y}))

let create x y hp txt width height tg =
  let e = new building () in
  e#tag#set tg;
  e#texture#set txt;
  e#position#set Vector.{x=x;y =y};
  e#box#set Rect.{width;height};
  e#health#set hp;
  Collision_system.(register (e:>t));
  Draw_system.(register (e:>t));
  Attack_system.(register (e:>t));
  Entity.register (e :> Entity.t) (fun () -> buildings := List.filter (fun b -> b != e) !buildings);
  buildings := !buildings @ [e];
  e

let add_core () = create 
  Cst.core_x
  Cst.core_y
  500.0
  (Texture.Image (Global.get()).core_img)
  Cst.bs Cst.bs
  Core

let add_archer x y = create
  x y
  120.0
  (Texture.Image (Global.get()).archer_img) 
  Cst.bs Cst.bs
  (Tower Archer)

let add_bomber x y = create
  x y
  150.0
  (Texture.Image (Global.get()).bomber_img)
  Cst.bs Cst.bs
  (Tower (Bomber Vector.{x=Cst.core_x; y=Cst.core_y}))

let add_freezer x y = create
  x y
  100.0
  (Texture.Image (Global.get()).freezer_img)
  Cst.bs Cst.bs
  (Tower Freezer)





