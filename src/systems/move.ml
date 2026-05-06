open Ecs
open Component_defs

type t = movable

let init _ = ()

let update _ el =
  Seq.iter (fun (e:t) -> 
    match e#tag#get with
    | No_tag -> ()
    | tag ->
      let pos = e#position#get in
      let vel = e#velocity#get in
      let vel = match tag with
        | Enemy Freeze -> Vector.mult Cst.freezer_slow_factor vel
        | _ -> vel
      in
      let pos' = Vector.add pos vel in
      e#position#set pos'
  ) el