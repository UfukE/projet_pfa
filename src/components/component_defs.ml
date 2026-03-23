open Ecs

class position () =
  let r = Component.init Vector.zero in
  object
    method position = r
  end

class velocity () =
  let r = Component.init Vector.zero in
  object
    method velocity = r
  end

class box () =
  let r = Component.init Rect.{width = 0; height = 0} in
  object
    method box = r
  end

class texture () =
  let r = Component.init (Texture.Color (Gfx.color 0 0 0 255)) in
  object
    method texture = r
  end

type tag = ..
type tag += No_tag
type tag += Projectile of int


class tagged () =
  let r = Component.init No_tag in
  object
    method tag = r
  end

(*class resolver () =
  let r = Component.init (fun (_ : Vector.t) (_ : tagged) -> ()) in
  object
    method resolve = r
  end
*)

class health () =
  let r = Component.init 100.0 in
  object
    method health = r
  end

class timer () =
  let r = Component.init 0.0 in
  object
    method timer = r
  end


(*archetype*)
class type drawable =
  object
    inherit Entity.t
    inherit position
    inherit box
    inherit texture
  end

class type movable =
  object
    inherit Entity.t
    inherit position
    inherit velocity
  end

class type interactable =
  object
    inherit Entity.t
    inherit tagged
    inherit health
    inherit timer
  end

class resolver () =
  let r = Component.init (fun (_ : Vector.t) (_ : interactable) -> ()) in
  object
    method resolve = r
  end

class type collidable =
  object
    inherit interactable
    inherit position
    inherit box
    inherit resolver
  end

(*real objects*)

class building () =
  object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit texture ()
    inherit tagged ()
    inherit resolver ()
    inherit health ()
    inherit timer ()
  end

class soldier () = 
  object 
    inherit Entity.t ()
    inherit position ()
    inherit box()
    inherit texture ()
    inherit velocity ()
    inherit tagged ()
    inherit resolver ()
    inherit health ()
    inherit timer ()
  end

class projectile () =
  object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit texture ()
    inherit velocity ()
    inherit tagged ()
    inherit resolver ()
  end