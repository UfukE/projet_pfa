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
type tag += Core
type tag += Tower | Wall | Projectile of int
type tag += Enemy | Ally

class tagged (t : tag) =
  let r = Component.init t in
  object
    method tag = r
  end

class resolver () =
  let r = Component.init (fun (_ : Vector.t) (_ : tag) -> ()) in
  object
    method resolve = r
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

class type collidable =
  object
    inherit Entity.t
    inherit position
    inherit box
    inherit tagged
    inherit resolver
  end

(*real objects*)

class building (t:tag) =
  object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit texture ()
    inherit tagged t
    inherit resolver ()
  end

class soldier (t:tag) = 
  object 
    inherit Entity.t ()
    inherit position ()
    inherit box()
    inherit texture ()
    inherit velocity ()
    inherit tagged t
    inherit resolver ()
  end

class projectile damage =
  object
    inherit Entity.t ()
    inherit position ()
    inherit box ()
    inherit texture ()
    inherit velocity ()
    inherit tagged (Projectile(damage))
    inherit resolver ()
  end