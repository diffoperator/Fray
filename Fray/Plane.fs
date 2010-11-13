module Plane

open System
open TracerPrimitives
open Vector
open Color

type Plane (norm: Vector, offset: float, surface) =
    inherit SceneObject(surface)
    let mutable Norm = norm
    let mutable Offset = offset

    override this.Intersect ray =
        let denom = Vector.dot Norm ray.dir
        {Thing = this; Ray = ray; Dist = ((Vector.dot Norm ray.start) + Offset) / (-denom)}

    override this.Normal pos =
        Norm