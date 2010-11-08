module Sphere

open System
open TracerPrimitives
open Vector
open Color

type Sphere (center : Vector, radius : float, surface) =
    inherit SceneObject(surface)
    let mutable Center = center
    let mutable Radius = radius
    
    override this.Intersect ray =
        let eo = Vector.subVector Center ray.start 
        let v = Vector.dot eo ray.dir
        let disc = Math.Pow(Radius, 2.0) - ((Vector.dot eo eo) - Math.Pow(v, 2.0))
        let dist = if disc < 0.0 then 0.0 else v - Math.Sqrt(disc)
        {Thing = this; Ray = ray; Dist = dist}
     
    override this.Normal pos =
        Vector.norm (Vector.subVector pos Center)

