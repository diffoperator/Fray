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
        let a = Vector.dot ray.dir ray.dir
        let b = 2.0 * Vector.dot ray.dir (Vector.subVector ray.start Center)
        let c = (Vector.dot (Vector.subVector ray.start Center) (Vector.subVector ray.start Center)) - Math.Pow(Radius, 2.0)
        let disc = Math.Sqrt (b*b - 4.0 * a * c)
        let dist = if disc > 0.0 then 
                            Math.Min ((- b - disc)/2.0 * a, (- b + disc)/2.0 * a)
                           else 0.0
        {Thing = this; Ray = ray; Dist = dist}
     
    override this.Normal pos =
        Vector.norm (Vector.subVector pos Center)

