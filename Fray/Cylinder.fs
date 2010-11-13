module Cylinder

open System
open TracerPrimitives
open Vector
open Color

type Cylinder (radius : float, origin : Vector, axis : Vector, surface) =
    inherit SceneObject(surface)
    let mutable Radius = radius
    let mutable Axis = axis
    let mutable Origin = origin

    //Cylinder is of radius r and oriented along line origin + axis * t
    override this.Intersect ray =
        (*let delP = Vector.subVector ray.start Origin
        let intermediateA = Vector.subVector ray.dir (Vector.scalarMult (Vector.dot ray.dir Axis) Axis)
        let a = Vector.dot intermediateA intermediateA
        let intermediateB = Vector.subVector delP (Vector.scalarMult (Vector.dot delP Axis) Axis)
        let b = 2.0 * (Vector.dot intermediateA intermediateB)
        let c = (Vector.dot intermediateB intermediateB) - Math.Pow(Radius, 2.0)
        let disc = b*b - 4.0 * a * c
        let mutable dist = if disc > 0.0 then
                                let discf = Math.Sqrt(disc)
                                Math.Min ((-b-discf)/2.0 * a, (-b+discf)/2.0 * a)
                            else 0.0*)
        let a = Math.Pow(ray.dir.i , 2.0) + Math.Pow(ray.dir.k , 2.0)
        let b = 2.0 * (ray.dir.i * (ray.start.i) + ray.dir.k * (ray.start.k)) 
        let c = Math.Pow(ray.start.i , 2.0) + Math.Pow(ray.start.k , 2.0) - 0.25
        let disc = b*b - 4.0 * a * c
        let mutable dist = if disc > 0.0 then
                                let discf = Math.Sqrt(disc)
                                Math.Min ((-b-discf)/2.0 * a, (-b+discf)/2.0 * a)
                            else 0.0
        if ray.start.j + dist * ray.dir.j < -0.5 then dist <- 0.0            
        {Thing = this; Ray = ray; Dist = dist}

    override this.Normal pos =
       ({i = (pos.i)/0.5; j = 0.0; k = pos.k/0.5})