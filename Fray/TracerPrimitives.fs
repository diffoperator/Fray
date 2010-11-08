namespace TracerPrimitives
open System
open Vector
open Color

open Microsoft.FSharp.Collections

type Ray = {start: Vector; dir: Vector}

[<AbstractClass>]
type SceneObject =
    val ObjectSurface : Surface
    new(surface) = { ObjectSurface = surface; }
    abstract member Intersect: Ray -> ISect
    abstract member Normal: Vector -> Vector    

and ISect = {Thing: SceneObject; Ray : Ray; Dist: double}
    
and Surface = {Diffuse: (Vector -> Color); 
                Specular: (Vector -> Color); 
                Reflect: (Vector -> float); 
                Roughness : Double} 

type Camera = {pos : Vector; forward: Vector; Up: Vector; Right: Vector}

type Light = {pos : Vector; color : Color}

type Scene =
    val Things : SceneObject seq
    val Lights : Light seq
    val Camera : Camera

    new(things, lights, camera) = { Things = things; Lights = lights; Camera = camera; }

    member this.Intersect ray =
       seq {for thing in this.Things do 
            yield thing.Intersect ray}