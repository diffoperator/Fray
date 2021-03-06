﻿namespace TracerPrimitives
open System
open Vector
open Color

open Microsoft.FSharp.Collections

type Ray = {start: Vector; dir: Vector}

[<AbstractClass>]
type SceneObject =
    val ObjectSurface : Surface
    new(surface) = { ObjectSurface = surface;}
    //This takes the intersecting ray and returns the distance from the eye of the ray to the point of intersection
    abstract member Intersect: Ray -> ISect
    //This takes the vector that represents the point of intersection and returns the normal
    abstract member Normal: Vector -> Vector    

and ISect = {Thing: SceneObject; Ray : Ray; Dist: double}
    
and Surface = {Diffuse: (Vector -> Color); 
                Specular: (Vector -> Color); 
                Reflect: (Vector -> float);
                DefaultColor: Color; 
                Roughness : Double;
                RefractiveIndex : Double} 

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