module Raytracer

open System
open System.Collections.Generic
open Vector
open Color
open TracerPrimitives
open Surface
open Sphere
open Plane

open Microsoft.FSharp.Collections

type RayTracer(screenWidth: int, screenHeight: int) =
    let maxDepth = 5
    let ScreenWidth = screenWidth
    let ScreenHeight = screenHeight
    
    let mutable bitmap = new System.Drawing.Bitmap (ScreenWidth, ScreenHeight)

    let createCamera (position: Vector) (lookat: Vector) =
        let forward = Vector.norm (Vector.subVector lookat position)
        let down = {i = 0.0; j = -0.1; k = 0.0}
        let right = Vector.scalarMult 1.5 (Vector.norm (Vector.cross forward down))
        let up = Vector.scalarMult 1.5 (Vector.norm (Vector.cross forward right))
        {pos = position; forward = forward; Up = up; Right = right}

    let intersections (ray: Ray) (scene: Scene) =
        scene.Things |> Seq.map (fun e -> e.Intersect (ray)) 
                        |> Seq.filter (fun e -> e.Dist > 0.0) 
                        |> Seq.sortBy (fun x -> x.Dist)    
    
    let testRay (ray: Ray) (scene: Scene) =
       let isects = intersections ray scene
       if Seq.isEmpty isects then 0.0 else (Seq.head isects).Dist 

    let getIlluminationColor illum e = 
        if illum > 0.0 then 
            Color.scalarMult illum e.color 
        else 
            {r = 0.0; g = 0.0; b = 0.0}
    
    let getSpecularColor specular (roughness: float) (thing: SceneObject) (light: Light) =
        if specular > 0.0 then
            Color.scalarMult (Math.Pow (specular, thing.ObjectSurface.Roughness)) light.color 
        else
            {r = 0.0; g = 0.0; b = 0.0}
    
    //Unpolished code
    let getRefractedRay (thing: SceneObject) (ray: Vector) (normal: Vector) refractiveIndex =
        let cos = Vector.dot ray normal
        let sinsq = System.Math.Pow(1.0/thing.ObjectSurface.RefractiveIndex, 2.0) * (1.0 - Math.Pow (cos, 2.0))
        let refRay = Vector.scalarMult (1.0/ thing.ObjectSurface.RefractiveIndex) ray
        let multiplier = (1.0 / thing.ObjectSurface.RefractiveIndex) * cos + Math.Sqrt (1.0 - sinsq)
        Vector.subVector refRay (Vector.scalarMult multiplier normal)

    let addSpecularDiffuse (thing: SceneObject) (pos: Vector) icolor scolor =
        Color.addColor (Color.multColor (thing.ObjectSurface.Diffuse pos) icolor)
                       (Color.multColor (thing.ObjectSurface.Specular pos) scolor)

    let getNaturalColor (thing: SceneObject) (pos: Vector) (norm: Vector) (rd: Vector) (scene: Scene) =
        scene.Lights |> PSeq.fold (fun acc e -> let ldis = Vector.subVector e.pos pos;
                                                let livec = Vector.norm ldis; 
                                                let neatIsect = testRay {start = pos; dir = livec} scene;
                                                if (neatIsect > (Vector.mag ldis)) || (neatIsect = 0.0) then 
                                                    let illum = Vector.dot livec norm;
                                                    let icolor = getIlluminationColor illum e
                                                    let specular = Vector.dot livec (Vector.norm rd);
                                                    let scolor = getSpecularColor specular thing.ObjectSurface.Roughness thing e
                                                    Color.addColor acc (addSpecularDiffuse thing pos icolor scolor)
                                                else
                                                    acc 
                                                ) {r = 0.0; g = 0.0; b = 0.0}
                                               
    let rec getReflectionColor (thing: SceneObject) (pos: Vector) (rd: Vector) (scene: Scene) depth refractiveIndex =
        Color.scalarMult (thing.ObjectSurface.Reflect pos) (traceRay ({start = pos; dir = rd}) scene (depth + 1) refractiveIndex)
    
    and getRefractionColor (isect: ISect) (normal: Vector) (pos: Vector) (rd: Vector) (scene: Scene) depth refractiveIndex = 
        if isect.Thing.ObjectSurface.RefractiveIndex = 0.0 || depth > maxDepth then
            {r = 0.0; g = 0.0; b = 0.0}
        else
            let n = refractiveIndex / isect.Thing.ObjectSurface.RefractiveIndex
            let cosI = - Vector.dot normal rd
            let cosT2 = 1.0 - Math.Pow (n, 2.0) * (1.0 - Math.Pow (cosI, 2.0))
            if cosT2 < 0.0 then
                {r = 0.0; g = 0.0; b = 0.0}
            else
                let t = Vector.addVector (Vector.scalarMult n rd) (Vector.scalarMult (n * cosI - Math.Sqrt(cosT2)) normal)
                let absorbance = Color.scalarMult (0.15*(-isect.Dist)) isect.Thing.ObjectSurface.DefaultColor
                let transparency = {r = Math.Exp(absorbance.r); g = Math.Exp(absorbance.g); b = Math.Exp(absorbance.b)}
                Color.multColor transparency (traceRay {start = (Vector.addVector pos (Vector.scalarMult 0.001 t)); dir = t} scene (depth + 1) isect.Thing.ObjectSurface.RefractiveIndex)

    and traceRay (ray: Ray) (scene: Scene) depth (refractiveIndex: Double) =
        let isects = intersections ray scene
        if (isects |> Seq.isEmpty) then {r = 0.0; g = 0.0; b= 0.0} else
            shade (isects |> Seq.head) scene depth refractiveIndex

    and shade (isect: ISect) (scene: Scene) (depth: int) refractiveIndex =
        let d = isect.Ray.dir
        let pos = Vector.addVector (Vector.scalarMult isect.Dist isect.Ray.dir) isect.Ray.start
        let normal = isect.Thing.Normal pos
        let reflectDir = Vector.subVector d (Vector.scalarMult (2.0 * (Vector.dot normal d)) normal)
        let refractedDir = getRefractedRay isect.Thing isect.Ray.dir normal refractiveIndex
        
        let reflectColor = getNaturalColor isect.Thing pos normal reflectDir scene 
        let refractColor = getRefractionColor isect normal pos d scene depth refractiveIndex

        let naturalColor = Color.addColor {r = 0.0; g = 0.0; b= 0.0} (Color.addColor reflectColor refractColor)
        if depth > maxDepth then
            Color.addColor naturalColor ({r = 0.5; g = 0.5; b= 0.5})
        else
            let reflectionColor = getReflectionColor (isect.Thing) (Vector.addVector pos (Vector.scalarMult 0.001 reflectDir)) reflectDir scene depth refractiveIndex
            let refractionColor = getRefractionColor isect normal (Vector.addVector pos (Vector.scalarMult 0.001 refractedDir)) refractedDir scene depth refractiveIndex
            Color.addColor naturalColor (Color.addColor reflectionColor refractionColor)

    let defaultScene =
        let (things: SceneObject seq) = seq [(new Plane({i = 0.0; j = 1.0; k = 0.0}, 0.5, Surface.checkerBoard));
                                             (new Sphere({i = 0.25; j = 0.0; k = -2.0}, 1.0, Surface.shiny));
                                             (new Sphere({i = -0.25; j = 0.0; k = 0.0}, 0.5, Surface.shinyseethroughred))]
                                             
        
        let (lights: Light seq) = seq [{pos = {i = -2.0; j = 2.5; k = 0.0}; color = {r = 0.49; g = 0.07; b = 0.07}};
                                       {pos = {i = 1.5; j = 2.5; k = 1.5}; color = {r = 0.07; g = 0.07; b = 0.49}};
                                       {pos = {i = 0.5; j = 1.5; k = -1.5}; color = {r = 0.07; g = 0.49; b = 0.071}};
                                       {pos = {i = 0.0; j = 0.0; k = 1.0}; color = {r = 0.21; g = 0.21; b = 0.35}}] 
        
        let camera = createCamera {i = 0.0; j = 0.0; k = 4.0} {i = 0.0;j = 0.0;k = 0.0}
        new Scene(things, lights, camera)                               

    member x.Render =
        let scene = defaultScene
        [0 .. (ScreenHeight-1)] |> List.iter (fun y ->
                let recenterY = -(float y - ((float screenHeight) / 2.0)) / (2.0 * (float screenHeight));
                [0 .. (ScreenWidth-1)] |> List.iter (fun x ->
                                                let recenterX = (float x - ((float screenWidth) / 2.0)) / (2.0 * (float screenWidth));
                                                let point = Vector.norm (Vector.addVector (scene.Camera.forward) 
                                                                                            (Vector.addVector (Vector.scalarMult recenterX scene.Camera.Right) 
                                                                                                            (Vector.scalarMult recenterY scene.Camera.Up)))
                                                let ray = {start = scene.Camera.pos; dir = point}
                                                let color = Color.toColor (traceRay ray scene 0 1.0)
                                                
                                                bitmap.SetPixel (x, y, color)
                                                )
                                       )
        bitmap
        

    