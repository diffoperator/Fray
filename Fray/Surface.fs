module Surface

open TracerPrimitives
open System
open Vector
open Color

let checkerBoard =
    let diffuse pos = if (Math.Floor (pos.k) + Math.Floor (pos.i)) % 2.0 <> 0.0 then
                        {r = 1.0; g = 0.0; b = 0.0}
                      else
                        {r = 0.0; g = 0.0; b = 0.0}
    let specular pos = {r = 1.0; g = 0.0; b = 1.0}
    let reflect pos = if (Math.Floor (pos.k) + Math.Floor (pos.i)) % 2.0 <> 0.0 then 0.1 else 0.7
    let roughness = 150.0
    {Diffuse = diffuse; Specular = specular; Reflect = reflect; Roughness = roughness}
    
let shiny =
    let diffuse pos = {r = 1.0; g = 1.0; b = 1.0}
    let specular pos = {r = 0.5; g = 0.5; b = 0.5}
    let reflect pos = 0.6
    let roughness = 50.0
    {Diffuse = diffuse; Specular = specular; Reflect = reflect; Roughness = roughness}