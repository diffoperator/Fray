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
    let Color = {r = 0.0; g = 0.0; b = 0.0}
    {Diffuse = diffuse; Specular = specular; Reflect = reflect; Roughness = roughness; RefractiveIndex = 0.0; DefaultColor = Color}
    
let shiny =
    let diffuse pos = {r = 1.0; g = 1.0; b = 1.0}
    let specular pos = {r = 0.5; g = 0.5; b = 0.5}
    let reflect pos = 0.7
    let roughness = 50.0
    let Color = {r = 0.0; g = 0.0; b = 0.0}
    {Diffuse = diffuse; Specular = specular; Reflect = reflect; Roughness = roughness; RefractiveIndex = 0.0; DefaultColor = Color}

let transparent =
    let diffuse pos = {r = 1.0; g = 1.0; b = 1.0}
    let specular pos = {r = 0.5; g = 0.5; b = 0.5}
    let reflect pos = 0.3
    let roughness = 45.0
    let refractiveIndex = 1.0
    let Color = {r = 0.0; g = 0.0; b = 0.0}
    {Diffuse = diffuse; Specular = specular; Reflect = reflect; Roughness = roughness; RefractiveIndex = refractiveIndex; DefaultColor = Color}

let shinyseethroughred = 
    let diffuse pos = {r = 1.0; g = 1.0; b = 1.0}
    let specular pos = {r = 0.5; g = 0.5; b = 0.5}
    let reflect pos = 0.2
    let roughness = 45.0
    let refractiveIndex = 0.95
    let Color = {r = 1.0; g = 0.0; b = 0.0}
    {Diffuse = diffuse; Specular = specular; Reflect = reflect; Roughness = roughness; RefractiveIndex = refractiveIndex; DefaultColor = Color}
