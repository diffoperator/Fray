module Color

open System

type Color = {r : float; g : float; b : float}

let scalarMult x (c1: Color) =
    {r = x*c1.r; g = x*c1.g; b = x*c1.b}

let addColor (c1: Color) (c2: Color) =
    {r = c1.r + c2.r; g = c1.g + c2.g; b = c1.b + c2.b}

let subColor (c1: Color) (c2: Color) =
    addColor c1 (scalarMult (-1.0) c2)

let multColor c1 c2 =
    {r = c1.r * c2.r; g = c1.g * c2.g; b = c1.b * c2.b}

let toColor c1 =
    let r = Math.Min (255, int (c1.r * 255.0))
    let g = Math.Min (255, int (c1.g * 255.0))
    let b = Math.Min (255, int (c1.b * 255.0))
    System.Drawing.Color.FromArgb(r, g, b)