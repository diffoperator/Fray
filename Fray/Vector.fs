module Vector

open System

type Vector = {i : float; j : float; k : float}

let scalarMult x (v1: Vector) =
    {i = x*v1.i; j = x*v1.j; k = x*v1.k}

let addVector (v1: Vector) (v2: Vector) =
    {i = v1.i + v2.i; j = v1.j + v2.j; k = v1.k + v2.k}

let subVector (v1: Vector) (v2: Vector) =
    addVector v1 (scalarMult (-1.0) v2)

let dot (v1: Vector) (v2: Vector) =
    v1.i * v2.i + v1.j * v2.j + v1.k * v2.k

let mag v1 =
    Math.Sqrt (dot v1 v1)

let cross (v1: Vector) (v2: Vector) =
    { i = v1.j * v2.k - v1.k * v2.j;
      j = v1.k * v2.i - v1.i * v2.k;
      k = v1.i * v2.j - v1.j * v2.i; }

let norm (v1: Vector) = 
    let magnitude = mag v1
    if magnitude = 0.0 then 
        (scalarMult 99999.0 v1)
    else
        (scalarMult (1.0/magnitude) v1)

let isEqual v1 v2 =
    v1.i = v2.i && v1.j = v2.j && v1.k = v2.k