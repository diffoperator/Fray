// Learn more about F# at http://fsharp.net

open System
open System.Drawing
open System.Windows.Forms

open Raytracer

// Size of window and canvas
let xSize = 600
let ySize = 600

let mainForm = new Form(Width = xSize, Height = ySize, Text = "Fray")
let box =
  new PictureBox (BackColor = Color.White, Dock = DockStyle.Fill,
   SizeMode = PictureBoxSizeMode.CenterImage)
mainForm.Controls.Add(box)

[<STAThread>]
do
    let tracer = new RayTracer(xSize, ySize)
    box.Image <- tracer.Render
    Application.Run (mainForm)


