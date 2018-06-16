module Snake.MainWindow 

open System
open Gtk;

type SnakeWindow() as this =
    inherit Window("Snake")

    let myEventBox = new EventBox()
    let myCanvas = new DrawingArea()
    do myEventBox.CanFocus <- true
    do myEventBox.Add(myCanvas)
    do this.Add(myEventBox)

    do this.SetDefaultSize(800,800)
    // TODO: Possibly remove this
    do this.Modal <- true
    do this.ShowAll()

    do this.DeleteEvent.Add(fun args ->
            Application.Quit()
            Environment.Exit 0
            args.RetVal <- false
        )

    member this.Canvas = myCanvas
    member this.EventBox = myEventBox

let GameWindow = new SnakeWindow()

