namespace Snake

    module MainWindow =

        open System
        open Gtk;

        let wWidth = 800
        let wHeight = 800

        type SnakeWindow() as this =
            inherit Window("Snake")

            let myEventBox = new EventBox()
            let myCanvas = new DrawingArea()
            do myEventBox.CanFocus <- true
            do myEventBox.Add(myCanvas)
            do this.Add(myEventBox)

            do this.SetDefaultSize(wWidth,wHeight)
            do this.ShowAll()

            do this.DeleteEvent.Add(fun args ->
                    Application.Quit()
                    Environment.Exit 0
                    args.RetVal <- false
                )
                
            member this.Canvas = myCanvas
            member this.EventBox = myEventBox

        let GameWindow = new SnakeWindow()

