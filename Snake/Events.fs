module Snake.Events

open Gtk
open MainWindow

// This is just to dispose the previous event subscription instead of continually subscribing
let mutable dirtyCanvas = GameWindow.Canvas.ExposeEvent |> Observable.subscribe (fun args -> ())

// Enables readInput() to get user input instead of subscribing to events
let mutable keyIn = Gdk.Key.Right

let onKeyPress (args:KeyPressEventArgs) = 
    printfn "%A" keyIn
    match args.Event.Key with
    | Gdk.Key.p | Gdk.Key.P ->
        keyIn <- Gdk.Key.p
    | Gdk.Key.Up ->
        keyIn <- Gdk.Key.Up
    | Gdk.Key.Down ->
        keyIn <- Gdk.Key.Down
    | Gdk.Key.Left ->
        keyIn <- Gdk.Key.Left
    | Gdk.Key.Right -> 
        keyIn <- Gdk.Key.Right
    | _ -> ()

GameWindow.EventBox.KeyPressEvent |> Observable.subscribe onKeyPress |> ignore

let readInput() = keyIn

let onDraw (state:State.Board) (args:Gtk.ExposeEventArgs) = 
    use context = Gdk.CairoHelper.Create(args.Event.Window)
    do context.SetSourceColor(Cairo.Color(1.0, 0.0, 0.0))
    do context.Save()
    do context.MoveTo(700.0,700.0)
    do context.SetFontSize(20.0)
    do context.ShowText("Score: " + state.score.ToString())
    do context.Restore()

    let unwrap (State.Body(x,y)) = (x,y)
    let (forward, reverse) = unwrap state.player.body

    do forward |> List.iter (fun pos -> context.Rectangle(float(pos.x), float(pos.y), float(State.plSize), float(State.plSize)))
    do reverse |> List.iter (fun pos -> context.Rectangle(float(pos.x), float(pos.y), float(State.plSize), float(State.plSize)))
    do context.Rectangle(float(state.prize.x), float(state.prize.y), float(State.plSize), float(State.plSize))

    do context.Fill()
    // TODO: Anything else needs to be disposed?
    do context.GetTarget().Dispose()

let draw (state:State.Board) =
    do dirtyCanvas.Dispose()
    let onDraw' = onDraw state
    do dirtyCanvas <- GameWindow.Canvas.ExposeEvent |> Observable.subscribe onDraw'
    do GameWindow.Canvas.QueueDraw() |> ignore


