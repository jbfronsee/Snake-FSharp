module Snake.Events

open Gtk
open MainWindow

let mutable keyIn = Gdk.Key.Right
let mutable dirtyCanvas = Window.Canvas.ExposeEvent |> Observable.subscribe (fun args -> ())

let OnKeyPress (args:KeyPressEventArgs) = 
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

let ReadInput() = keyIn

let Initialize() =
    do Window.EventBox.KeyPressEvent |> Observable.subscribe OnKeyPress |> ignore

let OnDraw (state:State.Board) (args:Gtk.ExposeEventArgs) = 
    use canvas = Gdk.CairoHelper.Create(args.Event.Window)
    do canvas.SetSourceColor(Cairo.Color(1.0, 0.0, 0.0))
    let unwrap (State.Body(x,y)) = (x,y)
    let (forward, reverse) = unwrap state.player.body
    do forward |> List.iter (fun pos -> canvas.Rectangle(pos.x, pos.y, 10.0, 10.0))
    do reverse |> List.iter (fun pos -> canvas.Rectangle(pos.x, pos.y, 10.0, 10.0))
    do canvas.Rectangle(state.prize.x, state.prize.y, 10.0, 10.0)
    do canvas.Fill()
    do canvas.GetTarget().Dispose()

let Draw (state:State.Board) =
    do dirtyCanvas.Dispose()
    let OnDraw' = OnDraw state
    do dirtyCanvas <- Window.Canvas.ExposeEvent |> Observable.subscribe OnDraw'
    do Window.Canvas.QueueDraw() |> ignore


