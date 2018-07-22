module Snake.Events

open Gtk
open System
open MainWindow

type KeyPress = 
    | Pressed of Gdk.Key
    | Released

// This is just to dispose the previous event subscription instead of continually subscribing
let mutable dirtyCanvas = GameWindow.Canvas.ExposeEvent |> Observable.subscribe (fun args -> ())

// Enables readInput() to get user input instead of subscribing to events
let mutable input = Released

let onKeyPress (args:KeyPressEventArgs) = 
    match args.Event.Key with
    | Gdk.Key.p | Gdk.Key.P ->
        input <- Pressed Gdk.Key.p
    | Gdk.Key.q | Gdk.Key.Q ->
        input <- Pressed Gdk.Key.q
    | Gdk.Key.Up ->
        input <- Pressed Gdk.Key.Up
    | Gdk.Key.Down ->
        input <- Pressed Gdk.Key.Down
    | Gdk.Key.Left ->
        input <- Pressed Gdk.Key.Left
    | Gdk.Key.Right -> 
        input <- Pressed Gdk.Key.Right
    | _ -> ()

do GameWindow.EventBox.KeyPressEvent |> Observable.subscribe onKeyPress |> ignore

let readInput() = 
    match input with
    | Pressed key ->
        do printfn "%A" key
        input <- Released
        Pressed(key)
    | Released ->
        Released

let ProcessInput keyPress state =
    let saveHighScore (board : Game.Board) =
        do IO.File.WriteAllLines("snake.txt", [ (Game.newHighScore board.score).ToString() ] |> List.toSeq)

    match keyPress with
    | Pressed key ->
        match key with
        | Gdk.Key.p | Gdk.Key.P ->
            Game.switch state
        | Gdk.Key.q | Gdk.Key.Q ->
            match state with
            | Game.Running data ->
                let (Game.RunningData board) = data
                saveHighScore board
            | Game.Paused data ->
                let (Game.PausedData board) = data
                saveHighScore board
            | Game.Quit ->
                ()

            Game.Quit 
        | Gdk.Key.Up ->
            state |> Game.changeDir Game.Up
        | Gdk.Key.Down ->
            state |> Game.changeDir Game.Down
        | Gdk.Key.Left ->
            state |> Game.changeDir Game.Left
        | Gdk.Key.Right -> 
            state |> Game.changeDir Game.Right
        | _ -> state
    | Released ->
        state

let onDraw (state:Game.Board) (args:Gtk.ExposeEventArgs) = 
    use context = Gdk.CairoHelper.Create(args.Event.Window)
    do context.SetSourceColor(Cairo.Color(1.0, 0.0, 0.0))
    do context.Save()
    do context.MoveTo(600.0,750.0)
    do context.SetFontSize(20.0)
    do context.ShowText("Score: " + state.score.present.ToString() + " High: " + state.score.high.ToString())
    do context.Restore()

    do context.Rectangle(0.0, 0.0, float(MainWindow.wWidth), float(MainWindow.wHeight))
    do context.LineWidth <- 5.0

    do context.Stroke()

    let unwrap (Game.Body(x,y)) = (x,y)
    let (forward, reverse) = unwrap state.player.body

    do forward |> List.iter (fun pos -> context.Rectangle(float(pos.x), float(pos.y), float(Game.plSize), float(Game.plSize)))
    do reverse |> List.iter (fun pos -> context.Rectangle(float(pos.x), float(pos.y), float(Game.plSize), float(Game.plSize)))
    do context.Rectangle(float(state.prize.x), float(state.prize.y), float(Game.plSize), float(Game.plSize))

    do context.Fill()
    do context.GetTarget().Dispose()
    do context.GetSource().Dispose()

let draw state =
    let drawBoard board =
        let onDraw' = onDraw board
        do dirtyCanvas <- GameWindow.Canvas.ExposeEvent |> Observable.subscribe onDraw'
        do GameWindow.Canvas.QueueDraw() |> ignore

    do dirtyCanvas.Dispose()
    match state with 
    | Game.Running data ->
        let (Game.RunningData board) = data
        drawBoard board
    | Game.Paused data ->
        let (Game.PausedData board) = data
        drawBoard board
    | Game.Quit ->
        // Do nothing.
        ()

